//! SSA IR primitives.
//!
//! the goal for this is to be a com target between static compilation and
//! dynamic execution. the dynamic vm will want extra processing to do things
//! like stack allocation and optimizing byte loads for the cache, but for
//! compiling to qbe or llvm or whatever backend the goal is for this to be
//! sufficient

const std = @import("std");
const Allocator = std.mem.Allocator;
const kz = @import("kritzler");
const com = @import("common");
const Name = com.Name;
const builtin = @import("builtin");
const canon = @import("../canon.zig");
const TypeWelt = canon.TypeWelt;
const TypeId = canon.TypeId;
const ReprWelt = canon.ReprWelt;
const ReprId = canon.ReprId;
const Repr = canon.Repr;
const Env = @import("../env.zig");
const TExpr = @import("../texpr.zig");
const rendering = @import("render_ssa.zig");

/// symbolic representation of operations. since blocks store type info,
/// there is no need for operations to be type or size specific; this can be
/// deduced later on
pub const Op = union(enum) {
    const Self = @This();

    pub const LoadConst = struct {
        a: Const,
        to: Local,
    };

    pub const Call = struct {
        func: Local,
        ctx: Local,
        params: []Local,
        ret: Local,
    };

    pub const Branch = struct {
        cond: Local,
        a: Label,
        b: Label,
    };

    pub const Jump = struct {
        dst: Label,
        data: Local,
    };

    pub const Phi = struct {
        from: []Label,
        to: Local,
    };

    pub const Alloca = struct {
        size: usize,
        to: Local,
    };

    pub const Pure = struct {
        params: []Local,
        to: Local,
    };

    pub const Effect = struct {
        params: []Local,
    };

    /// get pointer to a repr field
    pub const GetFieldPtr = struct {
        index: usize,
        obj: Local,
        to: Local,
    };

    // unique
    ldc: LoadConst,
    copy: Pure,
    ret: Effect,
    vcall: Call, // call & return by value
    rcall: Call, // call & return thru reference

    // control flow
    br: Branch,
    jmp: Jump,
    phi: Phi,

    // memory
    alloca: Alloca, // allocates a number of bytes and returns pointer
    gfp: GetFieldPtr,
    store: Effect, // *a = b
    load: Effect, // b = *a
    memcpy: Effect, // *a = *b

    // math
    add: Pure,
    sub: Pure,
    mul: Pure,
    div: Pure,
    mod: Pure,
    shl: Pure,
    shr: Pure,

    // conditional
    eq: Pure,
    @"or": Pure,
    @"and": Pure,
    not: Pure,

    // types
    slice_ty: Pure,
    fn_ty: Pure,

    pub fn initCall(
        ally: Allocator,
        ret_conv: Repr.Conv,
        func: Local,
        ctx: Local,
        params: []const Local,
        ret: Local,
    ) Allocator.Error!Self {
        switch (ret_conv) {
            inline else => |conv| {
                const tag: std.meta.Tag(Self) = comptime switch (conv) {
                    .by_ref => .rcall,
                    .by_value => .vcall,
                };

                return @unionInit(Self, @tagName(tag), Call{
                    .func = func,
                    .ctx = ctx,
                    .params = try ally.dupe(Local, params),
                    .ret = ret,
                });
            },
        }
    }

    pub fn initPhi(
        ally: Allocator,
        to: Local,
        from: []const Label,
    ) Allocator.Error!Self {
        return Self{ .phi = .{
            .to = to,
            .from = try ally.dupe(Label, from),
        } };
    }

    pub fn initPure(
        ally: Allocator,
        comptime tag: std.meta.Tag(Self),
        to: Local,
        params: []const Local,
    ) Allocator.Error!Self {
        return @unionInit(Self, @tagName(tag), Pure{
            .to = to,
            .params = try ally.dupe(Local, params),
        });
    }

    pub fn initEffect(
        ally: Allocator,
        comptime tag: std.meta.Tag(Self),
        params: []const Local,
    ) Allocator.Error!Self {
        return @unionInit(
            Self,
            @tagName(tag),
            Effect{ .params = try ally.dupe(Local, params) },
        );
    }

    pub fn deinit(self: *Self, ally: Allocator) void {
        switch (self.classify()) {
            inline .call, .pure, .effect => |data| ally.free(data.params),
            .phi => |phi| ally.free(phi.from),
            else => {},
        }
    }

    pub const Class = union(enum) {
        ldc: LoadConst,
        call: Call,
        branch: Branch,
        jump: Jump,
        phi: Phi,
        alloca: Alloca,
        pure: Pure,
        effect: Effect,
        gfp: GetFieldPtr,

        fn getFieldByType(comptime T: type) []const u8 {
            const fields = @typeInfo(Class).Union.fields;
            for (fields) |field| {
                if (field.field_type == T) {
                    return field.name;
                }
            }
        }
    };

    /// makes switching on ops + writing generalized code significantly easier
    pub fn classify(self: Self) Class {
        return switch (self) {
            inline else => |data| class: {
                const fieldname = comptime Class.getFieldByType(@TypeOf(data));
                break :class @unionInit(Class, fieldname, data);
            },
        };
    }

    /// mostly using this for bytecode comments
    pub const format = rendering.formatOp;
};

/// a handle for a const value
pub const Const = packed struct {
    const Self = @This();

    index: usize,

    pub fn of(index: usize) Self {
        return Self{ .index = index };
    }
};

/// a handle for a variable
pub const Local = packed struct {
    const Self = @This();

    index: usize,

    pub fn of(index: usize) Self {
        return Self{ .index = index };
    }

    pub const render = rendering.renderLocal;
};

/// a handle for a block
pub const Label = packed struct {
    const Self = @This();

    index: usize,

    pub fn of(index: usize) Self {
        return Self{ .index = index };
    }

    pub const render = rendering.renderLabel;
};

pub const Block = struct {
    const Self = @This();

    ops: std.ArrayListUnmanaged(Op) = .{},

    pub fn deinit(self: *Self, ally: Allocator) void {
        for (self.ops.items) |*op| op.deinit(ally);
        self.ops.deinit(ally);
    }
};

/// a hashable, unique location in an ssa program.
pub const Pos = struct {
    const Self = @This();

    // ensure autohashability
    comptime {
        std.debug.assert(std.meta.trait.hasUniqueRepresentation(Self));
    }

    ref: FuncRef,
    block: Label,
    index: usize,

    pub fn of(ref: FuncRef, block: Label, index: usize) Self {
        return Pos{
            .ref = ref,
            .block = block,
            .index = index,
        };
    }

    pub fn ofEntry(ref: FuncRef) Self {
        return Pos{
            .ref = ref,
            .block = Label.of(0),
            .index = 0,
        };
    }

    /// attempts to iterate this pos to the next ssa op in the func; returns
    /// null if this is the end of the function
    pub fn next(self: Self, func: *const Func) ?Self {
        // iterate index
        const op_count = func.blocks.items[self.block.index].ops.items.len;
        if (self.index + 1 < op_count) {
            return Self.of(self.ref, self.block, self.index + 1);
        }

        // iterate block
        if (self.block.index + 1 < func.blocks.items.len) {
            return Self.of(self.ref, Label.of(self.block.index + 1), 0);
        }

        // can't iterate
        return null;
    }

    pub fn order(self: Self, other: Self) std.math.Order {
        const xs = [_]usize{ self.ref.index, self.block.index, self.index };
        const ys = [_]usize{ other.ref.index, other.block.index, other.index };
        return std.mem.order(usize, &xs, &ys);
    }

    pub fn eql(self: Self, other: Self) bool {
        return self.order(other) == .eq;
    }

    pub const format = rendering.formatPos;
};

pub const Lifetime = struct {
    const Self = @This();

    start: Pos,
    stop: Pos,

    fn of(start: Pos, stop: Pos) Self {
        std.debug.assert(start.order(stop).compare(.lte));
        return Self{
            .start = start,
            .stop = stop,
        };
    }
};

/// map of the lifetime of each local
pub const Prophecy = struct {
    const Self = @This();

    ref: FuncRef,
    map: []Lifetime,

    const OpMeta = struct {
        to: ?Local,
        uses: []const Local,
    };

    /// creates OpMeta using buffer memory
    fn getMeta(buf: []Local, op: Op) OpMeta {
        // find `to` and `using`
        var to: ?Local = null;
        var using = std.BoundedArray(Local, 256){};
        switch (op.classify()) {
            inline else => |data| {
                const T = @TypeOf(data);
                inline for (@typeInfo(T).Struct.fields) |field| {
                    const el = @field(data, field.name);
                    if (comptime std.mem.eql(u8, field.name, "to")) {
                        to = el;
                    } else switch (field.field_type) {
                        Local => using.appendAssumeCapacity(el),
                        []Local => using.appendSliceAssumeCapacity(el),
                        else => {},
                    }
                }
            },
        }

        // copy using to buf
        std.mem.copy(Local, buf, using.slice());

        return OpMeta{
            .to = to,
            .uses = buf[0..using.len],
        };
    }

    fn generateMap(
        ally: Allocator,
        func: *const Func,
    ) Allocator.Error![]Lifetime {
        const map = try ally.alloc(Lifetime, func.locals.items.len);

        const entry = Pos.ofEntry(func.ref);
        std.mem.set(Lifetime, map, Lifetime.of(entry, entry));

        // values created from computation
        for (func.blocks.items) |block, i| {
            const label = Label.of(i);
            for (block.ops.items) |op, j| {
                var buf: [256]Local = undefined;
                const meta = getMeta(&buf, op);
                const here = Pos.of(func.ref, label, j);

                // new ops
                if (meta.to) |to| {
                    map[to.index].start = here;
                }

                // refreshed ops
                for (meta.uses) |refresh| {
                    map[refresh.index].stop = here;
                }
            }
        }

        if (builtin.mode == .Debug) {
            // verify starts and stops
            for (map) |lt| {
                std.debug.assert(lt.start.order(lt.stop).compare(.lte));
            }
        }

        return map;
    }

    pub fn init(ally: Allocator, func: *const Func) Allocator.Error!Self {
        return Self{
            .ref = func.ref,
            .map = try generateMap(ally, func),
        };
    }

    pub fn deinit(self: *Self, ally: Allocator) void {
        ally.free(self.map);
    }

    pub fn get(self: Self, local: Local) Lifetime {
        return self.map[local.index];
    }

    pub const render = rendering.renderProphecy;
};

pub const FuncRef = packed struct {
    const Self = @This();

    index: usize,

    pub fn of(index: usize) Self {
        return Self{ .index = index };
    }

    pub fn eql(self: Self, other: Self) bool {
        return self.index == other.index;
    }

    pub fn getConst(self: Self, env: Env, c: Const) TExpr {
        return env.getFuncConst(self).getConst(c);
    }

    pub fn getLocal(self: Self, env: Env, l: Local) ReprId {
        return env.getFuncConst(self).getLocal(l);
    }

    /// clones expr and stores in func
    pub fn addConst(self: Self, env: *Env, expr: TExpr) Allocator.Error!Const {
        std.debug.assert(expr.known_const);

        const func = env.getFunc(self);
        const @"const" = Const.of(func.consts.items.len);
        try func.consts.append(env.ally, try expr.clone(env.ally));

        return @"const";
    }

    pub fn addLocal(self: Self, env: *Env, repr: ReprId) Allocator.Error!Local {
        const func = env.getFunc(self);
        const local = Local.of(func.locals.items.len);
        try func.locals.append(env.ally, repr);

        return local;
    }

    pub fn addBlock(self: Self, env: *Env) Allocator.Error!Label {
        const func = env.getFunc(self);
        const label = Label.of(func.blocks.items.len);
        try func.blocks.append(env.ally, .{});

        return label;
    }

    pub fn addOp(
        self: Self,
        env: *Env,
        label: Label,
        op: Op,
    ) Allocator.Error!void {
        const func = env.getFunc(self);
        const block = &func.blocks.items[label.index];
        try block.ops.append(env.ally, op);
    }
};

pub const Func = struct {
    const Self = @This();

    // owned by env
    name: Name,
    /// type of function
    ty: TypeId,
    /// number of parameters, or null if this is not a function
    takes: usize,
    /// how the function returns a value
    ret_conv: Repr.Conv,
    /// func id (TODO I really shouldn't have to store this here)
    ref: FuncRef,

    consts: std.ArrayListUnmanaged(TExpr) = .{},
    locals: std.ArrayListUnmanaged(ReprId) = .{},
    blocks: std.ArrayListUnmanaged(Block) = .{},

    fn deinit(self: *Self, ally: Allocator) void {
        for (self.consts.items) |*value| value.deinit(ally);
        self.consts.deinit(ally);
        self.locals.deinit(ally);
        for (self.blocks.items) |*block| block.deinit(ally);
        self.blocks.deinit(ally);
    }

    pub fn getConst(self: Self, @"const": Const) TExpr {
        return self.consts.items[@"const".index];
    }

    pub fn getLocal(self: Self, local: Local) ReprId {
        return self.locals.items[local.index];
    }

    pub const render = rendering.renderFunc;
};

pub const Program = struct {
    const Self = @This();

    unused: std.ArrayListUnmanaged(FuncRef) = .{},
    funcs: std.ArrayListUnmanaged(Func) = .{},

    pub fn deinit(self: *Self, ally: Allocator) void {
        self.unused.deinit(ally);
        self.funcs.deinit(ally);
    }

    fn nextRef(self: *Self, ally: Allocator) Allocator.Error!FuncRef {
        if (self.unused.items.len == 0) {
            const ref = FuncRef.of(self.funcs.items.len);
            _ = try self.funcs.addOne(ally);
            return ref;
        }

        return self.unused.pop();
    }

    pub fn add(
        self: *Self,
        ally: Allocator,
        env: *Env,
        name: Name,
        tid: TypeId,
    ) Repr.ConversionError!FuncRef {
        const params = env.tw.get(tid).func.takes;
        const fn_repr = env.rw.get(try env.reprOf(tid)).func;

        const ref = try self.nextRef(ally);
        var func = Func{
            .name = name,
            .ty = tid,
            .takes = params.len,
            .ret_conv = fn_repr.returns.conv,
            .ref = ref,
        };

        // implicit parameters
        if (func.ret_conv == .by_ref) {
            const ret_ptr = try env.rw.intern(env.ally, Repr{
                .ptr = fn_repr.returns.of,
            });
            try func.locals.append(ally, ret_ptr);
        }

        try func.locals.append(ally, fn_repr.ctx.of);

        // params
        for (fn_repr.takes) |param| {
            try func.locals.append(ally, param.of);
        }

        self.funcs.items[ref.index] = func;

        return ref;
    }

    /// invalidated on next `add` call
    pub fn get(self: *Self, ref: FuncRef) *Func {
        if (builtin.mode == .Debug) {
            for (self.unused.items) |unused| {
                if (unused.index == ref.index) {
                    @panic("funcref use-after-free");
                }
            }
        }

        return &self.funcs.items[ref.index];
    }

    pub fn remove(
        self: *Self,
        ally: Allocator,
        ref: FuncRef,
    ) Allocator.Error!void {
        self.get(ref).deinit(ally);
        try self.unused.append(ally, ref);
    }

    pub const render = rendering.renderProgram;
};
