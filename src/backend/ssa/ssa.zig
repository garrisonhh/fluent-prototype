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
const TypeId = canon.TypeId;
const Env = @import("../env.zig");
const TExpr = @import("../texpr.zig");

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
        params: []Local,
        to: Local,
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

    pub const Impure = struct {
        params: []Local,
    };

    // unique
    ldc: LoadConst,
    cast: Pure,
    ret: Impure,
    call: Call,

    // control flow
    br: Branch,
    jmp: Jump,
    phi: Phi,

    // memory
    alloca: Alloca, // allocates a number of bytes and returns pointer
    store: Impure, // store a at addr b
    store_elem: Impure, // store a at addr b with offset c
    load: Pure, // loads data from ptr a

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
        to: Local,
        func: Local,
        params: []const Local,
    ) Allocator.Error!Self {
        return Self{ .call = .{
            .to = to,
            .func = func,
            .params = try ally.dupe(Local, params),
        } };
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

    pub fn initImpure(
        ally: Allocator,
        comptime tag: std.meta.Tag(Self),
        params: []const Local,
    ) Allocator.Error!Self {
        return @unionInit(
            Self,
            @tagName(tag),
            Impure{ .params = try ally.dupe(Local, params) },
        );
    }

    pub fn deinit(self: *Self, ally: Allocator) void {
        switch (self.classify()) {
            .call => |call| ally.free(call.params),
            .pure => |pure| ally.free(pure.params),
            .impure => |impure| ally.free(impure.params),
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
        impure: Impure,

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
    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        switch (self.classify()) {
            inline else => |data| {
                const T = @TypeOf(data);

                if (@hasField(T, "to")) {
                    try writer.print("%{} = ", .{data.to.index});
                }

                try writer.writeAll(@tagName(self));

                inline for (@typeInfo(T).Struct.fields) |field| {
                    if (comptime std.mem.eql(u8, field.name, "to")) {
                        continue;
                    }

                    const x = @field(data, field.name);
                    switch (field.field_type) {
                        Const => try writer.print(" c{}", .{x.index}),
                        Local => try writer.print(" %{}", .{x.index}),
                        []Local => for (x) |el| {
                            try writer.print(" %{}", .{el.index});
                        },
                        Label => try writer.print(" @{}", .{x.index}),
                        []Label => for (x) |el| {
                            try writer.print(" @{}", .{el.index});
                        },
                        usize => try writer.print(" {}", .{x}),
                        else => |F| comptime {
                            const msg = std.fmt.comptimePrint(
                                "TODO print ssa op field type `{}`",
                                .{F},
                            );
                            @compileError(msg);
                        },
                    }
                }
            },
        }
    }
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

    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.print("%{}", .{self.index});
    }
};

/// a handle for a block
pub const Label = packed struct {
    const Self = @This();

    index: usize,

    pub fn of(index: usize) Self {
        return Self{ .index = index };
    }

    pub fn render(self: Self, ctx: *kz.Context) !kz.Ref {
        return try ctx.print(.{ .fg = .cyan }, "@{}", .{self.index});
    }
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

    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.print(
            "{}@{}:{}",
            .{ self.ref.index, self.block.index, self.index },
        );
    }
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

    pub fn init(ally: Allocator, func: *const Func) Allocator.Error!Self {
        // init map
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
            for (map) |lt, i| {
                if (lt.start.order(lt.stop) == .gt) {
                    std.debug.panic(
                        "bad lifetime for %{}: {} - {}",
                        .{ i, lt.start, lt.stop },
                    );
                }
            }
        }

        return Self{
            .ref = func.ref,
            .map = map,
        };
    }

    pub fn deinit(self: *Self, ally: Allocator) void {
        ally.free(self.map);
    }

    pub fn get(self: Self, local: Local) Lifetime {
        return self.map[local.index];
    }

    pub fn render(self: Self, ctx: *kz.Context, env: Env) !kz.Ref {
        const ally = ctx.ally;
        const func = env.getFuncConst(self.ref);

        // map positions to y values
        var map = std.AutoHashMap(Pos, usize).init(ally);
        defer map.deinit();

        var y: usize = 0;
        for (func.blocks.items) |block, i| {
            const label = Label.of(i);
            y += 1;
            for (block.ops.items) |_, j| {
                try map.put(Pos.of(func.ref, label, j), y);
                y += 1;
            }
        }

        // generate lifetime graph
        const bars = try ally.alloc(kz.Ref, self.map.len);
        defer ally.free(bars);

        for (self.map) |lt, i| {
            const index = map.get(lt.start).?;
            const len = map.get(lt.stop).? + 1 - index;

            // generate bar
            const stub = try ctx.block(.{ 1, y }, .{ .fg = .dark_gray }, '.');
            const color: kz.Color = if (i % 5 == 0) .blue else .green;
            const bar = try ctx.block(.{ 1, len }, .{ .bg = color }, ' ');

            bars[i] = try ctx.unify(stub, bar, .{ 0, @intCast(isize, index) });
        }

        return try ctx.stack(bars, .right, .{});
    }
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
        return env.getFuncConst(self).consts.items[c.index];
    }

    pub fn getLocal(self: Self, env: Env, l: Local) TypeId {
        return env.getFuncConst(self).locals.items[l.index];
    }

    /// clones expr and stores in func
    pub fn addConst(self: Self, env: *Env, expr: TExpr) Allocator.Error!Const {
        std.debug.assert(expr.known_const);

        const func = env.getFunc(self);
        const @"const" = Const.of(func.consts.items.len);
        try func.consts.append(env.ally, try expr.clone(env.ally));

        return @"const";
    }

    pub fn addLocal(self: Self, env: *Env, ty: TypeId) Allocator.Error!Local {
        const func = env.getFunc(self);
        const local = Local.of(func.locals.items.len);
        try func.locals.append(env.ally, ty);

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

    name: Name, // owned by env
    takes: usize,
    // TODO this is undefined until lowering is completed. TERRIBLE idea.
    returns: TypeId,
    ref: FuncRef,

    consts: std.ArrayListUnmanaged(TExpr) = .{},
    locals: std.ArrayListUnmanaged(TypeId) = .{},
    blocks: std.ArrayListUnmanaged(Block) = .{},

    fn deinit(self: *Self, ally: Allocator) void {
        for (self.consts.items) |*value| value.deinit(ally);
        self.consts.deinit(ally);
        self.locals.deinit(ally);
        for (self.blocks.items) |*block| block.deinit(ally);
        self.blocks.deinit(ally);
    }

    fn renderLocal(
        self: Self,
        ctx: *kz.Context,
        env: Env,
        local: Local,
    ) !kz.Ref {
        const ty = self.locals.items[local.index];
        return try ctx.slap(
            try ty.render(ctx, env.tw),
            try ctx.print(.{}, "%{}", .{local.index}),
            .right,
            .{ .space = 1 },
        );
    }

    fn renderParams(
        self: Self,
        ctx: *kz.Context,
        env: Env,
        list: *std.ArrayList(kz.Ref),
        params: []const Local,
    ) !void {
        const comma = try ctx.print(.{}, ", ", .{});
        defer ctx.drop(comma);

        for (params) |param, i| {
            if (i > 0) try list.append(try ctx.clone(comma));
            try list.append(try self.renderLocal(ctx, env, param));
        }
    }

    fn renderLabels(
        ctx: *kz.Context,
        list: *std.ArrayList(kz.Ref),
        labels: []const Label,
    ) !void {
        const comma = try ctx.print(.{}, ", ", .{});
        defer ctx.drop(comma);

        for (labels) |label, i| {
            if (i > 0) try list.append(try ctx.clone(comma));
            try list.append(try label.render(ctx));
        }
    }

    fn renderOp(self: Self, ctx: *kz.Context, env: Env, op: Op) !kz.Ref {
        var line = std.ArrayList(kz.Ref).init(ctx.ally);
        defer line.deinit();

        // collect textures
        const tag = @tagName(op);
        const comma = try ctx.print(.{}, ", ", .{});
        const class = op.classify();
        switch (class) {
            .ldc => |ldc| {
                const expr = self.consts.items[ldc.a.index];
                try line.appendSlice(&.{
                    try self.renderLocal(ctx, env, ldc.to),
                    try ctx.print(.{}, " = ", .{}),
                    try expr.render(ctx, env),
                });
            },
            .call => |call| {
                try line.appendSlice(&.{
                    try self.renderLocal(ctx, env, call.to),
                    try ctx.print(.{}, " = {s} ", .{tag}),
                    try self.renderLocal(ctx, env, call.func),
                    try ctx.clone(comma),
                });

                try self.renderParams(ctx, env, &line, call.params);
            },
            .phi => |phi| {
                try line.appendSlice(&.{
                    try self.renderLocal(ctx, env, phi.to),
                    try ctx.print(.{}, " = {s} ", .{tag}),
                });

                try renderLabels(ctx, &line, phi.from);
            },
            .branch => |br| {
                try line.appendSlice(&.{
                    try ctx.print(.{}, "{s} ", .{tag}),
                    try self.renderLocal(ctx, env, br.cond),
                    try ctx.clone(comma),
                    try br.a.render(ctx),
                    try ctx.clone(comma),
                    try br.b.render(ctx),
                });
            },
            .jump => |jmp| {
                try line.appendSlice(&.{
                    try ctx.print(.{}, "{s} ", .{tag}),
                    try jmp.dst.render(ctx),
                    try ctx.print(.{}, " ", .{}),
                    try self.renderLocal(ctx, env, jmp.data),
                });
            },
            .alloca => |all| {
                try line.appendSlice(&.{
                    try self.renderLocal(ctx, env, all.to),
                    try ctx.print(.{}, " = {s} {d}", .{ tag, all.size }),
                });
            },
            .pure => |pure| {
                try line.appendSlice(&.{
                    try self.renderLocal(ctx, env, pure.to),
                    try ctx.print(.{}, " = {s} ", .{tag}),
                });

                try self.renderParams(ctx, env, &line, pure.params);
            },
            .impure => |impure| {
                try line.append(try ctx.print(.{}, "{s} ", .{tag}));
                try self.renderParams(ctx, env, &line, impure.params);
            },
        }

        // stack and return
        return ctx.stack(line.items, .right, .{});
    }

    fn renderBlock(
        self: Self,
        ctx: *kz.Context,
        env: Env,
        label: Label,
    ) !kz.Ref {
        const INDENT = 4;

        // collect op refs
        var op_texs = std.ArrayList(kz.Ref).init(ctx.ally);
        defer op_texs.deinit();

        for (self.blocks.items[label.index].ops.items) |op| {
            const tex = try self.renderOp(ctx, env, op);
            try op_texs.append(tex);
        }

        // format block nicely
        const ops_tex = try ctx.stack(op_texs.items, .bottom, .{});
        const indent_tex = try ctx.blank(.{ INDENT, 0 });
        const body_tex = try ctx.slap(indent_tex, ops_tex, .right, .{});

        const label_tex = try label.render(ctx);
        return try ctx.slap(body_tex, label_tex, .top, .{});
    }

    fn render(self: Self, ctx: *kz.Context, env: Env) !kz.Ref {
        const red = kz.Style{ .fg = .red };

        // name
        var header_texs = std.ArrayList(kz.Ref).init(ctx.ally);
        defer header_texs.deinit();

        if (self.name.syms.len > 0) {
            try header_texs.append(try ctx.print(red, "{}", .{self.name}));
        } else {
            try header_texs.append(try ctx.print(red, "<expr>", .{}));
        }

        // parameters
        try header_texs.append(try ctx.print(.{}, " :: ", .{}));

        var param = Local.of(0);
        while (param.index < self.takes) : (param.index += 1) {
            if (param.index > 0) {
                try header_texs.append(try ctx.print(.{}, ", ", .{}));
            }

            try header_texs.append(try self.renderLocal(ctx, env, param));
        }

        if (self.takes > 0) {
            try header_texs.append(try ctx.print(.{}, " -> ", .{}));
        }

        // return type
        try header_texs.append(try self.returns.render(ctx, env.tw));

        const header = try ctx.stack(header_texs.items, .right, .{});

        // body
        var block_texs = std.ArrayList(kz.Ref).init(ctx.ally);
        defer block_texs.deinit();

        for (self.blocks.items) |_, i| {
            const tex = try self.renderBlock(ctx, env, Label.of(i));
            try block_texs.append(tex);
        }

        const body = try ctx.stack(block_texs.items, .bottom, .{});

        // lifetime graph
        var proph = try Prophecy.init(ctx.ally, &self);
        defer proph.deinit(ctx.ally);

        const proph_tex = try proph.render(ctx, env);

        // slap it together
        const code = try ctx.slap(header, body, .bottom, .{});
        const sz = kz.toOffset(ctx.getSize(code));
        return try ctx.unify(code, proph_tex, .{ sz[0] + 1, 1 });
    }
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
        name: Name,
        params: []const TypeId,
    ) Allocator.Error!FuncRef {
        const ref = try self.nextRef(ally);
        var func = Func{
            .name = name,
            .takes = params.len,
            .returns = undefined,
            .ref = ref,
        };

        try func.locals.appendSlice(ally, params);
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

    pub fn render(self: Self, ctx: *kz.Context, env: Env) !kz.Ref {
        const ally = ctx.ally;

        // construct set of unused refs
        var unused = std.AutoHashMap(FuncRef, void).init(ctx.ally);
        defer unused.deinit();

        for (self.unused.items) |ref| {
            try unused.put(ref, {});
        }

        // render each ref if it's not unused
        var refs = std.ArrayList(kz.Ref).init(ally);
        defer refs.deinit();

        for (self.funcs.items) |func, i| {
            if (!unused.contains(FuncRef.of(i))) {
                try refs.append(try func.render(ctx, env));
            }
        }

        return ctx.stack(refs.items, .bottom, .{ .space = 1 });
    }
};
