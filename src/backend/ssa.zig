//! SSA IR primitives.
//!
//! the goal for this is to be a common target between static compilation and
//! dynamic execution. the dynamic vm will want extra processing to do things
//! like stack allocation and optimizing byte loads for the cache, but for
//! compiling to qbe or llvm or whatever backend the goal is for this to be
//! sufficient

const std = @import("std");
const Allocator = std.mem.Allocator;
const kz = @import("kritzler");
const util = @import("util");
const Symbol = util.Symbol;
const Name = util.Name;
const builtin = @import("builtin");
const types = @import("types.zig");
const TypeId = types.TypeId;
const Env = @import("env.zig");
const Value = @import("value.zig");
const TExpr = @import("texpr.zig");
const canon = @import("canon.zig");

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

    // conditional
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
        params: []const Local
    ) Allocator.Error!Self {
        return Self{
            .call = .{
                .to = to,
                .func = func,
                .params = try ally.dupe(Local, params),
            }
        };
    }

    pub fn initPure(
        ally: Allocator,
        comptime tag: std.meta.Tag(Self),
        to: Local,
        params: []const Local
    ) Allocator.Error!Self {
        return @unionInit(Self, @tagName(tag), Pure{
            .to = to,
            .params = try ally.dupe(Local, params)
        });
    }

    pub fn initImpure(
        ally: Allocator,
        comptime tag: std.meta.Tag(Self),
        params: []const Local
    ) Allocator.Error!Self {
        return @unionInit(Self, @tagName(tag), Impure{
            .params = try ally.dupe(Local, params)
        });
    }

    pub fn deinit(self: *Self, ally: Allocator) void {
        switch (self.classify()) {
            .call => |call| ally.free(call.params),
            .pure => |pure| ally.free(pure.params),
            .impure => |impure| ally.free(impure.params),
            else => {}
        }
    }

    pub const Class = union(enum) {
        ldc: LoadConst,
        call: Call,
        branch: Branch,
        jump: Jump,
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
            }
        };
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
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        _ = fmt;
        _ = options;

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

pub const FuncRef = packed struct {
    const Self = @This();

    index: usize,

    pub fn of(index: usize) Self {
        return Self{ .index = index };
    }

    pub fn eql(self: Self, other: Self) bool {
        return self.index == other.index;
    }

    pub fn getConst(self: Self, env: Env, c: Const) Value {
        return env.getFuncConst(self).consts.items[c.index];
    }

    pub fn getLocal(self: Self, env: Env, l: Local) TypeId {
        return env.getFuncConst(self).locals.items[l.index];
    }

    /// clones expr and stores in func
    pub fn addConst(
        self: Self,
        env: *Env,
        expr: TExpr
    ) Allocator.Error!Const {
        std.debug.assert(expr.known_const);

        const func = env.getFunc(self);
        const @"const" = Const.of(func.consts.items.len);
        try func.consts.append(env.ally, try expr.clone(env.ally));

        return @"const";
    }

    pub fn addLocal(
        self: Self,
        env: *Env,
        ty: TypeId
    ) Allocator.Error!Local {
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
        op: Op
    ) Allocator.Error!void {
        const func = env.getFunc(self);
        const block = &func.blocks.items[label.index];
        try block.ops.append(env.ally, op);
    }
};

pub const Func = struct {
    const Self = @This();

    name: Name, // unowned
    takes: usize,
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
        local: Local
    ) !kz.Ref {
        const ty = self.locals.items[local.index];
        const ty_text = try ty.writeAlloc(ctx.ally, env.tw);
        defer ctx.ally.free(ty_text);

        const ty_tex = try ctx.print(.{ .fg = .green }, "{s}", .{ty_text});
        const var_tex = try ctx.print(.{}, "%{}", .{local.index});

        return try ctx.slap(ty_tex, var_tex, .right, .{ .space = 1 });
    }

    fn renderParams(
        self: Self,
        ctx: *kz.Context,
        env: Env,
        list: *std.ArrayList(kz.Ref),
        params: []const Local
    ) !void {
        const comma = try ctx.print(.{}, ", ", .{});
        defer ctx.drop(comma);

        for (params) |param, i| {
            if (i > 0) try list.append(try ctx.clone(comma));
            try list.append(try self.renderLocal(ctx, env, param));
        }
    }

    fn renderOp(
        self: Self,
        ctx: *kz.Context,
        env: Env,
        op: Op
    ) !kz.Ref {
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
                    try expr.render(ctx, env.tw),
                });
            },
            .call => |call| {
                try line.appendSlice(&.{
                    try self.renderLocal(ctx, env, call.to),
                    try ctx.print(.{}, " = {s} ", .{tag}),
                    try self.renderLocal(ctx, env, call.func),
                    try ctx.print(.{}, " ", .{}),
                });

                try self.renderParams(ctx, env, &line, call.params);
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
                });
            },
            .alloca => |all| {
                try line.appendSlice(&.{
                    try self.renderLocal(ctx, env, all.to),
                    try ctx.print(.{}, " = {s} {d}", .{tag, all.size}),
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
            // else => std.debug.panic(
                // "TODO render op class {s}",
                // .{@tagName(class)}
            // )
        }

        // stack and return
        return ctx.stack(line.items, .right, .{});
    }

    fn renderBlock(
        self: Self,
        ctx: *kz.Context,
        env: Env,
        label: Label
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
        const indent_tex = try ctx.blank(.{INDENT, 0});
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
        const returns = try self.returns.writeAlloc(ctx.ally, env.tw);
        defer ctx.ally.free(returns);

        try header_texs.appendSlice(&.{
            try ctx.print(.{ .fg = .green }, "{s}", .{returns}),
        });

        const header = try ctx.stack(header_texs.items, .right, .{});

        // body
        var block_texs = std.ArrayList(kz.Ref).init(ctx.ally);
        defer block_texs.deinit();

        var i: usize = 0;
        while (i < self.blocks.items.len) : (i += 1) {
            const tex = try self.renderBlock(ctx, env, Label.of(i));
            try block_texs.append(tex);
        }

        const body = try ctx.stack(block_texs.items, .bottom, .{});
        return ctx.slap(header, body, .bottom, .{});
    }
};

pub const Program = struct {
    const Self = @This();

    // TODO go merge refmap and use it here so I don't reimplement this pattern
    // a million times
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
        return &self.funcs.items[ref.index];
    }

    pub fn remove(self: *Self, ally: Allocator, ref: FuncRef) void {
        var func = self.funcs.swapRemove(ref.index);
        func.deinit(ally);
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
        const refs = try ally.alloc(kz.Ref, self.funcs.items.len);
        defer ally.free(refs);

        for (self.funcs.items) |func, i| {
            if (!unused.contains(FuncRef.of(i))) {
                refs[i] = try func.render(ctx, env);
            }
        }

        return ctx.stack(refs, .bottom, .{ .space = 1 });
    }
};
