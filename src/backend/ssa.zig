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
const builtin = @import("builtin");
const types = @import("types.zig");
const TypeId = types.TypeId;
const Env = @import("env.zig");
const Value = @import("value.zig");
const TExpr = @import("texpr.zig");

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
        dest: FuncRef,
    };

    pub const Arg = struct {
        arg: usize,
        from: Local,
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

    pub const Unary = struct {
        a: Local,
        to: Local,
    };

    pub const Binary = struct {
        a: Local,
        b: Local,
        to: Local,
    };

    pub const UnaryEffect = struct {
        a: Local,
    };

    pub const BinaryEffect = struct {
        a: Local,
        b: Local,
    };

    // unique
    ldc: LoadConst,
    cast: Unary,
    ret: UnaryEffect,

    // calls
    call: Call,
    arg: Arg,

    // control flow
    br: Branch,
    jmp: Jump,

    // memory
    alloca: Alloca, // allocates a number of bytes and returns pointer
    store: BinaryEffect, // stores local a at address in ptr b
    load: Unary, // loads data from ptr a

    // math
    add: Binary,
    sub: Binary,
    mul: Binary,
    div: Binary,
    mod: Binary,

    // conditional
    @"or": Binary,
    @"and": Binary,
    not: Unary,

    pub const Class = union(enum) {
        // unique logic
        ldc: LoadConst,
        call: Call,
        arg: Arg,
        branch: Branch,
        jump: Jump,
        alloca: Alloca,

        // generalizable logic
        unary: Unary,
        binary: Binary,
        unary_eff: UnaryEffect,
        binary_eff: BinaryEffect,
    };

    /// makes switching on ops + writing generalized code significantly easier
    pub fn classify(self: Self) Class {
        return switch (self) {
            .ldc => |ldc| Class{ .ldc = ldc },
            .call => |call| Class{ .call = call },
            .arg => |arg| Class{ .arg = arg },
            .br => |br| Class{ .branch = br },
            .jmp => |jmp| Class{ .jump = jmp },
            .alloca => |all| Class{ .alloca = all },
            .cast, .not, .load => |un| Class{ .unary = un },
            .add, .sub, .mul, .div, .mod, .@"or", .@"and"
                => |bin| Class{ .binary = bin },
            .ret => |un_eff| Class{ .unary_eff = un_eff },
            .store => |bin_eff| Class{ .binary_eff = bin_eff },
        };
    }
};

pub const Const = packed struct {
    const Self = @This();

    index: usize,

    pub fn of(index: usize) Self {
        return Self{ .index = index };
    }
};

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

pub const Label = packed struct {
    const Self = @This();

    index: usize,

    pub fn of(index: usize) Self {
        return Self{ .index = index };
    }
};

pub const FuncRef = packed struct {
    const Self = @This();

    index: usize,

    pub fn of(index: usize) Self {
        return Self{ .index = index };
    }
};

pub const Func = struct {
    const Self = @This();

    label: Symbol,
    takes: usize,
    returns: TypeId,
    entry: Label,

    consts: []Value,
    locals: []TypeId,
    ops: []Op,
    labels: []usize, // 'maps' label -> op index

    pub fn deinit(self: Self, ally: Allocator) void {
        ally.free(self.label.str);

        for (self.consts) |*value| value.deinit(ally);
        ally.free(self.consts);
        ally.free(self.locals);
        ally.free(self.ops);
        ally.free(self.labels);
    }

    pub const LabelMap = std.AutoHashMapUnmanaged(usize, Label);

    /// constructs a hashmap mapping op index -> label
    pub fn mapLabels(self: Self, ally: Allocator) Allocator.Error!LabelMap {
        var map = LabelMap{};
        for (self.labels) |index, i| {
            try map.put(ally, index, Label.of(i));
        }

        return map;
    }

    fn renderLocal(
        self: Self,
        ctx: *kz.Context,
        env: Env,
        local: Local
    ) !kz.Ref {
        const ty = self.locals[local.index];
        const ty_text = try ty.writeAlloc(ctx.ally, env.tw);
        defer ctx.ally.free(ty_text);

        const ty_tex = try ctx.print(.{ .fg = .green }, "{s}", .{ty_text});
        const var_tex = try ctx.print(.{}, "%{}", .{local.index});

        return try ctx.slap(ty_tex, var_tex, .right, .{ .space = 1 });
    }

    fn renderOp(
        self: Self,
        ctx: *kz.Context,
        env: *Env,
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
                const ty = self.locals[ldc.to.index];
                const value = self.consts[ldc.a.index];
                const expr = try value.resurrect(env.*, ty);
                defer expr.deinit(ctx.ally);

                try line.appendSlice(&.{
                    try self.renderLocal(ctx, env.*, ldc.to),
                    try ctx.print(.{}, " = ", .{}),
                    try expr.render(ctx, env.tw),
                });
            },
            .branch => |br| {
                try line.appendSlice(&.{
                    try ctx.print(.{}, "{s} ", .{tag}),
                    try self.renderLocal(ctx, env.*, br.cond),
                    try ctx.clone(comma),
                    try renderLabel(ctx, br.a),
                    try ctx.clone(comma),
                    try renderLabel(ctx, br.b),
                });
            },
            .jump => |jmp| {
                try line.appendSlice(&.{
                    try ctx.print(.{}, "{s} ", .{tag}),
                    try renderLabel(ctx, jmp.dst),
                });
            },
            .alloca => |all| {
                // create u64 size for sake of consistency
                const @"u64" = try env.identify(types.Type{
                    .number = .{ .layout = .uint, .bits = 64 },
                });
                const expr = TExpr.init(null, @"u64", .{
                    .number = .{
                        .bits = 64,
                        .data = .{ .uint = @intCast(u64, all.size) }
                    }
                });
                defer expr.deinit(env.ally);

                try line.appendSlice(&.{
                    try self.renderLocal(ctx, env.*, all.to),
                    try ctx.print(.{}, " = {s} ", .{tag}),
                    try expr.render(ctx, env.tw),
                });
            },
            .unary => |un| {
                try line.appendSlice(&.{
                    try self.renderLocal(ctx, env.*, un.to),
                    try ctx.print(.{}, " = {s} ", .{tag}),
                    try self.renderLocal(ctx, env.*, un.a),
                });
            },
            .binary => |bin| {
                try line.appendSlice(&.{
                    try self.renderLocal(ctx, env.*, bin.to),
                    try ctx.print(.{}, " = {s} ", .{tag}),
                    try self.renderLocal(ctx, env.*, bin.a),
                    try ctx.clone(comma),
                    try self.renderLocal(ctx, env.*, bin.b),
                });
            },
            .unary_eff => |un| {
                try line.appendSlice(&.{
                    try ctx.print(.{}, "{s} ", .{tag}),
                    try self.renderLocal(ctx, env.*, un.a),
                });
            },
            .binary_eff => |bin| {
                try line.appendSlice(&.{
                    try ctx.print(.{}, "{s} ", .{tag}),
                    try self.renderLocal(ctx, env.*, bin.a),
                    try ctx.clone(comma),
                    try self.renderLocal(ctx, env.*, bin.b),
                });
            },
            else => std.debug.panic(
                "TODO render op class {s}",
                .{@tagName(class)}
            )
        }

        // stack and return
        return ctx.stack(line.items, .right, .{});
    }

    fn renderLabel(ctx: *kz.Context, label: Label) !kz.Ref {
        return try ctx.print(.{ .fg = .cyan }, "@{}", .{label.index});
    }

    fn renderBody(self: Self, ctx: *kz.Context, env: *Env) !kz.Ref {
        const INDENT = 4;

        var body = try ctx.stub();

        // entry point
        const entry = try ctx.slap(
            try ctx.print(.{}, "enter", .{}),
            try renderLabel(ctx, self.entry),
            .right,
            .{ .space = 1 }
        );

        body = try ctx.slap(body, entry, .bottom, .{});

        // construct reverse label map
        var labels = try self.mapLabels(ctx.ally);
        defer labels.deinit(ctx.ally);

        // render ops
        var y: isize = 1;
        for (self.ops) |op, i| {
            if (labels.get(i)) |label| {
                const tex = try renderLabel(ctx, label);
                body = try ctx.unify(body, tex, .{0, y});
                y += 1;
            }

            const tex = try self.renderOp(ctx, env, op);
            body = try ctx.unify(body, tex, .{INDENT, y});
            y += 1;
        }

        return body;
    }

    pub fn render(self: Self, ctx: *kz.Context, env: *Env) !kz.Ref {
        // function header
        var header_texs = std.ArrayList(kz.Ref).init(ctx.ally);
        defer header_texs.deinit();

        try header_texs.appendSlice(&.{
            try ctx.print(.{ .fg = .red }, "{s}", .{self.label.str}),
            try ctx.print(.{}, " :: ", .{}),
        });

        var param = Local.of(0);
        while (param.index < self.takes) : (param.index += 1) {
            try header_texs.append(try self.renderLocal(ctx, env.*, param));
            try header_texs.append(try ctx.print(.{}, " -> ", .{}));
        }

        const returns = try self.returns.writeAlloc(ctx.ally, env.tw);
        defer ctx.ally.free(returns);

        try header_texs.appendSlice(&.{
            try ctx.print(.{ .fg = .green }, "{s}", .{returns}),
        });

        // put it together
        const header = try ctx.stack(header_texs.items, .right, .{});
        const body = try self.renderBody(ctx, env);
        return ctx.slap(header, body, .bottom, .{});
    }
};

pub const Program = struct {
    const Self = @This();

    funcs: []Func,
    entry: FuncRef,

    pub fn deinit(self: Self, ally: Allocator) void {
        for (self.funcs) |func| func.deinit(ally);
        ally.free(self.funcs);
    }

    pub fn render(self: Self, ctx: *kz.Context, env: *Env) !kz.Ref {
        var tex = try ctx.stub();
        for (self.funcs) |func| {
            tex = try ctx.slap(tex, try func.render(ctx, env), .bottom, .{});
        }

        return tex;
    }
};

// builders ====================================================================

pub const FuncBuilder = struct {
    const Self = @This();

    ally: Allocator,

    label: Symbol,
    takes: usize, // number of parameters; these are the first n local values
    returns: TypeId,

    consts: std.ArrayListUnmanaged(Value) = .{},
    locals: std.ArrayListUnmanaged(TypeId) = .{},
    ops: std.ArrayListUnmanaged(Op) = .{},
    labels: std.ArrayListUnmanaged(usize) = .{},

    pub fn init(
        ally: Allocator,
        label: Symbol,
        takes: []const TypeId,
        returns: TypeId
    ) Allocator.Error!Self {
        var self = Self{
            .ally = ally,
            .label = try label.clone(ally),
            .takes = takes.len,
            .returns = returns,
        };

        try self.locals.appendSlice(self.ally, takes);

        return self;
    }

    /// invalidates this builder.
    pub fn build(self: *Self, entry: Label) Func {
        return Func{
            .label = self.label,
            .takes = self.takes,
            .returns = self.returns,
            .entry = entry,
            .consts = self.consts.toOwnedSlice(self.ally),
            .locals = self.locals.toOwnedSlice(self.ally),
            .ops = self.ops.toOwnedSlice(self.ally),
            .labels = self.labels.toOwnedSlice(self.ally),
        };
    }

    pub fn addConst(self: *Self, data: []const u8) Allocator.Error!Const {
        const @"const" = Const.of(self.consts.items.len);
        try self.consts.append(self.ally, try Value.init(self.ally, data));

        return @"const";
    }

    pub fn addLocal(self: *Self, ty: TypeId) Allocator.Error!Local {
        const local = Local.of(self.locals.items.len);
        try self.locals.append(self.ally, ty);

        return local;
    }

    pub fn addLabel(self: *Self) Allocator.Error!Label {
        const label = Label.of(self.labels.items.len);
        try self.labels.append(self.ally, self.ops.items.len);

        return label;
    }

    pub fn addOp(self: *Self, op: Op) Allocator.Error!void {
        try self.ops.append(self.ally, op);
    }

    /// this replaces a label's original op index with the next op index.
    ///
    /// this allows generating SSA IR without messing with backrefs!
    pub fn replaceLabel(self: *Self, label: Label) Allocator.Error!void {
        self.labels.items[label.index] = self.ops.items.len;
    }
};

pub const ProgramBuilder = struct {
    const Self = @This();

    ally: Allocator,
    funcs: std.ArrayListUnmanaged(Func) = .{},

    pub fn init(ally: Allocator) Self {
        return Self{
            .ally = ally,
        };
    }

    /// invalidates this builder.
    pub fn build(self: *Self, entry: FuncRef) Allocator.Error!Program {
        return Program{
            .funcs = self.funcs.toOwnedSlice(self.ally),
            .entry = entry,
        };
    }

    pub fn addFunc(self: *Self, func: Func) Allocator.Error!FuncRef {
        const ref = FuncRef.of(self.funcs.items.len);
        try self.funcs.append(self.ally, func);

        return ref;
    }
};