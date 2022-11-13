//! SSA IR primitives.
//!
//! the goal for this is to be a common target between static compilation and
//! dynamic execution. the dynamic vm will want extra processing to do things
//! like stack allocation and optimizing byte loads for the cache, but for
//! compiling to qbe or llvm or whatever backend the goal is for this to be
//! sufficient

const std = @import("std");
const kz = @import("kritzler");
const util = @import("util");
const builtin = @import("builtin");
const types = @import("types.zig");
const Env = @import("env.zig");

const Allocator = std.mem.Allocator;
const Symbol = util.Symbol;
const TypeId = types.TypeId;

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
    alloca: Unary, // allocates a number of bytes and returns pointer
    store: BinaryEffect, // stores local b at address in ptr a
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

        // generalizable logic
        unary: Unary,
        binary: Binary,
        unary_eff: UnaryEffect,
        binary_eff: BinaryEffect,
    };

    /// makes switching on ops much easier
    pub fn classify(self: Self) Class {
        return switch (self) {
            .ldc => |ldc| Class{ .ldc = ldc },
            .call => |call| Class{ .call = call },
            .arg => |arg| Class{ .arg = arg },
            .br => |br| Class{ .branch = br },
            .jmp => |jmp| Class{ .jump = jmp },
            .cast, .not, .load, .alloca => |un| Class{ .unary = un },
            .add, .sub, .mul, .div, .mod, .@"or", .@"and"
                => |bin| Class{ .binary = bin },
            .ret => |un_eff| Class{ .unary_eff = un_eff },
            .store => |bin_eff| Class{ .binary_eff = bin_eff },
        };
    }
};

/// essentially just a system-v aligned pointer with some operations added.
/// this allows for generic operations over bits and easy c interop.
pub const Value = struct {
    const Self = @This();

    // NOTE potential optimization here would be to store only the raw array ptr
    ptr: []align(16) u8,

    /// allocates and dupes data to aligned ptr
    pub fn init(ally: Allocator, data: []const u8) Allocator.Error!Self {
        const mem = try ally.alignedAlloc(u8, 16, data.len);
        std.mem.copy(u8, mem, data);

        return Self{ .ptr = mem };
    }

    pub fn deinit(self: *Self, ally: Allocator) void {
        ally.free(self.ptr);
    }

    pub fn asPtr(self: Self, comptime T: type) *align(16) T {
        if (builtin.mode == .Debug) {
            if (@sizeOf(T) != self.ptr.len) {
                std.debug.panic(
                    "attempted to cast Value of size {} to type {} of size {}",
                    .{self.ptr.len, T, @sizeOf(T)}
                );
            }
        }

        return @ptrCast(*align(16) T, self.ptr);
    }

    /// bitcast to the type desired
    pub fn as(self: Self, comptime T: type) T {
        return self.asPtr(T).*;
    }

    pub const Printable = union(enum) {
        @"bool": bool,
        int: i64,
        uint: u64,
        float: f64,

        pub fn format(
            self: @This(),
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype
        ) @TypeOf(writer).Error!void {
            _ = fmt;
            _ = options;

            switch (self) {
                .@"bool" => |b| try writer.print("{}", .{b}),
                .int => |i| try writer.print("{}i", .{i}),
                .uint => |u| try writer.print("{}u", .{u}),
                .float => |f| try writer.print("{d}f", .{f}),
            }
        }
    };

    pub fn toPrintable(self: Self, env: Env, tid: TypeId) Printable {
        const ty = env.typeGet(tid);
        return switch (ty.*) {
            .@"bool" => Printable{ .@"bool" = self.as(u8) > 0 },
            .number => |num| num: {
                const bits = num.bits orelse 64;
                break :num switch (num.layout) {
                    .int => Printable{
                        .int = switch (bits) {
                            64 => self.as(i64),
                            32 => self.as(i32),
                            16 => self.as(i16),
                            8 => self.as(i8),
                            else => unreachable
                         }
                     },
                    .uint => Printable{
                        .uint = switch (bits) {
                            64 => self.as(u64),
                            32 => self.as(u32),
                            16 => self.as(u16),
                            8 => self.as(u8),
                            else => unreachable
                         }
                     },
                    .float => Printable{
                        .float = switch (bits) {
                            64 => self.as(f64),
                            32 => self.as(f32),
                            else => unreachable
                         }
                     },
                };
            },
            else => {
                const text = tid.writeAlloc(env.ally, env) catch unreachable;
                defer env.ally.free(text);

                std.debug.panic("TODO type {s} toPrintable", .{text});
            }
        };
    }
};

pub const Const = packed struct {
    const Self = @This();

    index: usize,

    pub fn of(index: usize) Self {
        return Self{ .index = index };
    }

    // TODO render() function with type for consistent output
};

pub const Local = packed struct {
    const Self = @This();

    index: usize,

    pub fn of(index: usize) Self {
        return Self{ .index = index };
    }

    // TODO render() function with type for consistent output

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

    fn renderLocal(
        self: Self,
        ctx: *kz.Context,
        env: Env,
        local: Local
    ) !kz.Ref {
        const ty_text = try self.locals[local.index].writeAlloc(ctx.ally, env);
        defer ctx.ally.free(ty_text);

        const ty = try ctx.print(.{ .fg = .green }, "{s}", .{ty_text});
        const var_tex = try ctx.print(.{}, "%{}", .{local.index});

        return try ctx.slap(ty, var_tex, .right, .{ .space = 1 });
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
                const ty = self.locals[ldc.to.index];
                const p = self.consts[ldc.a.index].toPrintable(env, ty);
                try line.appendSlice(&.{
                    try self.renderLocal(ctx, env, ldc.to),
                    try ctx.print(.{}, " = ", .{}),
                    try ctx.print(.{ .fg = .magenta }, "{}", .{p}),
                });
            },
            .branch => |br| {
                try line.appendSlice(&.{
                    try ctx.print(.{}, "{s} ", .{tag}),
                    try self.renderLocal(ctx, env, br.cond),
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
            .unary => |un| {
                try line.appendSlice(&.{
                    try self.renderLocal(ctx, env, un.to),
                    try ctx.print(.{}, " = {s} ", .{tag}),
                    try self.renderLocal(ctx, env, un.a),
                });
            },
            .binary => |bin| {
                try line.appendSlice(&.{
                    try self.renderLocal(ctx, env, bin.to),
                    try ctx.print(.{}, " = {s} ", .{tag}),
                    try self.renderLocal(ctx, env, bin.a),
                    try ctx.clone(comma),
                    try self.renderLocal(ctx, env, bin.b),
                });
            },
            .unary_eff => |un| {
                try line.appendSlice(&.{
                    try ctx.print(.{}, "{s} ", .{tag}),
                    try self.renderLocal(ctx, env, un.a),
                });
            },
            .binary_eff => |bin| {
                try line.appendSlice(&.{
                    try ctx.print(.{}, "{s} ", .{tag}),
                    try self.renderLocal(ctx, env, bin.a),
                    try ctx.clone(comma),
                    try self.renderLocal(ctx, env, bin.b),
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

    fn renderBody(self: Self, ctx: *kz.Context, env: Env) !kz.Ref {
        const INDENT = 4;

        var body = ctx.stub();

        // entry point
        const entry = try ctx.slap(
            try ctx.print(.{}, "enter", .{}),
            try renderLabel(ctx, self.entry),
            .right,
            .{ .space = 1 }
        );

        body = try ctx.slap(body, entry, .bottom, .{});

        // construct reverse label map
        var labels = std.AutoHashMap(usize, Label).init(ctx.ally);
        defer labels.deinit();

        for (self.labels) |index, i| {
            try labels.put(index, Label.of(i));
        }

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

    pub fn render(self: Self, ctx: *kz.Context, env: Env) !kz.Ref {
        // function header
        var header_texs = std.ArrayList(kz.Ref).init(ctx.ally);
        defer header_texs.deinit();

        try header_texs.appendSlice(&.{
            try ctx.print(.{ .fg = .red }, "{s}", .{self.label.str}),
            try ctx.print(.{}, " :: (", .{}),
        });


        var param = Local.of(0);
        while (param.index < self.takes) : (param.index += 1) {
            if (param.index > 0) {
                try header_texs.append(try ctx.print(.{}, ", ", .{}));
            }
            try header_texs.append(try self.renderLocal(ctx, env, param));
        }

        const returns = try self.returns.writeAlloc(ctx.ally, env);
        defer ctx.ally.free(returns);

        try header_texs.appendSlice(&.{
            try ctx.print(.{}, ") -> ", .{}),
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

    pub fn render(self: Self, ctx: *kz.Context, env: Env) !kz.Ref {
        var tex = ctx.stub();
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