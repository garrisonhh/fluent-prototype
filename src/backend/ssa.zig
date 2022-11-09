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

pub const BlockRef = packed struct {
    const Self = @This();

    index: usize,

    pub fn of(index: usize) Self {
        return Self{ .index = index };
    }
};

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
        dest: BlockRef,
    };

    pub const Arg = struct {
        arg: usize,
        from: Local,
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
            .cast, .not => |un| Class{ .unary = un },
            .add, .sub, .mul, .div, .mod, .@"or", .@"and"
                => |bin| Class{ .binary = bin },
            .ret => |un_eff| Class{ .unary_eff = un_eff },
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

/// linear block representation
///
/// expects to own all memory (except for jump symbols)
pub const Block = struct {
    const Self = @This();

    label: Symbol,
    consts: []Value,
    locals: []TypeId,
    ops: []Op,

    pub fn deinit(self: Self, ally: Allocator) void {
        ally.free(self.label.str);
        for (self.consts) |*value| value.deinit(ally);
        ally.free(self.consts);
        ally.free(self.locals);
        ally.free(self.ops);
    }

    pub fn render(self: Self, env: Env, ally: Allocator) !kz.Texture {
        // locals
        const locals = try ally.alloc([]u8, self.locals.len);
        defer {
            for (locals) |str| ally.free(str);
            ally.free(locals);
        }

        for (self.locals) |ty, i| {
            locals[i] = try ty.writeAlloc(ally, env);
        }

        // ops
        var op_text = std.ArrayList(u8).init(ally);
        defer op_text.deinit();
        const op_writer = op_text.writer();

        for (self.ops) |op| {
            switch (op.classify()) {
                .ldc => |ldc| {
                    const @"const" = self.consts[ldc.a.index];
                    const local = self.locals[ldc.to.index];
                    const val = @"const".toPrintable(env, local);

                    try op_writer.print(
                        "{s} {} = {}\n",
                        .{locals[ldc.to.index], ldc.to, val}
                    );
                },
                .unary => |un| {
                    const tag = @tagName(op);
                    const ty_to = locals[un.to.index];
                    const ty_a = locals[un.a.index];

                    try op_writer.print(
                        "{s} {} = {s} {s} {}\n",
                        .{ty_to, un.to, tag, ty_a, un.a}
                    );
                },
                .binary => |bin| {
                    const tag = @tagName(op);
                    const ty_to = locals[bin.to.index];
                    const ty_a = locals[bin.a.index];
                    const ty_b = locals[bin.b.index];

                    try op_writer.print(
                        "{s} {} = {s} {s} {}, {s} {}\n",
                        .{ty_to, bin.to, tag, ty_a, bin.a, ty_b, bin.b}
                    );
                },
                .unary_eff => |un_eff| {
                    const tag = @tagName(op);
                    const ty_a = locals[un_eff.a.index];

                    try op_writer.print(
                        "{s} {s} {}\n",
                        .{tag, ty_a, un_eff.a}
                    );
                },
                .binary_eff => |bin_eff| {
                    const tag = @tagName(op);
                    const ty_a = locals[bin_eff.a.index];
                    const ty_b = locals[bin_eff.b.index];

                    try op_writer.print(
                        "{s} {s} {}, {s} {}\n",
                        .{tag, ty_a, bin_eff.a, ty_b, bin_eff.b}
                    );
                },
                else => @panic("TODO"),
            }
        }

        // stack with label
        const tex = try kz.Texture.from(ally, kz.Format{}, op_text.items);
        defer tex.deinit(ally);
        const label =
            try kz.Texture.print(ally, kz.Format{}, "@{}", .{self.label});
        defer label.deinit(ally);

        return label.unify(ally, tex, .{2, 1});
    }
};

pub const Program = struct {
    const Self = @This();

    blocks: []Block,
    entry: BlockRef,

    pub fn deinit(self: Self, ally: Allocator) void {
        for (self.blocks) |block| block.deinit(ally);
        ally.free(self.blocks);
    }

    pub fn render(self: Self, env: Env, ally: Allocator) !kz.Texture {
        const blocks = try ally.alloc(kz.Texture, self.blocks.len);
        defer {
            for (blocks) |tex| tex.deinit(ally);
            ally.free(blocks);
        }

        for (self.blocks) |block, i| {
            blocks[i] = try block.render(env, ally);
        }

        return try kz.Texture.stack(ally, blocks, .bottom, .close);
    }
};

// builders ====================================================================

pub const BlockBuilder = struct {
    const Self = @This();

    ally: Allocator,
    label: Symbol,
    consts: std.ArrayListUnmanaged(Value) = .{},
    locals: std.ArrayListUnmanaged(TypeId) = .{},
    ops: std.ArrayListUnmanaged(Op) = .{},

    pub fn init(ally: Allocator, label: Symbol) Allocator.Error!Self {
        return Self{
            .ally = ally,
            .label = try label.clone(ally),
        };
    }

    /// invalidates this builder.
    pub fn build(self: *Self) Block {
        return Block{
            .label = self.label,
            .consts = self.consts.toOwnedSlice(self.ally),
            .locals = self.locals.toOwnedSlice(self.ally),
            .ops = self.ops.toOwnedSlice(self.ally),
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

    pub fn addOp(self: *Self, op: Op) Allocator.Error!void {
        try self.ops.append(self.ally, op);
    }
};

pub const ProgramBuilder = struct {
    const Self = @This();

    ally: Allocator,
    blocks: std.ArrayListUnmanaged(Block) = .{},

    pub fn init(ally: Allocator) Self {
        return Self{
            .ally = ally,
        };
    }

    /// invalidates this builder.
    pub fn build(self: *Self, entry: BlockRef) Allocator.Error!Program {
        return Program{
            .blocks = self.blocks.toOwnedSlice(self.ally),
            .entry = entry,
        };
    }

    pub fn addBlock(self: *Self, block: Block) Allocator.Error!BlockRef {
        const ref = BlockRef.of(self.blocks.items.len);
        try self.blocks.append(self.ally, block);

        return ref;
    }
};