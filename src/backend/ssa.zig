//! SSA IR primitives.
//!
//! the goal for this is to be a common target between static compilation and
//! dynamic execution. the dynamic vm will want extra processing to do things
//! like stack allocation and optimizing byte loads for the cache, but for
//! compiling to qbe or llvm or whatever backend the goal is for this to be
//! sufficient

const std = @import("std");
const util = @import("util");
const builtin = @import("builtin");
const types = @import("types.zig");

const Allocator = std.mem.Allocator;
const Symbol = util.Symbol;
const TypeId = types.TypeId;

/// symbolic representation of operationss. since blocks store type info,
/// there is no need for operations to be type or size specific; this can be
/// deduced later on
pub const Op = union(enum) {
    const Self = @This();

    pub const Unary = struct {
        a: usize,
        to: usize,
    };

    pub const Binary = struct {
        a: usize,
        b: usize,
        to: usize,
    };

    pub const Jump = struct {
        dest: *const Symbol,
    };

    pub const Jnz = struct {
        cond: usize,
        dest: *const Symbol,
    };

    pub const Arg = struct {
        arg: usize,
        from: usize,
    };

    // unique
    ldc: Unary,

    // math
    add: Binary,
    sub: Binary,
    mul: Binary,
    div: Binary,
    mod: Binary,

    // bitwise
    @"or": Binary,
    @"and": Binary,
    xor: Binary,
    not: Unary,
    call: Jump,
    arg: Arg,

    pub const Class = union(enum) {
        // unique logic
        ldc: *Unary,
        ret: *Jump,
        jmp: *Jump,
        jnz: *Jnz,
        call: *Jump,
        arg: *Arg,

        // generalizable logic
        binary: *Binary,
        unary: *Unary,
    };

    /// makes switching on ops much easier
    pub fn classify(self: Self) Class {
        return switch (self) {
            .ldc => |*ldc| Class{ .ldc = ldc },
            .ret => |*ret| Class{ .ret = ret },
            .jmp => |*jmp| Class{ .jmp = jmp },
            .jnz => |*jnz| Class{ .jnz = jnz },
            .call => |*call| Class{ .call = call },
            .arg => |*arg| Class{ .arg = arg },

            .not => |*un| Class{ .unary = un },
            .add, .sub, .mul, .div, .mod => |*bin| Class{ .binary = bin },
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
            if (@sizeOf(T) != self.len) {
                std.debug.panic(
                    "attempted to cast Value of size {} to type {} of size {}",
                    .{self.len, T, @sizeOf(T)}
                );
            }
        }

        return @ptrCast(*align(16) T, self.ptr);
    }

    /// essentially a bitcast to the type desired
    pub fn as(self: Self, comptime T: type) T {
        return self.asPtr(T).*;
    }
};

/// linear block representation
///
/// expects to own all memory
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
};

pub const Program = struct {
    const Self = @This();

    blocks: Symbol.HashMapUnmanaged(Block),
    entry: *const Block,

    // jump operations use symbols. to avoid having to iterate through ops
    // to deinitialize them, they are pooled here
    jmp_symbols: []Symbol,

    pub fn deinit(self: Self, ally: Allocator) void {
        var blocks = self.blocks.valueIterator();
        while (blocks.next()) |block| block.deinit(ally);
        self.blocks.deinit(ally);

        for (self.jmp_symbols) |sym| ally.free(sym.str);
        ally.free(self.jmp_symbols);
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
            .consts = self.consts.toOwnedSlice(),
            .locals = self.locals.toOwnedSlice(),
            .ops = self.ops.toOwnedSlice(),
        };
    }

    pub fn addConst(self: *Self, data: []const u8) Allocator.Error!void {
        try self.consts.append(self.ally, try Value.init(self.ally, data));
    }

    pub fn addLocal(self: *Self, ty: TypeId) Allocator.Error!void {
        try self.locals.append(ty);
    }

    pub fn addOp(self: *Self, op: Op) Allocator.Error!void {
        try self.ops.append(op);
    }
};

pub const ProgramBuilder = struct {
    // TODO
};