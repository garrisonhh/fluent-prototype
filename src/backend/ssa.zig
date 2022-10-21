//! SSA IR primitives.
//!
//! the goal for this is to be a common target between static compilation and
//! dynamic execution. the dynamic vm will want extra processing to do things
//! like stack allocation and optimizing byte loads for the cache, but for
//! compiling to qbe or llvm or whatever backend the goal is for this to be
//! sufficient

const std = @import("std");
const builtin = @import("builtin");
const types = @import("types.zig");

const Allocator = std.mem.Allocator;
const TypeId = types.TypeId;

/// symbolic representation of operationss. since blocks store type info,
/// there is no need for operations to be type or size specific; this can be
/// deduced later on
pub const Op = union(enum) {
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
        dest: *const Block,
    };

    pub const Jnz = struct {
        cond: usize,
        dest: *const Block,
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

    // control flow
    jmp: Jump,
    jnz: Jnz,
    ret: Jump,

    // functions
    call: Jump,
    arg: Arg,
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

    name: []const u8,
    consts: []Value,
    locals: []TypeId,
    ops: []Op,

    pub fn deinit(self: Self, ally: Allocator) void {
        ally.free(self.name);
        for (self.consts) |*value| value.deinit(ally);
        ally.free(self.consts);
        ally.free(self.locals);
        ally.free(self.ops);
    }
};

pub const Program = struct {
    const Self = @This();

    blocks: []Block,
    entry: *const Block,

    pub fn deinit(self: Self, ally: Allocator) void {
        for (self.blocks) |block| block.deinit(ally);
        ally.free(Self.blocks);
    }
};