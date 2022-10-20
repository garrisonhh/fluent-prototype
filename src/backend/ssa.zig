//! SSA IR data structure definitions.
//!
//! the goal for this is to be a common target between static compilation and
//! dynamic execution. the dynamic vm will want extra processing to do things
//! like stack allocation and optimizing byte loads for the cache, but for
//! compiling to qbe or llvm or whatever backend the goal is for this to be
//! sufficient

const std = @import("std");
const types = @import("types.zig");

const Allocator = std.mem.Allocator;
const TypeId = types.TypeId;

/// symbolic representation of instructions. since blocks store type info,
/// there is no need for instructions to be type or size aware; this can be
/// deduced later on
pub const Inst = union(Op) {
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

pub const Value = struct {
    const Self = @This();

    ptr: [*]align(16) u8,

    /// cast to a pointer with a comptime-known size
    pub fn asSize(self: Self, comptime N: usize) *align(16) [N]u8 {
        return @ptrCast(*align(16) [N]u8, self.ptr);
    }

    /// essentially a bitcast to the correct type
    pub fn deref(self: Self, comptime T: type) T {
        return *@ptrCast(*T, self.ptr);
    }
};

pub const Block = struct {
    const_mem: []align(16) u8, // storage for constants
    consts: []Value,
    locals: []TypeId,
    ops: []Op,
};