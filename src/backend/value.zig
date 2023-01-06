//! value is the canonical low-level representation of a type-erased value. this
//! is what SSA IR constants use, and the bytecode VM follows its layout rules
//! in code generation.

// TODO move this to canon?

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const util = @import("util");
const Env = @import("env.zig");
const types = @import("types.zig");
const TypeId = types.TypeId;
const Type = types.Type;
const TExpr = @import("texpr.zig");
const FuncRef = @import("ssa.zig").FuncRef;
const canon = @import("canon.zig");
const context = @import("../context.zig");

const Self = @This();

ptr: []align(16) u8,

/// allocates and dupes data to aligned ptr
pub fn init(ally: Allocator, data: []const u8) Allocator.Error!Self {
    const self = try initEmpty(ally, data.len);
    std.mem.copy(u8, self.ptr, data);

    return self;
}

pub fn initEmpty(ally: Allocator, size: usize) Allocator.Error!Self {
    return Self{ .ptr = try ally.alignedAlloc(u8, 16, size) };
}

pub fn deinit(self: Self, ally: Allocator) void {
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
