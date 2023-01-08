//! value is the canonical low-level representation of a type-erased value. this
//! is what SSA IR constants use, and the bytecode VM follows its layout rules
//! in code generation.

// TODO move this to canon?

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");

const Self = @This();

buf: []u8,

pub fn of(buf: []u8) Self {
    return Self{ .buf = buf };
}

/// allocates and dupes data to aligned ptr
pub fn init(ally: Allocator, buf: []const u8) Allocator.Error!Self {
    return Self.of(try ally.dupe(u8, buf));
}

pub fn deinit(self: Self, ally: Allocator) void {
    ally.free(self.buf);
}

/// bitcast to the type desired
pub fn as(self: Self, comptime T: type) T {
    // bitcast
    var t: T = undefined;
    const view = std.mem.asBytes(&t);
    std.mem.set(u8, view, 0);
    std.mem.copy(u8, view, self.buf);

    return t;
}
