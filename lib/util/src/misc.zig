const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");

/// takes a value and shallow copies it onto an allocator
pub fn placeOn(
    ally: Allocator,
    value: anytype
) Allocator.Error!*@TypeOf(value) {
    const ptr = try ally.create(@TypeOf(value));
    ptr.* = value;
    return ptr;
}