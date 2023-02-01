const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");

/// gets current time in milliseconds (see std.time.nanoTimestamp)
pub fn now() f64 {
    return 1e-6 * @intToFloat(f64, std.time.nanoTimestamp());
}

/// takes a value and shallow copies it onto an allocator
pub fn placeOn(
    ally: Allocator,
    value: anytype
) Allocator.Error!*@TypeOf(value) {
    const ptr = try ally.create(@TypeOf(value));
    ptr.* = value;
    return ptr;
}