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
    value: anytype,
) Allocator.Error!*@TypeOf(value) {
    const ptr = try ally.create(@TypeOf(value));
    ptr.* = value;
    return ptr;
}

/// pads a size so that it fits an alignment
pub fn padAlignment(sz: usize, aln: usize) usize {
    const aln_diff = sz % aln;
    return if (aln_diff == 0) sz else sz + aln - aln_diff;
}
