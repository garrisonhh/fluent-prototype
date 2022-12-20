//! canonical data representation for the bytecode model

const std = @import("std");

/// given up to 8 bytes, return canonical u64 representation for vm
pub fn toCanonical(bytes: []const u8) u64 {
    std.debug.assert(bytes.len <= 8);

    var conv: u64 = 0;
    const slice = @ptrCast(*[8]u8, &conv);

    std.mem.copy(u8, slice[8 - bytes.len..], bytes);

    return conv;
}

pub fn fromCanonical(buf: []u8, n: u64) void {
    const slice = @ptrCast(*const [8]u8, &n);
    std.mem.copy(u8, buf, slice[8 - buf.len..]);
}