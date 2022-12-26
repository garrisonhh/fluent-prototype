//! canonical data representation for the bytecode model

const std = @import("std");
const builtin = @import("builtin");

pub const Number = @import("canon/number.zig");
pub const Builtin = @import("canon/builtins.zig").Builtin;
pub usingnamespace @import("canon/prelude.zig");

/// given up to 8 bytes, return canonical u64 representation for vm
pub fn toCanonical(bytes: []const u8) u64 {
    std.debug.assert(bytes.len <= 8);
    std.debug.assert(builtin.cpu.arch.endian() == .Little);

    var dst: [8]u8 align(8) = undefined;
    std.mem.set(u8, &dst, 0);
    std.mem.copy(u8, &dst, bytes);

    return @ptrCast(*const u64, &dst).*;
}

pub fn fromCanonical(buf: []u8, n: u64) void {
    std.debug.assert(builtin.cpu.arch.endian() == .Little);

    const slice = @ptrCast(*const [8]u8, &n);
    const nbytes = 8 - (@clz(n) / 8);

    std.mem.set(u8, buf, 0);
    std.mem.copy(u8, buf, slice[0..nbytes]);
}
