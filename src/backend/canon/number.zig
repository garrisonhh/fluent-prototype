//! represents data for a generic number with layout information. used across
//! the different representations of fluent source.

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const util = @import("util");

const Self = @This();

const Int = i64;
const UInt = u64;
const Float = f64;

pub const Concrete = union(util.Number.Layout) {
    int: Int,
    uint: UInt,
    float: Float,
};

bits: ?u8,
data: Concrete,

pub fn from(num: util.Number) util.ParseNumberError!Self {
    return Self{
        .bits = num.bits,
        .data = if (num.layout) |layout| switch (layout) {
            .int  => .{ .int = try num.to(Int) },
            .uint  => .{ .uint = try num.to(UInt) },
            .float => .{ .float = try num.to(Float) },
        } else if (num.post.len > 0) .{ .float = try num.to(Float) }
        else .{ .int = try num.to(Int) }
    };
}

/// returns this value as bytes on an allocator
pub fn asBytes(self: @This(), ally: Allocator) Allocator.Error![]const u8 {
    const data = switch (self.data) {
        .int => |n| std.mem.asBytes(&n),
        .uint => |n| std.mem.asBytes(&n),
        .float => |n| std.mem.asBytes(&n),
    };

    const bits = self.bits orelse 64;
    std.debug.assert(bits % 8 == 0 and bits / 8 <= 8);
    std.debug.assert(builtin.target.cpu.arch.endian() == .Little);

    return ally.dupe(u8, data[0..bits / 8]);
}

pub fn eql(self: @This(), other: @This()) bool {
    // compare type
    const Layout = util.Number.Layout;
    if (self.bits != other.bits
     or @as(Layout, self.data) != @as(Layout, other.data)) {
        return false;
    }

    // compare bytes
    var mem: [16]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&mem);
    const ally = fba.allocator();

    const bytes1 = self.asBytes(ally) catch unreachable;
    const bytes2 = other.asBytes(ally) catch unreachable;

    return std.mem.eql(u8, bytes1, bytes2);
}

pub fn format(
    self: @This(),
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    _ = fmt;
    _ = options;

    switch (self.data) {
        .int => |i| try writer.print("{}i", .{i}),
        .uint => |u| try writer.print("{}u", .{u}),
        .float => |f| try writer.print("{d}f", .{f}),
    }

    if (self.bits) |bits| try writer.print("{}", .{bits});
}
