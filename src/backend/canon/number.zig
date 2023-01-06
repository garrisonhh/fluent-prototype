//! represents data for a generic number with layout information. used across
//! the different representations of fluent source.

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const util = @import("util");
const Value = @import("../value.zig");

pub const Layout = util.Number.Layout;

const Self = @This();

const Int = i64;
const UInt = u64;
const Float = f64;

pub const Concrete = union(Layout) {
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

pub fn cast(self: Self, bits: ?u8, layout: util.Number.Layout) Self {
    const data = switch (self.data) {
        .int => |i| switch (layout) {
            .int => self.data,
            .uint => Concrete{ .uint = @intCast(UInt, i) },
            .float => Concrete{ .float = @intToFloat(Float, i) },
        },
        .uint => |u| switch (layout) {
            .int => Concrete{ .int = @intCast(Int, u) },
            .uint => self.data,
            .float => Concrete{ .float = @intToFloat(Float, u) },
        },
        .float => |f| switch (layout) {
            .int => Concrete{ .int = @floatToInt(Int, f) },
            .uint => Concrete{ .uint = @floatToInt(UInt, f) },
            .float => self.data,
        },
    };

    return Self{
        .bits = bits,
        .data = data,
    };
}

/// returns this as a value on an allocator
pub fn asValue(self: @This(), ally: Allocator) Allocator.Error!Value {
    const data = switch (self.data) {
        .int => |n| std.mem.asBytes(&n),
        .uint => |n| std.mem.asBytes(&n),
        .float => |n| std.mem.asBytes(&n),
    };

    const bits = self.bits orelse 64;
    std.debug.assert(bits % 8 == 0 and bits / 8 <= 8);
    std.debug.assert(builtin.target.cpu.arch.endian() == .Little);

    return try Value.init(ally, data[0..bits / 8]);
}

pub fn eql(self: @This(), other: @This()) bool {
    // compare type
    if (self.bits != other.bits
     or @as(Layout, self.data) != @as(Layout, other.data)) {
        return false;
    }

    // compare bytes
    var mem: [32]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&mem);
    const ally = fba.allocator();

    const val1 = self.asValue(ally) catch unreachable;
    const val2 = other.asValue(ally) catch unreachable;

    return std.mem.eql(u8, val1.ptr, val2.ptr);
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
