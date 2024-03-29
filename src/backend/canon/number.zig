//! represents data for a generic number with layout information. used across
//! the different representations of fluent source.

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const com = @import("common");

pub const Layout = com.Number.Layout;

const Self = @This();

pub const Int = i64;
pub const UInt = u64;
pub const Float = f64;

pub const Concrete = union(Layout) {
    int: Int,
    uint: UInt,
    float: Float,
};

bits: ?u8,
data: Concrete,

pub fn from(num: com.Number) com.ParseNumberError!Self {
    return Self{
        .bits = num.bits,
        .data = if (num.layout) |layout| switch (layout) {
            .int => .{ .int = try num.to(Int) },
            .uint => .{ .uint = try num.to(UInt) },
            .float => .{ .float = try num.to(Float) },
        } else if (num.post.len > 0) .{
            .float = try num.to(Float),
        } else .{
            .int = try num.to(Int),
        },
    };
}

pub fn cast(self: Self, bits: ?u8, layout: com.Number.Layout) Self {
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

pub fn asBytes(self: Self) *const [8]u8 {
    return switch (self.data) {
        inline else => |n| @ptrCast(*const [8]u8, &n),
    };
}

pub fn eql(self: Self, other: Self) bool {
    // compare type
    // zig fmt: off
    const types_match = self.bits == other.bits
                    and @as(Layout, self.data) == @as(Layout, other.data);
    // zig fmt: on
    if (!types_match) return false;

    // compare bytes
    var mem: [32]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&mem);
    const ally = fba.allocator();

    const val1 = self.asValue(ally) catch unreachable;
    const val2 = other.asValue(ally) catch unreachable;

    return std.mem.eql(u8, val1.buf, val2.buf);
}

pub fn format(
    self: Self,
    comptime _: []const u8,
    _: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    switch (self.data) {
        .int => |i| try writer.print("{}i", .{i}),
        .uint => |u| try writer.print("{}u", .{u}),
        .float => |f| try writer.print("{d}f", .{f}),
    }

    if (self.bits) |bits| try writer.print("{}", .{bits});
}
