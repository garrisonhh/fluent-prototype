const std = @import("std");
const Expr = @import("expr.zig");
const FlType = @import("type.zig").FlType;

/// FlValue is the *dynamic* representation of a Fluent value
pub const FlValue = union(enum) {
    const Self = @This();

    int: i64,
    float: f64,
    string: []const u8,

    pub fn from_literal(expr: *const Expr) !Self {
        std.debug.assert(expr.is_flat_literal());
        return switch (expr.etype) {
            .int => Self{
                .int = try std.fmt.parseInt(i64, expr.slice, 10)
            },
            .float => Self{
                .float = try std.fmt.parseFloat(f64, expr.slice)
            },
            .string => Self{
                .string = expr.slice
            },
            else => @panic("TODO"),
        };
    }

    pub fn format(
        self: *const Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        _ = fmt;
        _ = options;

        switch (self.*) {
            .int => |n| try writer.print("{d}", .{n}),
            .float => |n| try writer.print("{d}", .{n}),
            .string => |s| try writer.print("{s}", .{s}),
        }
    }
};