const std = @import("std");
const Expr = @import("expr.zig");
const FlType = @import("type.zig").FlType;

/// FlValue is the *dynamic* representation of a Fluent value
/// TODO eventually may be able to remove the enum as an optimization
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

    const Fmt = struct {
        self: *const Self,
        options: FmtOptions,

        pub fn format(
            formattable: *const Fmt,
            comptime fmt_str: []const u8,
            fmt_options: std.fmt.FormatOptions,
            writer: anytype
        ) @TypeOf(writer).Error!void {
            _ = fmt_str;
            _ = fmt_options;

            const self = formattable.self;
            const options = formattable.options;

            if (options.typed) {
                try writer.print("<{s}> ", .{@tagName(self.*)});
            }

            switch (self.*) {
                .int => |n| try writer.print("{d}", .{n}),
                .float => |n| try writer.print("{d}", .{n}),
                .string => |s| try writer.print("{s}", .{s}),
            }
        }
    };

    const FmtOptions = struct {
        typed: bool = false,
    };

    pub fn fmt(self: *const Self, options: FmtOptions) Fmt {
        return Fmt{
            .self = self,
            .options = options
        };
    }
};