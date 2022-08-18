const std = @import("std");
const FlType = @import("type.zig").FlType;

const Allocator = std.mem.Allocator;

/// FlValue is the dynamic representation of a Fluent value. it is specifically
/// designed to be usable on an arena allocator, no reallocations are performed.
/// eventually may be able to remove the enum as an optimization
pub const FlValue = union(enum) {
    const Self = @This();
    const Enum = @typeInfo(Self).Union.tag_type.?;

    comptime {
        // nil being zero means initializing empty FlValues will be faster
        std.debug.assert(@enumToInt(Enum.nil) == 0);
    }

    nil,
    int: i64,
    float: f64,
    // TODO a global type set with type ids?
    ltype: FlType,
    list: []Self,

    /// creates a zeroed list
    pub fn init_list(ally: Allocator, n: usize) Allocator.Error!Self {
        var list = try ally.alloc(Self, n);
        std.mem.set(Self, list, Self{ .nil = {} });

        return Self{ .list = list };
    }

    pub fn deinit(self: *Self, ally: Allocator) void {
        switch (self.*) {
            .ltype => |*t| t.deinit(ally),
            .list => |l| ally.free(l),
            else => {}
        }
    }

    pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
        return switch (self) {
            .nil, .int, .float => self,
            .ltype => |t| Self{ .ltype = try t.clone(ally) },
            .list => |l| blk: {
                var new_list = try ally.alloc(FlValue, self.list.len);
                for (l) |child, i| new_list[i] = try child.clone(ally);

                break :blk Self{ .list = new_list };
            },
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
                .nil => try writer.writeAll("nil"),
                .int => |n| try writer.print("{d}", .{n}),
                .float => |n| try writer.print("{d}", .{n}),
                .ltype => |t| try writer.print("{}", .{t}),
                .list => |l| {
                    try writer.writeByte('[');
                    for (l) |child, i| {
                        if (i > 0) try writer.writeByte(' ');
                        try writer.print("{}", .{child.fmt(options)});
                    }
                    try writer.writeByte(']');
                }
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