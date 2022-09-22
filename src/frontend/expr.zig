const std = @import("std");
const util = @import("../util/util.zig");

const Allocator = std.mem.Allocator;

/// AstExpr is the most basic AST representation
const Self = @This();

pub const Type = enum {
    number,
    character,
    string,
    symbol,
    list,
    call,
    program,

    fn is_sequence(self: Type) bool {
        return switch (self) {
            .program, .call, .list => true,
            else => false
        };
    }
};

etype: Type,
slice: []const u8,
children: ?[]Self = null,

pub fn init_sequence(tag: Type, slice: []const u8, children: []Self) Self {
    std.debug.assert(tag.is_sequence());
    return Self{
        .etype = tag,
        .slice = slice,
        .children = children
    };
}

pub fn init_slice(tag: Type, slice: []const u8) Self {
    std.debug.assert(!tag.is_sequence());
    return Self{
        .etype = tag,
        .slice = slice
    };
}

pub fn deinit(self: Self, ally: Allocator) void {
    if (self.etype.is_sequence()) {
        const children = self.children.?;
        for (children) |child| child.deinit(ally);

        ally.free(children);
    }
}

fn format_r(
    self: Self,
    level: i32,
    writer: anytype
) @TypeOf(writer).Error!void {
    switch (self.etype) {
        .program, .call, .list => |etype| {
            const children = self.children.?;

            try writer.writeAll(if (etype == .list) "[" else "(");
            if (children.len > 0) {
                try children[0].format_r(level + 1, writer);

                for (children[1..]) |*child| {
                    try writer.writeAll(" ");
                    try child.format_r(level + 1, writer);
                }
            }
            try writer.writeAll(if (etype == .list) "]" else ")");
        },
        .symbol, .string, .character, .number => {
            try writer.writeAll(self.slice);
        },
    }
}

pub fn format(
    self: Self,
    comptime fmt_str: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype
) @TypeOf(writer).Error!void {
    _ = fmt_str;
    _ = options;

    try self.format_r(0, writer);
}
