const std = @import("std");
const util = @import("../util/util.zig");
const FlType = @import("type.zig").FlType;

const Self = @This();

pub const Type = enum {
    call,
    file,
    list,
    ident,
    float,
    int,
    string,
    ltype,

    fn is_sequence(self: Type) bool {
        return switch (self) {
            .call, .file, .list => true,
            else => false
        };
    }
};

etype: Type,
slice: []const u8,
ltype: FlType = FlType{ .unknown = {} },
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

/// top-down traversal
pub fn traverse(
    self: *const Self,
    ctx: anytype,
    cb: fn(@TypeOf(ctx), *const Self) anyerror!void
) anyerror!void {
    try cb(ctx, self);

    if (self.children) |children| {
        for (children) |*child| try child.traverse(ctx, cb);
    }
}

const Fmt = struct {
    const Expr = Self;

    root: *const Expr,
    indent: i32,
    typed: bool,

    fn format_r(
        self: *const @This(),
        expr: *const Expr,
        level: i32,
        noindent: bool,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        // indent
        if (!noindent) {
            var i: usize = 0;
            while (i < level * self.indent) : (i += 1) {
                try writer.writeAll(" ");
            }
        }

        // data
        switch (expr.etype) {
            .call, .file, .list => |etype| {
                const children = expr.children.?;

                try writer.writeAll(if (etype == .list) "[" else "(");
                if (children.len > 0) {
                    try self.format_r(&children[0], level + 1, true, writer);

                    for (children[1..]) |*child| {
                        if (self.indent > 0) {
                            try writer.writeAll("\n");
                        } else {
                            try writer.writeAll(" ");
                        }

                        try self.format_r(child, level + 1, false, writer);
                    }
                }
                try writer.writeAll(if (etype == .list) "]" else ")");
            },
            .ident, .string, .int, .float => try writer.writeAll(expr.slice),
            else => @panic("TODO ???")
        }

        // types
        if (self.typed) try std.fmt.format(writer, " {}", .{expr.ltype});
    }

    pub fn format(
        self: *const @This(),
        comptime fmt_str: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        _ = fmt_str;
        _ = options;

        try format_r(self, self.root, 0, true, writer);
    }
};

const FmtArgs = struct {
    indent: i32 = 0,
    typed: bool = false,
};

pub fn fmt(self: *const Self, args: FmtArgs) Fmt {
    return Fmt{
        .root = self,
        .indent = args.indent,
        .typed = args.typed
    };
}