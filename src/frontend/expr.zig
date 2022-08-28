const std = @import("std");
const util = @import("../util/util.zig");

/// Expr is the most basic AST representation
const Self = @This();

pub const Type = enum {
    nil,
    int,
    float,
    string,

    ident,

    list,
    call,
    file,

    fn is_sequence(self: Type) bool {
        return switch (self) {
            .call, .file, .list => true,
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

/// TODO only added this to make hacky stuff easier, worth removing as soon as
/// those hacks are removed
pub fn is_ident(self: Self, comptime ident: []const u8) bool {
    return self.etype == .ident and std.mem.eql(u8, self.slice, ident);
}

const Fmt = struct {
    const Expr = Self;

    root: *const Expr,
    indent: i32,
    typing: FmtArgs.TypeLevel,

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

        // types
        if (self.typing == .all
         or self.typing == .functions and expr == .call) {
            try writer.print("<{}> ", .{expr.ltype});
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
    const TypeLevel = enum {
        none,
        functions,
        all,
    };

    typing: TypeLevel = .none,
    indent: i32 = 0,
};

pub fn fmt(self: *const Self, args: FmtArgs) Fmt {
    return Fmt{
        .root = self,
        .indent = args.indent,
        .typing = args.typing
    };
}