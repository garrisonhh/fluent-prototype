const std = @import("std");
const Allocator = std.mem.Allocator;
const kz = @import("kritzler");
const SExpr = @import("sexpr.zig");

pub fn format(
    self: SExpr,
    comptime _: []const u8,
    _: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    switch (self.data) {
        .number => |num| try writer.print("{}", .{num}),
        .string => |sym| try writer.print("\"{s}\"", .{sym.str}),
        .symbol => |sym| try writer.print("{s}", .{sym.str}),
        .call => |exprs| {
            try writer.writeByte('(');
            for (exprs) |expr, i| {
                if (i > 0) try writer.writeByte(' ');
                try writer.print("{}", .{expr});
            }
            try writer.writeByte(')');
        },
    }
}

pub const RenderOptions = struct {
    indented: bool = true,
    force_parens: bool = false,
};

pub fn render(
    self: SExpr,
    ctx: *kz.Context,
    env: RenderOptions,
) Allocator.Error!kz.Ref {
    const INDENT = 2;

    const magenta = kz.Style{ .fg = .magenta };
    const green = kz.Style{ .fg = .green };

    return switch (self.data) {
        .number => |num| try ctx.print(magenta, "{}", .{num}),
        .string => |sym| try ctx.print(green, "\"{s}\"", .{sym.str}),
        .symbol => |sym| try ctx.print(.{}, "{s}", .{sym.str}),
        .call => |exprs| {
            // unit special case
            if (exprs.len == 0) {
                return try ctx.print(.{}, "()", .{});
            }

            const indented =
                env.indented and exprs.len > 1 and for (exprs) |child|
            {
                if (child.data == .call) {
                    break true;
                }
            } else false;

            // render children
            var ref = try exprs[0].render(ctx, RenderOptions{
                .indented = false,
                .force_parens = true,
            });

            if (indented) {
                for (exprs[1..]) |child| {
                    const height = @intCast(isize, ctx.getSize(ref)[1]);
                    const child_ref = try child.render(ctx, env);

                    ref = try ctx.unify(ref, child_ref, .{ INDENT, height });
                }
            } else {
                for (exprs[1..]) |child| {
                    const child_ref = try child.render(ctx, env);
                    ref = try ctx.slap(ref, child_ref, .right, .{ .space = 1 });
                }
            }

            // parentheses
            if (env.force_parens or !env.indented) {
                const lparen = try ctx.print(.{}, "(", .{});
                const rparen = try ctx.print(.{}, ")", .{});

                ref = try ctx.slap(ref, lparen, .left, .{});
                ref = try ctx.slap(ref, rparen, .right, .{ .aln = .far });
            }

            return ref;
        },
    };
}
