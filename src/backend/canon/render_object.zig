const std = @import("std");
const Allocator = std.mem.Allocator;
const kz = @import("kritzler");
const Object = @import("object.zig");
const Env = @import("../env.zig");

pub fn render(
    obj: Object,
    ctx: *kz.Context,
    env: Env,
) Allocator.Error!kz.Ref {
    const lit = kz.Style{ .fg = .magenta };

    var children = std.ArrayList(kz.Ref).init(ctx.ally);
    defer children.deinit();

    const ty = env.tw.get(obj.ty);

    const header = switch (ty.*) {
        .unit => try ctx.print(.{}, "()", .{}),
        .@"bool" => try ctx.print(lit, "{}", .{obj.intoBool()}),
        .number => try ctx.print(lit, "{}", .{obj.intoNumber(env)}),
        .builtin => try ctx.stack(
            &.{
                try ctx.print(.{}, "<", .{}),
                try obj.ty.render(ctx, env.tw),
                try ctx.print(.{}, "> ", .{}),
                try ctx.print(lit, "{s}", .{@tagName(obj.intoBuiltin())}),
            },
            .right,
            .{},
        ),
        .ty => try obj.intoType().render(ctx, env.tw),
        else => |tag| try ctx.print(
            .{},
            "[TODO `{s}` object]",
            .{@tagName(tag)},
        ),
    };

    const body = try ctx.stack(children.items, .bottom, .{});
    return try ctx.unify(header, body, .{ 2, 1 });
}
