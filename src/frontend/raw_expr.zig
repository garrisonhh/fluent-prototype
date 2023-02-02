const std = @import("std");
const Allocator = std.mem.Allocator;
const kz = @import("kritzler");
const com = @import("common");
const Loc = com.Loc;
const Project = com.Project;
const auto = @import("auto.zig");
const Form = auto.Form;

const Self = @This();

loc: Loc,
form: Form,
exprs: []Self = &.{},

/// create an expr by shallow cloning slice
pub fn init(
    ally: Allocator,
    loc: Loc,
    form: Form,
    exprs: []const Self,
) Allocator.Error!Self {
    return Self{
        .loc = loc,
        .form = form,
        .exprs = try ally.dupe(Self, exprs),
    };
}

pub fn deinit(self: Self, ally: Allocator) void {
    for (self.exprs) |child| child.deinit(ally);
    ally.free(self.exprs);
}

pub fn render(
    self: Self,
    ctx: *kz.Context,
    proj: Project,
) Allocator.Error!kz.Ref {
    const INDENT = 2;
    const head_sty = kz.Style{};
    const arm_sty = kz.Style{ .special = .faint };

    if (self.exprs.len == 0) lit: {
        // literals
        const color: kz.Color = switch (self.form) {
            .unit => .white,
            .number => .magenta,
            .symbol => .red,
            .string => .green,
            else => break :lit,
        };

        const slice = self.loc.slice(proj);
        return try ctx.print(.{ .fg = color }, "{s}", .{slice});
    }

    const head = try ctx.print(head_sty, "{s}", .{@tagName(self.form)});

    // indented body
    var children = std.ArrayList(kz.Ref).init(ctx.ally);
    defer children.deinit();

    for (self.exprs) |expr, i| {
        const child = try expr.render(ctx, proj);

        var arm = try ctx.block(.{ INDENT - 1, 1 }, arm_sty, ' ');

        const star = try ctx.block(.{ 1, 1 }, arm_sty, '-');
        arm = try ctx.slap(arm, star, .left, .{});

        if (i < self.exprs.len - 1) {
            const height = ctx.getSize(child)[1];
            const bar = try ctx.block(.{ 1, height - 1 }, arm_sty, '|');
            arm = try ctx.slap(bar, arm, .top, .{});
        }

        try children.append(try ctx.slap(child, arm, .left, .{}));
    }

    const body = try ctx.stack(children.items, .bottom, .{});

    return try ctx.slap(head, body, .bottom, .{});
}
