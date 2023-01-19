const std = @import("std");
const Allocator = std.mem.Allocator;
const kz = @import("kritzler");
const util = @import("util");
const Loc = util.Loc;
const Project = util.Project;
const auto = @import("auto.zig");

const Self = @This();

pub const Form = struct {
    kind: auto.Form,
    exprs: []Self,
};

pub const Data = union(enum) {
    number,
    string,
    symbol,
    group: []Self,
    form: Form,
};

loc: Loc,
data: Data,

/// create a group expr by shallow cloning slice
pub fn initGroup(
    ally: Allocator,
    loc: Loc,
    exprs: []const Self
) Allocator.Error!Self {
    return Self{
        .loc = loc,
        .data = .{ .group = try ally.dupe(Self, exprs) },
    };
}

/// create a form expr by shallow cloning slice
pub fn initForm(
    ally: Allocator,
    loc: Loc,
    kind: Form,
    exprs: []const Self
) Allocator.Error!Self {
    return Self{
        .loc = loc,
        .data = .{
            .form = .{
                .kind = kind,
                .exprs = try ally.dupe(Self, exprs),
            }
        },
    };
}

pub fn deinit(self: Self, ally: Allocator) void {
    switch (self.data) {
        .group => |group| {
            for (group) |child| child.deinit(ally);
            ally.free(group);
        },
        .form => |form| {
            for (form.exprs) |child| child.deinit(ally);
            ally.free(form.exprs);
        },
        else => {}
    }
}

pub fn render(
    self: Self,
    ctx: *kz.Context,
    proj: Project
) Allocator.Error!kz.Ref {
    const INDENT = 2;
    return switch (self.data) {
        .number, .string, .symbol => lit: {
            const color: kz.Color = switch (self.data) {
                .number => .magenta,
                .string => .green,
                .symbol => .red,
                else => unreachable
            };

            const slice = self.loc.slice(proj);
            break :lit try ctx.print(.{ .fg = color }, "{s}", .{slice});
        },
        .group => |exprs| group: {
            const faint = kz.Style{ .special = .faint };
            const head = try ctx.print(faint, "group", .{});

            var children = std.ArrayList(kz.Ref).init(ctx.ally);
            defer children.deinit();

            for (exprs) |expr| {
                try children.append(try expr.render(ctx, proj));
            }

            break :group try ctx.unify(
                head,
                try ctx.stack(children.items, .bottom, .{}),
                .{INDENT, 1},
            );
        },
        .form => |form| form: {
            const yellow = kz.Style{ .fg = .yellow };
            const name = @tagName(form.kind);
            const head = try ctx.print(yellow, "{s}", .{name});

            var children = std.ArrayList(kz.Ref).init(ctx.ally);
            defer children.deinit();

            for (form.exprs) |expr| {
                try children.append(try expr.render(ctx, proj));
            }

            break :form try ctx.unify(
                head,
                try ctx.stack(children.items, .bottom, .{}),
                .{INDENT, 1},
            );
        },
    };
}