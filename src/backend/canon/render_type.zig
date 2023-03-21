const std = @import("std");
const Allocator = std.mem.Allocator;
const kz = @import("kritzler");
const Type = @import("type.zig").Type;
const TypeWelt = @import("typewelt.zig");
const TypeId = TypeWelt.TypeId;

const STY = kz.Style{ .fg = .blue };

const Selves = std.AutoHashMapUnmanaged(TypeId, void);

fn renderId(
    ctx: *kz.Context,
    selves: *Selves,
    tw: TypeWelt,
    id: TypeId,
) Allocator.Error!kz.Ref {
    return renderTypeAdvanced(tw.get(id), ctx, selves, tw);
}

fn renderTypeArray(
    ctx: *kz.Context,
    selves: *Selves,
    tw: TypeWelt,
    types: []const TypeId,
) Allocator.Error!kz.Ref {
    var list = std.ArrayList(kz.Ref).init(ctx.ally);
    defer list.deinit();

    for (types) |id| {
        try list.append(try renderId(ctx, selves, tw, id));
    }

    const comma = try ctx.print(.{}, ", ", .{});
    return try ctx.stack(&.{
        try ctx.print(.{}, "{{", .{}),
        try ctx.sep(comma, list.items, .right, .{}),
        try ctx.print(.{}, "}}", .{}),
    }, .right, .{});
}

fn renderTypeAdvanced(
    self: *const Type,
    ctx: *kz.Context,
    selves: *Selves,
    tw: TypeWelt,
) Allocator.Error!kz.Ref {
    return switch (self.*) {
        // title case
        .any => title: {
            const name = @tagName(self.*);
            const first = std.ascii.toUpper(name[0]);

            break :title try ctx.print(
                STY,
                "{c}{s}",
                .{ first, name[1..] },
            );
        },

        // lowercase
        .unit,
        .builtin,
        .bool,
        .hole,
        .name,
        => try ctx.print(STY, "{s}", .{@tagName(self.*)}),
        .ty => try ctx.print(STY, "type", .{}),

        .number => |num| num: {
            if (num.bits) |bits| {
                break :num try ctx.slap(
                    try ctx.print(STY, "{c}", .{@tagName(num.layout)[0]}),
                    try ctx.print(STY, "{d}", .{bits}),
                    .right,
                    .{},
                );
            }

            break :num try ctx.print(
                STY,
                "compiler_{s}",
                .{@tagName(num.layout)},
            );
        },
        .array => |arr| try ctx.stack(
            &.{
                try ctx.print(.{}, "[", .{}),
                try ctx.print(STY, "{d}", .{arr.size}),
                try ctx.print(.{}, "]", .{}),
                try renderId(ctx, selves, tw, arr.of),
            },
            .right,
            .{},
        ),
        .set => |set| set: {
            const cnt = set.count();
            var list = try std.ArrayList(TypeId).initCapacity(ctx.ally, cnt);
            defer list.deinit();

            var ids = set.keyIterator();
            while (ids.next()) |id| {
                list.appendAssumeCapacity(id.*);
            }

            break :set try ctx.slap(
                try ctx.print(STY, "set", .{}),
                try renderTypeArray(ctx, selves, tw, list.items),
                .right,
                .{ .space = 1 },
            );
        },
        .tuple => |tup| tup: {
            const cnt = tup.len;
            var list = try std.ArrayList(kz.Ref).initCapacity(ctx.ally, cnt);
            defer list.deinit();

            for (tup) |id| {
                try list.append(try renderId(ctx, selves, tw, id));
            }

            break :tup try ctx.stack(
                &.{
                    try ctx.print(.{}, "(", .{}),
                    try ctx.sep(
                        try ctx.print(.{}, ", ", .{}),
                        list.items,
                        .right,
                        .{},
                    ),
                    try ctx.print(.{}, ")", .{}),
                },
                .right,
                .{},
            );
        },
        .@"struct", .variant => |fields| coll: {
            // check if this is a self
            const id = tw.retrieve(self);
            if (selves.contains(id)) {
                if (tw.getName(id)) |name| {
                    break :coll try ctx.print(STY, "{}", .{name});
                }

                break :coll id.render(ctx, {});
            }

            // not a self
            try selves.put(ctx.ally, id, {});
            defer _ = selves.remove(id);

            const cnt = fields.len;
            var list = try std.ArrayList(kz.Ref).initCapacity(ctx.ally, cnt);
            defer list.deinit();

            for (fields) |field| {
                try list.append(try ctx.slap(
                    try ctx.print(.{}, "{}: ", .{field.name}),
                    try renderId(ctx, selves, tw, field.of),
                    .right,
                    .{},
                ));
            }

            // put it together
            const header = try ctx.slap(
                try ctx.print(STY, "{s}", .{@tagName(self.*)}),
                try ctx.print(.{}, " {{", .{}),
                .right,
                .{},
            );
            const footer = try ctx.print(.{}, "}}", .{});

            break :coll switch (cnt) {
                0 => try ctx.slap(header, footer, .right, .{}),
                1 => try ctx.stack(&.{
                    header,
                    list.items[0],
                    footer,
                }, .right, .{}),
                else => many: {
                    const indent = try ctx.print(.{}, "  ", .{});
                    defer ctx.drop(indent);

                    for (list.items) |tex, i| {
                        list.items[i] = try ctx.slap(
                            tex,
                            try ctx.clone(indent),
                            .left,
                            .{},
                        );
                    }

                    break :many try ctx.stack(&.{
                        header,
                        try ctx.stack(list.items, .bottom, .{}),
                        footer,
                    }, .bottom, .{});
                },
            };
        },
        .func => |func| try ctx.stack(&.{
            try renderTypeArray(ctx, selves, tw, func.takes),
            try ctx.print(.{}, "->", .{}),
            try renderId(ctx, selves, tw, func.returns),
        }, .right, .{ .space = 1 }),
        .ptr => |ptr| ptr: {
            const pre: []const u8 = switch (ptr.kind) {
                .single => "*",
                .many => "[*]",
                .slice => "[]",
            };

            break :ptr try ctx.slap(
                try ctx.print(.{}, "{s}", .{pre}),
                try renderId(ctx, selves, tw, ptr.to),
                .right,
                .{},
            );
        },
    };
}

pub fn renderType(
    self: *const Type,
    ctx: *kz.Context,
    tw: TypeWelt,
) Allocator.Error!kz.Ref {
    var selves = Selves{};
    defer selves.deinit(ctx.ally);

    return renderTypeAdvanced(self, ctx, &selves, tw);
}
