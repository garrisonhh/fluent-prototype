const std = @import("std");
const Allocator = std.mem.Allocator;
const kz = @import("kritzler");
const Type = @import("type.zig").Type;
const TypeWelt = @import("typewelt.zig");
const TypeId = TypeWelt.TypeId;

const sty = kz.Style{ .fg = .blue };

fn renderTypeArray(
    ctx: *kz.Context,
    tw: TypeWelt,
    types: []const TypeId,
) Allocator.Error!kz.Ref {
    var elems = try ctx.stub();
    const comma = try ctx.print(.{}, ", ", .{});
    defer ctx.drop(comma);

    for (types) |id, i| {
        if (i > 0) {
            elems = try ctx.slap(elems, try ctx.clone(comma), .right, .{});
        }

        elems = try ctx.slap(elems, try id.render(ctx, tw), .right, .{});
    }

    return try ctx.stack(&.{
        try ctx.print(.{}, "{{", .{}),
        elems,
        try ctx.print(.{}, "}}", .{}),
    }, .right, .{});
}

pub fn renderType(
    self: Type,
    ctx: *kz.Context,
    tw: TypeWelt,
) Allocator.Error!kz.Ref {
    return switch (self) {
        // title case
        .any => title: {
            const name = @tagName(self);
            const first = std.ascii.toUpper(name[0]);

            break :title try ctx.print(sty, "{c}{s}", .{ first, name[1..] });
        },

        // lowercase
        .unit,
        .builtin,
        .bool,
        .hole,
        .name,
        => try ctx.print(sty, "{s}", .{@tagName(self)}),
        .ty => try ctx.print(sty, "type", .{}),

        .number => |num| num: {
            if (num.bits) |bits| {
                break :num try ctx.slap(
                    try ctx.print(sty, "{c}", .{@tagName(num.layout)[0]}),
                    try ctx.print(sty, "{d}", .{bits}),
                    .right,
                    .{},
                );
            }

            break :num try ctx.print(
                sty,
                "compiler_{s}",
                .{@tagName(num.layout)},
            );
        },
        .array => |arr| try ctx.stack(
            &.{
                try ctx.print(.{}, "[", .{}),
                try ctx.print(sty, "{d}", .{arr.size}),
                try ctx.print(.{}, "]", .{}),
                try arr.of.render(ctx, tw),
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
                try ctx.print(sty, "set", .{}),
                try renderTypeArray(ctx, tw, list.items),
                .right,
                .{ .space = 1 },
            );
        },
        .tuple => |tup| tup: {
            const cnt = tup.len;
            var list = try std.ArrayList(kz.Ref).initCapacity(ctx.ally, cnt);
            defer list.deinit();

            for (tup) |id| {
                try list.append(try id.render(ctx, tw));
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
        .@"struct", .variant  => |fields| coll: {
            const cnt = fields.len;
            var list = try std.ArrayList(kz.Ref).initCapacity(ctx.ally, cnt);
            defer list.deinit();

            for (fields) |field| {
                try list.append(try ctx.slap(
                    try ctx.print(.{}, "{}: ", .{field.name}),
                    try field.of.render(ctx, tw),
                    .right,
                    .{},
                ));
            }

            break :coll try ctx.stack(
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
        .func => |func| try ctx.stack(&.{
            try renderTypeArray(ctx, tw, func.takes),
            try ctx.print(.{}, "->", .{}),
            try func.returns.render(ctx, tw),
        }, .right, .{ .space = 1 }),
        .ptr => |ptr| ptr: {
            const pre: []const u8 = switch (ptr.kind) {
                .single => "*",
                .many => "[*]",
                .slice => "[]",
            };

            break :ptr try ctx.slap(
                try ctx.print(.{}, "{s}", .{pre}),
                try ptr.to.render(ctx, tw),
                .right,
                .{},
            );
        },
    };
}
