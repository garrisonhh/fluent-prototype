const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");
const Symbol = com.Symbol;
const kz = @import("kritzler");
const Env = @import("../env.zig");
const canon = @import("../canon.zig");
const Object = canon.Object;
const Type = canon.Type;

const Error = Object.InitError;

const LIT_STY = kz.Style{ .fg = .magenta };
const SYNTAX_STY = kz.Style{ .fg = .blue };
const IDENT_STY = kz.Style{ .fg = .red };
const INDENT = 2;

fn renderField(
    parent: Object,
    field: Type.Field,
    offset: usize,
    ctx: *kz.Context,
    env: *Env,
) Error!kz.Ref {
    // grab obj
    const obj = try Object.init(env, field.of);
    defer obj.deinit(env);

    const sz = obj.val.buf.len;
    const slice = parent.val.buf[offset .. offset + sz];
    std.mem.copy(u8, obj.val.buf, slice);

    // render as a field
    const name = try ctx.slap(
        try ctx.print(IDENT_STY, "{}", .{field.name}),
        try ctx.print(.{}, ": ", .{}),
        .right,
        .{},
    );

    const data = try obj.render(ctx, env);

    if (ctx.getSize(data)[1] > 1) {
        return try ctx.unify(name, data, .{ INDENT, 1 });
    }

    return try ctx.slap(name, data, .right, .{});
}

pub fn render(
    obj: Object,
    ctx: *kz.Context,
    env: *Env,
) Error!kz.Ref {
    // header
    const ty = env.tw.get(obj.ty);
    const header = switch (ty.*) {
        .unit => try ctx.print(.{}, "()", .{}),
        .@"bool" => try ctx.print(LIT_STY, "{}", .{obj.intoBool()}),
        .number => try ctx.print(LIT_STY, "{}", .{obj.intoNumber(env.*)}),
        .builtin => b: {
            const name = @tagName(obj.intoBuiltin());
            break :b try ctx.print(LIT_STY, "{s}", .{name});
        },
        .ty => try obj.intoType().render(ctx, env.tw),
        .@"struct",
        .variant,
        => try ctx.print(
            SYNTAX_STY,
            "{s}",
            .{@tagName(ty.*)},
        ),
        else => try ctx.print(.{}, "[TODO {s} object]", .{@tagName(ty.*)}),
    };

    // body from children
    var children = std.ArrayList(kz.Ref).init(ctx.ally);
    defer children.deinit();

    switch (ty.*) {
        .@"struct" => |fields| {
            const repr_fields = env.rw.get(obj.repr).coll;
            for (fields) |field, i| {
                const offset = repr_fields[i].offset;
                const tex = try renderField(obj, field, offset, ctx, env);
                try children.append(tex);
            }
        },
        .variant => |fields| {
            const repr_fields = env.rw.get(obj.repr).coll;

            const tag_offset = repr_fields[0].offset;
            const tag = canon.to(obj.val.buf[tag_offset .. tag_offset + 8]);

            const offset = repr_fields[tag + 1].offset;

            const tex = try renderField(obj, fields[tag], offset, ctx, env);
            try children.append(tex);
        },
        else => {},
    }

    const body = try ctx.stack(children.items, .bottom, .{});

    // put it together
    return try ctx.unify(header, body, .{ INDENT, 1 });
}
