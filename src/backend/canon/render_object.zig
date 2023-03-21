const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");
const Symbol = com.Symbol;
const kz = @import("kritzler");
const Env = @import("../env.zig");
const canon = @import("../canon.zig");
const Object = canon.Object;
const Type = canon.Type;
const TypeId = canon.TypeId;
const Basic = canon.Basic;
const Ptr = canon.Ptr;

const Error = Object.InitError;

const LIT_STY = kz.Style{ .fg = .magenta };
const STR_STY = kz.Style{ .fg = .green };
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
    const obj = try Object.from(env, field.of, parent.ptr.add(offset));

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

fn renderSlice(
    obj: Object,
    elem_ty: TypeId,
    ctx: *kz.Context,
    env: *Env,
) Error!kz.Ref {
    // acquire slice ptr and len
    const repr_fields = env.rw.get(obj.repr).coll;
    const ptr_offset = repr_fields[0].offset;
    const len_offset = repr_fields[1].offset;

    const slice_addr = env.img.read(obj.ptr.add(ptr_offset), Ptr);
    const slice_len = env.img.read(obj.ptr.add(len_offset), u64);
    const slice_ptr = env.img.into(slice_addr, [*]const u8);

    // strings get special behavior
    str: {
        // must be []u8
        if (!elem_ty.eql(Basic.u8.get())) break :str;

        // all characters must be printable
        const str = slice_ptr[0..slice_len];
        for (str) |ch| {
            if (!std.ascii.isPrint(ch)) break :str;
        }

        // this is an acceptable string
        const escaped = try com.stringEscape(env.ally, str);
        defer env.ally.free(escaped);

        return try ctx.print(STR_STY, "\"{s}\"", .{escaped});
    }

    // render each element
    var elems = std.ArrayList(kz.Ref).init(env.ally);
    defer elems.deinit();

    const elem_sz = try env.sizeOf(elem_ty);

    var i: usize = 0;
    while (i < slice_len) : (i += 1) {
        const elem_offset = i * elem_sz;
        const elem = try Object.from(env, elem_ty, slice_addr.add(elem_offset));
        try elems.append(try elem.render(ctx, env));
    }

    // render elements together
    return try ctx.stack(elems.items, .bottom, .{});
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
        .@"bool" => try ctx.print(LIT_STY, "{}", .{obj.intoBool(env)}),
        .number => try ctx.print(LIT_STY, "{}", .{obj.intoNumber(env)}),
        .ty => try env.tw.get(obj.intoType(env)).render(ctx, env.tw),
        .ptr => |ptr| switch (ptr.kind) {
            .single, .many => addr: {
                const raw_ptr = obj.intoPtr(env, *u8);
                break :addr try ctx.print(LIT_STY, "@{*}", .{raw_ptr});
            },
            .slice => try ty.render(ctx, env.tw),
        },

        .builtin => b: {
            const name = @tagName(obj.intoBuiltin(env));
            break :b try ctx.print(LIT_STY, "{s}", .{name});
        },

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
            const tag = env.img.into(obj.ptr.add(tag_offset), *const u64).*;

            const offset = repr_fields[tag + 1].offset;

            const tex = try renderField(obj, fields[tag], offset, ctx, env);
            try children.append(tex);
        },
        .ptr => |ptr| switch (ptr.kind) {
            .single, .many => {},
            .slice => {
                try children.append(try renderSlice(obj, ptr.to, ctx, env));
            },
        },
        else => {},
    }

    const body = try ctx.stack(children.items, .bottom, .{});

    // put it together
    if (ctx.getSize(body)[1] > 1) {
        return try ctx.unify(header, body, .{ INDENT, 1 });
    }

    return try ctx.slap(header, body, .right, .{ .space = 1 });
}
