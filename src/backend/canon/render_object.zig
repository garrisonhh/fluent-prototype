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
    const slice_addr = canon.to(obj.val.buf[ptr_offset .. ptr_offset + 8]);

    const slice_ptr = @intToPtr([*]const u8, slice_addr);
    const slice_len = canon.to(obj.val.buf[len_offset .. len_offset + 8]);

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

    const elem = try Object.init(env, elem_ty);
    defer elem.deinit(env);

    const elem_sz = try env.sizeOf(elem_ty);

    var i: usize = 0;
    while (i < slice_len) : (i += 1) {
        // copy raw data to object
        const elem_offset = i * elem_sz;
        const elem_buf = slice_ptr[elem_offset .. elem_offset + elem_sz];
        std.mem.copy(u8, elem.val.buf, elem_buf);

        // render
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
        .@"bool" => try ctx.print(LIT_STY, "{}", .{obj.intoBool()}),
        .number => try ctx.print(LIT_STY, "{}", .{obj.intoNumber(env.*)}),
        .ty => try env.tw.get(obj.intoType()).render(ctx, env.tw),
        .ptr => |ptr| switch (ptr.kind) {
            .single, .many => addr: {
                const raw_ptr = obj.intoPtr(*anyopaque);
                break :addr try ctx.print(LIT_STY, "@{*}", .{raw_ptr});
            },
            .slice => try ty.render(ctx, env.tw),
        },

        .builtin => b: {
            const name = @tagName(obj.intoBuiltin());
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
            const tag = canon.to(obj.val.buf[tag_offset .. tag_offset + 8]);

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
