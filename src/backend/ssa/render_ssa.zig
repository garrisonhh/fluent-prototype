const std = @import("std");
const kz = @import("kritzler");
const ssa = @import("ssa.zig");
const Const = ssa.Const;
const Local = ssa.Local;
const Label = ssa.Label;
const Op = ssa.Op;
const Pos = ssa.Pos;
const Func = ssa.Func;
const FuncRef = ssa.FuncRef;
const Prophecy = ssa.Prophecy;
const Program = ssa.Program;
const Env = @import("../env.zig");

// format ======================================================================

pub fn formatOp(
    self: Op,
    comptime _: []const u8,
    _: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    switch (self.classify()) {
        inline else => |data| {
            const T = @TypeOf(data);

            if (@hasField(T, "to")) {
                try writer.print("%{} = ", .{data.to.index});
            }

            try writer.writeAll(@tagName(self));

            inline for (@typeInfo(T).Struct.fields) |field| {
                if (comptime std.mem.eql(u8, field.name, "to")) {
                    continue;
                }

                const x = @field(data, field.name);
                switch (field.field_type) {
                    Const => try writer.print(" c{}", .{x.index}),
                    Local => try writer.print(" %{}", .{x.index}),
                    []Local => for (x) |el| {
                        try writer.print(" %{}", .{el.index});
                    },
                    Label => try writer.print(" @{}", .{x.index}),
                    []Label => for (x) |el| {
                        try writer.print(" @{}", .{el.index});
                    },
                    usize => try writer.print(" {}", .{x}),
                    else => |F| comptime {
                        const msg = std.fmt.comptimePrint(
                            "TODO print ssa op field type `{}`",
                            .{F},
                        );
                        @compileError(msg);
                    },
                }
            }
        },
    }
}

pub fn formatPos(
    self: Pos,
    comptime _: []const u8,
    _: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    try writer.print(
        "{}@{}:{}",
        .{ self.ref.index, self.block.index, self.index },
    );
}

// render ======================================================================

pub fn renderLocal(self: Local, ctx: *kz.Context, _: void) !kz.Ref {
    return try ctx.print(.{ .fg = .cyan }, "%{}", .{self.index});
}

pub fn renderLabel(self: Label, ctx: *kz.Context, _: void) !kz.Ref {
    return try ctx.print(.{ .fg = .cyan }, "@{}", .{self.index});
}

pub fn renderProphecy(self: Prophecy, ctx: *kz.Context, env: Env) !kz.Ref {
    const ally = ctx.ally;
    const func = env.getFuncConst(self.ref);

    // map positions to y values
    var map = std.AutoHashMap(Pos, usize).init(ally);
    defer map.deinit();

    var y: usize = 0;
    for (func.blocks.items) |block, i| {
        const label = Label.of(i);
        y += 1;
        for (block.ops.items) |_, j| {
            try map.put(Pos.of(func.ref, label, j), y);
            y += 1;
        }
    }

    // generate lifetime graph
    const bars = try ally.alloc(kz.Ref, self.map.len);
    defer ally.free(bars);

    for (self.map) |lt, i| {
        const index = map.get(lt.start).?;
        const len = map.get(lt.stop).? + 1 - index;

        // generate bar
        const stub = try ctx.block(.{ 1, y }, .{ .fg = .dark_gray }, '.');
        const color: kz.Color = if (i % 5 == 0) .blue else .green;
        const bar = try ctx.block(.{ 1, len }, .{ .bg = color }, ' ');

        bars[i] = try ctx.unify(stub, bar, .{ 0, @intCast(isize, index) });
    }

    return try ctx.stack(bars, .right, .{});
}

fn renderFuncLocal(
    self: Func,
    ctx: *kz.Context,
    env: Env,
    local: Local,
) !kz.Ref {
    const ty = self.locals.items[local.index];
    return try ctx.slap(
        try ty.render(ctx, env.tw),
        try local.render(ctx, {}),
        .right,
        .{ .space = 1 },
    );
}

fn renderFuncParams(
    self: Func,
    ctx: *kz.Context,
    env: Env,
    list: *std.ArrayList(kz.Ref),
    params: []const Local,
) !void {
    const comma = try ctx.print(.{}, ", ", .{});
    defer ctx.drop(comma);

    for (params) |param, i| {
        if (i > 0) try list.append(try ctx.clone(comma));
        try list.append(try renderFuncLocal(self, ctx, env, param));
    }
}

fn renderFuncLabels(
    ctx: *kz.Context,
    list: *std.ArrayList(kz.Ref),
    labels: []const Label,
) !void {
    const comma = try ctx.print(.{}, ", ", .{});
    defer ctx.drop(comma);

    for (labels) |label, i| {
        if (i > 0) try list.append(try ctx.clone(comma));
        try list.append(try label.render(ctx, {}));
    }
}

fn renderFuncOp(self: Func, ctx: *kz.Context, env: Env, op: Op) !kz.Ref {
    var line = std.ArrayList(kz.Ref).init(ctx.ally);
    defer line.deinit();

    // collect textures
    const tag = @tagName(op);
    const comma = try ctx.print(.{}, ", ", .{});
    const class = op.classify();
    switch (class) {
        .ldc => |ldc| {
            const expr = self.consts.items[ldc.a.index];
            try line.appendSlice(&.{
                try renderFuncLocal(self, ctx, env, ldc.to),
                try ctx.print(.{}, " = ", .{}),
                try expr.render(ctx, env),
            });
        },
        .call => |call| {
            try line.appendSlice(&.{
                try renderFuncLocal(self, ctx, env, call.to),
                try ctx.print(.{}, " = {s} ", .{tag}),
                try renderFuncLocal(self, ctx, env, call.func),
                try ctx.clone(comma),
            });

            try renderFuncParams(self, ctx, env, &line, call.params);
        },
        .phi => |phi| {
            try line.appendSlice(&.{
                try renderFuncLocal(self, ctx, env, phi.to),
                try ctx.print(.{}, " = {s} ", .{tag}),
            });

            try renderFuncLabels(ctx, &line, phi.from);
        },
        .branch => |br| {
            try line.appendSlice(&.{
                try ctx.print(.{}, "{s} ", .{tag}),
                try renderFuncLocal(self, ctx, env, br.cond),
                try ctx.clone(comma),
                try br.a.render(ctx, {}),
                try ctx.clone(comma),
                try br.b.render(ctx, {}),
            });
        },
        .jump => |jmp| {
            try line.appendSlice(&.{
                try ctx.print(.{}, "{s} ", .{tag}),
                try jmp.dst.render(ctx, {}),
                try ctx.print(.{}, " ", .{}),
                try renderFuncLocal(self, ctx, env, jmp.data),
            });
        },
        .alloca => |all| {
            try line.appendSlice(&.{
                try renderFuncLocal(self, ctx, env, all.to),
                try ctx.print(.{}, " = {s} {d}", .{ tag, all.size }),
            });
        },
        .pure => |pure| {
            try line.appendSlice(&.{
                try renderFuncLocal(self, ctx, env, pure.to),
                try ctx.print(.{}, " = {s} ", .{tag}),
            });

            try renderFuncParams(self, ctx, env, &line, pure.params);
        },
        .impure => |impure| {
            try line.append(try ctx.print(.{}, "{s} ", .{tag}));
            try renderFuncParams(self, ctx, env, &line, impure.params);
        },
    }

    // stack and return
    return ctx.stack(line.items, .right, .{});
}

fn renderFuncBlock(
    self: Func,
    ctx: *kz.Context,
    env: Env,
    label: Label,
) !kz.Ref {
    const INDENT = 4;

    // collect op refs
    var op_texs = std.ArrayList(kz.Ref).init(ctx.ally);
    defer op_texs.deinit();

    for (self.blocks.items[label.index].ops.items) |op| {
        try op_texs.append(try renderFuncOp(self, ctx, env, op));
    }

    // format block nicely
    const ops_tex = try ctx.stack(op_texs.items, .bottom, .{});
    const indent_tex = try ctx.blank(.{ INDENT, 0 });
    const body_tex = try ctx.slap(indent_tex, ops_tex, .right, .{});

    const label_tex = try label.render(ctx, {});
    return try ctx.slap(body_tex, label_tex, .top, .{});
}

pub fn renderFunc(self: Func, ctx: *kz.Context, env: Env) !kz.Ref {
    const red = kz.Style{ .fg = .red };

    // name
    var header_texs = std.ArrayList(kz.Ref).init(ctx.ally);
    defer header_texs.deinit();

    if (self.name.syms.len > 0) {
        try header_texs.append(try ctx.print(red, "{}", .{self.name}));
    } else {
        try header_texs.append(try ctx.print(red, "<expr>", .{}));
    }

    // parameters
    try header_texs.append(try ctx.print(.{}, " :: ", .{}));

    var param = Local.of(0);
    while (param.index < self.takes) : (param.index += 1) {
        if (param.index > 0) {
            try header_texs.append(try ctx.print(.{}, ", ", .{}));
        }

        try header_texs.append(try renderFuncLocal(self, ctx, env, param));
    }

    if (self.takes > 0) {
        try header_texs.append(try ctx.print(.{}, " -> ", .{}));
    }

    // return type
    try header_texs.append(try self.returns.render(ctx, env.tw));

    const header = try ctx.stack(header_texs.items, .right, .{});

    // body
    var block_texs = std.ArrayList(kz.Ref).init(ctx.ally);
    defer block_texs.deinit();

    for (self.blocks.items) |_, i| {
        const tex = try renderFuncBlock(self, ctx, env, Label.of(i));
        try block_texs.append(tex);
    }

    const body = try ctx.stack(block_texs.items, .bottom, .{});

    // lifetime graph
    var proph = try Prophecy.init(ctx.ally, &self);
    defer proph.deinit(ctx.ally);

    const proph_tex = try proph.render(ctx, env);

    // slap it together
    const code = try ctx.slap(header, body, .bottom, .{});
    const sz = kz.toOffset(ctx.getSize(code));
    return try ctx.unify(code, proph_tex, .{ sz[0] + 1, 1 });
}

pub fn renderProgram(self: Program, ctx: *kz.Context, env: Env) !kz.Ref {
    const ally = ctx.ally;

    // construct set of unused refs
    var unused = std.AutoHashMap(FuncRef, void).init(ctx.ally);
    defer unused.deinit();

    for (self.unused.items) |ref| {
        try unused.put(ref, {});
    }

    // render each ref if it's not unused
    var refs = std.ArrayList(kz.Ref).init(ally);
    defer refs.deinit();

    for (self.funcs.items) |func, i| {
        if (!unused.contains(FuncRef.of(i))) {
            try refs.append(try func.render(ctx, env));
        }
    }

    return ctx.stack(refs.items, .bottom, .{ .space = 1 });
}
