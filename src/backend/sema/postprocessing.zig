//! extra sema passes that happen after the main type analysis routine

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const Env = @import("../env.zig");
const Id = Env.Id;
const TExpr = @import("../texpr.zig");
const com = @import("common");
const Loc = com.Loc;
const Message = com.Message;

const Result = Message.Result(void);

fn err(
    ally: Allocator,
    loc: ?Loc,
    comptime fmt: []const u8,
    args: anytype,
) Allocator.Error!Result {
    return Result.err(try Message.print(ally, .@"error", loc, fmt, args));
}

/// flattens `do` blocks with one expression into the expression
fn pruneDoBlocks(env: *Env, id: Id) void {
    const texpr = env.get(id);
    for (texpr.getChildren()) |child| {
        pruneDoBlocks(env, child);
    }

    // look for trivial do blocks
    if (texpr.data != .call) return;

    const xs = texpr.data.call;
    const head = env.get(xs[0]);
    if (!head.isBuiltin(.do)) return;

    if (xs.len == 2) {
        env.squash(id, xs[1]);
    }
}

/// after analysis, it's possible that types that fluent can't actually lower
/// are produced (e.g. Any and com type sets like Int).
fn verifyDynamic(env: Env, id: Id) Allocator.Error!Result {
    const texpr = env.get(id);

    // check that the class isn't analysis
    const class = env.tw.get(texpr.ty).classifyRuntime(env.tw);

    if (class == .analysis) {
        const ty_text = try texpr.ty.toString(env.ally, env.tw);
        defer env.ally.free(ty_text);

        return try err(
            env.ally,
            texpr.loc,
            "inferred type `{s}`, which cannot be executed",
            .{ty_text},
        );
    }

    for (texpr.getChildren()) |child| {
        const res = try verifyDynamic(env, child);
        res.get() orelse return res;
    }

    return Result.ok({});
}

/// when you cast a number literal immediately, there's no reason to execute
/// the cast on a vm to get the output
fn removeTrivialCasts(env: *Env, id: Id) Allocator.Error!void {
    const texpr = env.get(id);
    for (texpr.getChildren()) |child| {
        try removeTrivialCasts(env, child);
    }

    blk: {
        // must be a call
        if (texpr.data != .call) break :blk;

        const exprs = texpr.data.call;
        if (exprs.len == 0) break :blk;

        // must be a call to cast
        const head = env.get(exprs[0]);
        if (head.data != .builtin or head.data.builtin != .cast) {
            break :blk;
        }

        // must be a call to cast of a number to another number
        const body = env.get(exprs[1]);
        if (body.data != .number) break :blk;

        const ty = env.tw.get(texpr.ty);
        std.debug.assert(ty.* == .number);

        // perform cast and replace texpr
        const num = ty.number;
        const casted = body.data.number.cast(num.bits, num.layout);

        const final = try env.new(texpr.loc, texpr.ty, .{ .number = casted });
        env.squash(id, final);
    }
}

fn identifyLeftovers(env: Env, id: Id) Allocator.Error!Result {
    const texpr = env.get(id);

    if (texpr.isBuiltin(.pie_stone)) {
        const msg = "found pie stone in expr after sema.";
        return try err(env.ally, texpr.loc, msg, .{});
    }

    for (texpr.getChildren()) |child| {
        const res = try identifyLeftovers(env, child);
        res.get() orelse return res;
    }

    return Result.ok({});
}

pub fn postprocess(env: *Env, id: Id) Allocator.Error!Result {
    // TODO remove
    {
        const kz = @import("kritzler");
        const stdout = std.io.getStdOut().writer();

        stdout.writeAll("[before postprocessing]\n") catch {};
        kz.display(env.ally, env.*, env.get(id), stdout) catch {};
        stdout.writeAll("\n") catch {};
    }

    pruneDoBlocks(env, id);

    const vd_res = try verifyDynamic(env.*, id);
    vd_res.get() orelse return vd_res;

    try removeTrivialCasts(env, id);

    // compiler self-diagnostics
    if (builtin.mode == .Debug) {
        const il_res = try identifyLeftovers(env.*, id);
        il_res.get() orelse return il_res;
    }

    return Result.ok({});
}
