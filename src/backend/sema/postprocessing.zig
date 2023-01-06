//! extra sema passes that happen after the main type analysis routine

const std = @import("std");
const Env = @import("../env.zig");
const TExpr = @import("../texpr.zig");
const context = @import("../../context.zig");

const Error = context.MessageError || context.FluentError;

/// flattens `do` blocks with one expression into the expression
fn pruneDoBlocks(env: Env, texpr: *TExpr) void {
    for (texpr.getChildren()) |*child| {
        pruneDoBlocks(env, child);
    }

    if (texpr.data == .call and texpr.data.call[0].isBuiltin(.do)) {
        const xs = texpr.data.call;
        if (xs.len == 2) {
            const child = xs[1];
            env.ally.free(xs);

            texpr.* = child;
        }
    }
}

/// after analysis, it's possible that types that fluent can't actually lower
/// are produced (e.g. Any and common type sets like Int).
fn verifyDynamic(env: Env, texpr: TExpr) Error!void {
    // check that the class isn't analysis
    const class = env.tw.get(texpr.ty).classifyRuntime(env.tw);

    if (class == .analysis) {
        const ty_text = try texpr.ty.writeAlloc(env.ally, env.tw);
        defer env.ally.free(ty_text);

        const text = "inferred type `{s}`, which cannot be executed";
        _ = try context.post(.err, texpr.loc, text, .{ty_text});

        return error.FluentError;
    }

    for (texpr.getChildren()) |child| {
        try verifyDynamic(env, child);
    }
}

/// when you cast a number literal immediately, there's no reason to execute
/// the cast on a vm to get the output
fn removeTrivialCasts(env: Env, texpr: *TExpr) void {
    blk: {
        // must be a call
        if (texpr.data != .call) break :blk;

        const exprs = texpr.data.call;
        if (exprs.len == 0) break :blk;

        // must be a call to cast
        const head = exprs[0];
        if (head.data != .builtin or head.data.builtin != .cast) {
            break :blk;
        }

        // must be a call to cast of a number to another number
        const body = &exprs[1];
        if (body.data != .number) break :blk;

        const ty = env.tw.get(texpr.ty);
        std.debug.assert(ty.* == .number);

        // perform cast and replace texpr
        const num = ty.number;
        const casted = body.data.number.cast(num.bits, num.layout);

        const original = texpr.*;
        defer original.deinit(env.ally);

        texpr.* = TExpr.init(original.loc, false, original.ty, .{
            .number = casted
        });
    }

    for (texpr.getChildren()) |*child| {
        removeTrivialCasts(env, child);
    }
}

pub fn postprocess(env: Env, texpr: *TExpr) Error!void {
    pruneDoBlocks(env, texpr);
    try verifyDynamic(env, texpr.*);
    removeTrivialCasts(env, texpr);
}