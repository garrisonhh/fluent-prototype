//! plumbing is where the pipelines are. lol
//! this is code connecting the frontend to the backend for use in main

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const kz = @import("kritzler");
const util = @import("util");
const Name = util.Name;
const frontend = @import("frontend.zig");
const backend = @import("backend.zig");
const Env = backend.Env;
const TExpr = backend.TExpr;
const context = @import("context.zig");
const FileHandle = context.FileHandle;

const stdout = std.io.getStdOut().writer();

/// evaluate in the provided scope and environment.
pub fn eval(env: *Env, expr: TExpr) !TExpr {
    const now = std.time.nanoTimestamp;
    const start = now();
    var render_time: i128 = 0;

    // lower to ssa ir
    var ssa = try backend.lower(env.ally, env, expr);
    defer ssa.deinit(env.ally);

    if (builtin.mode == .Debug) {
        const t = now();
        var ctx = kz.Context.init(env.ally);
        defer ctx.deinit();

        const tex = try ssa.render(&ctx, env);

        try stdout.writeAll("[SSA Program]\n");
        try ctx.write(tex, stdout);
        try stdout.writeByte('\n');

        render_time += now() - t;
    }

    // compile to bytecode
    const bc = try backend.compile(env.ally, env.tw, ssa);
    defer bc.deinit(env.ally);

    if (builtin.mode == .Debug) {
        const t = now();
        var ctx = kz.Context.init(env.ally);
        defer ctx.deinit();

        const tex = try bc.render(&ctx);

        try stdout.writeAll("[Bytecode]\n");
        try ctx.write(tex, stdout);
        try stdout.writeByte('\n');

        render_time += now() - t;
    }

    // run compiled bytecode
    const return_ty = bc.returns;
    const value = try backend.run(env.ally, &env.tw, bc);
    defer value.deinit(env.ally);

    const final = try value.resurrect(env.*, return_ty);

    // render final value
    {
        const t = now();

        var ctx = kz.Context.init(env.ally);
        defer ctx.deinit();

        const tex = try final.render(&ctx, env.tw);

        try stdout.writeAll("[Value]\n");
        try ctx.write(tex, stdout);
        try stdout.writeByte('\n');

        render_time += now() - t;
    }

    // time logging
    const stop = std.time.nanoTimestamp();
    const seconds = @intToFloat(f64, stop - start - render_time) * 1e-9;

    try stdout.print("backend execution finished in {d:.6}s.\n", .{seconds});
    if (render_time > 0) {
        const render_secs = @intToFloat(f64, render_time) * 1e-9;
        try stdout.print("render time {d:.6}s.\n", .{render_secs});
    }

    return final;
}

/// loads a file and evaluate it
pub fn exec(env: *Env, handle: FileHandle) !TExpr {
    const now = std.time.nanoTimestamp;
    const start = now();
    var render_time: i128 = 0;

    // parse
    const ast = frontend.parse(env.ally, handle) catch {
        try context.flushMessages();
        return error.FluentError;
    };
    defer ast.deinit(env.ally);

    // translate
    const sexpr = try backend.translate(env.ally, ast);
    defer sexpr.deinit(env.ally);

    if (builtin.mode == .Debug) {
        const t = now();
        try stdout.print("[Translated AST]\n{}\n\n", .{sexpr});

        render_time += now() - t;
    }

    // analyze
    const file_sym = util.Symbol.init(handle.getName());
    const scope = try env.defNamespace(Env.ROOT, file_sym);

    const any = try env.identify(backend.Type{ .any = {} });
    const texpr = try backend.analyze(env, scope, sexpr, any);
    defer texpr.deinit(env.ally);

    if (builtin.mode == .Debug) {
        const t = now();
        var ctx = kz.Context.init(env.ally);
        defer ctx.deinit();

        const tex = try texpr.render(&ctx, env.tw);

        try stdout.writeAll("[Analyzed AST]\n");
        try ctx.write(tex, stdout);
        try stdout.writeByte('\n');

        render_time += now() - t;
    }

    // time logging
    const stop = std.time.nanoTimestamp();
    const seconds = @intToFloat(f64, stop - start - render_time) * 1e-9;

    try stdout.print("frontend processing finished in {d:.6}s.\n", .{seconds});
    if (render_time > 0) {
        const render_secs = @intToFloat(f64, render_time) * 1e-9;
        try stdout.print("render time {d:.6}s.\n", .{render_secs});
    }

    // call backend
    return try eval(env, texpr);
}
