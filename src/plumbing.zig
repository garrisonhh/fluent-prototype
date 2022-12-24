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
const eval = backend.eval;
const context = @import("context.zig");
const FileHandle = context.FileHandle;

const stdout = std.io.getStdOut().writer();

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
