//! plumbing is where the pipelines are. lol
//! this is code connecting the frontend to the backend for use in main

const std = @import("std");
const stdout = std.io.getStdOut().writer();
const util = @import("util");
const FileRef = util.FileRef;
const Project = util.Project;
const frontend = @import("frontend.zig");
const backend = @import("backend.zig");
const Env = backend.Env;
const TExpr = backend.TExpr;
const eval = backend.eval;

/// loads a file and evaluate it
pub fn exec(
    proj: Project,
    env: *Env,
    file: FileRef,
    what: frontend.ParseType
) !eval.Result {
    const ally = env.ally;

    const now = std.time.nanoTimestamp;
    const start = now();
    var render_time: i128 = 0;

    // parse
    const parse_res = try frontend.parse(ally, proj, file, what);
    const ast = parse_res.get() orelse return parse_res.cast(TExpr);
    defer ast.deinit(ally);

    // translate
    const trans_res = try backend.translate(env.ally, proj, ast);
    const sexpr = trans_res.get() orelse return trans_res.cast(TExpr);
    defer sexpr.deinit(env.ally);

    if (util.options.log.translate) {
        const t = now();
        try stdout.print("[Translated AST]\n{}\n\n", .{sexpr});

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
    return try eval.eval(env, Env.ROOT, sexpr);
}
