//! plumbing is where the pipelines are. lol
//! this is code connecting the frontend to the backend for use in main

const std = @import("std");
const stdout = std.io.getStdOut().writer();
const kz = @import("kritzler");
const util = @import("util");
const now = util.now;
const FileRef = util.FileRef;
const Project = util.Project;
const frontend = @import("frontend.zig");
const translate = @import("translate.zig").translate;
const backend = @import("backend.zig");
const Env = backend.Env;
const TExpr = backend.TExpr;
const eval = backend.eval;

/// loads a file and evaluate it
pub fn exec(
    proj: Project,
    env: *Env,
    file: FileRef,
    what: frontend.ParseType,
) !eval.Result {
    const ally = env.ally;

    const start = now();
    var render_time: f64 = 0;

    // parse
    const parse_res = try frontend.parse(ally, proj, file, what);
    const ast = parse_res.get() orelse return parse_res.cast(TExpr);
    defer ast.deinit(ally);

    // translate
    const trans_res = try translate(ally, proj, ast);
    const sexpr = trans_res.get() orelse return trans_res.cast(TExpr);
    defer sexpr.deinit(ally);

    if (util.options.log.translate) {
        const t = now();
        defer render_time += now() - t;

        try stdout.print("[Translated AST]\n", .{});
        try kz.display(ally, .{}, sexpr, stdout);
    }

    // time logging
    const time = now() - start;
    try stdout.print("frontend finished in {d:.6}ms.\n", .{time});

    if (render_time > 0) {
        try stdout.print("render time {d:.6}ms.\n", .{render_time});
    }

    // call backend
    return try eval.eval(env, Env.ROOT, sexpr);
}
