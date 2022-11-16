//! plumbing is where the pipelines are. lol
//! this is code connecting the frontend to the backend for use in main

const std = @import("std");
const builtin = @import("builtin");
const kz = @import("kritzler");
const frontend = @import("frontend.zig");
const backend = @import("backend.zig");
const context = @import("context.zig");

const Allocator = std.mem.Allocator;
const Env = backend.Env;

const stdout = std.io.getStdOut().writer();

pub fn execute(ally: Allocator, env: *Env, handle: context.FileHandle) !void {
    const now = std.time.nanoTimestamp;
    const start = now();
    var render_time: i128 = 0;

    // parse
    const ast = frontend.parse(ally, handle) catch {
        try context.flushMessages();
        return;
    };
    defer ast.deinit(ally);

    // translate
    const sexpr = try backend.translate(ally, ast);
    defer sexpr.deinit(ally);

    if (builtin.mode == .Debug) {
        const t = now();
        try stdout.print("[Translated AST]\n{}\n\n", .{sexpr});

        render_time += now() - t;
    }

    // analyze
    var local = Env.init(env);
    defer local.deinit();

    const any = try env.typeIdentify(backend.Type{ .any = {} });
    const texpr = try backend.analyze(&local, sexpr, any);
    defer texpr.deinit(ally);

    if (builtin.mode == .Debug) {
        const t = now();
        var ctx = kz.Context.init(ally);
        defer ctx.deinit();

        const tex = try texpr.render(&ctx, local);

        try stdout.writeAll("[Analyzed AST]\n");
        try ctx.write(tex, stdout);
        try stdout.writeByte('\n');

        render_time += now() - t;
    }

    // lower to ssa ir
    var ssa = try backend.lower(ally, env, texpr);
    defer ssa.deinit(ally);

    if (builtin.mode == .Debug) {
        const t = now();
        var ctx = kz.Context.init(ally);
        defer ctx.deinit();

        const tex = try ssa.render(&ctx, local);

        try stdout.writeAll("[SSA Program]\n");
        try ctx.write(tex, stdout);
        try stdout.writeByte('\n');

        render_time += now() - t;
    }

    // compile to bytecode
    const bc = try backend.compile(ally, env.typewelt, ssa);
    defer bc.deinit(ally);

    if (builtin.mode == .Debug) {
        const t = now();
        var ctx = kz.Context.init(ally);
        defer ctx.deinit();

        const tex = try bc.render(&ctx);

        try stdout.writeAll("[Bytecode]\n");
        try ctx.write(tex, stdout);
        try stdout.writeByte('\n');

        render_time += now() - t;
    }

    // run compiled bytecode
    const final_ty = bc.returns;
    const final = try backend.run(ally, env.typewelt, bc);
    defer final.deinit(ally);

    // render final value
    {
        const t = now();
        const revived = try final.revive(ally, env.*, final_ty);
        defer revived.deinit(ally);

        var ctx = kz.Context.init(ally);
        defer ctx.deinit();

        const tex = try revived.render(&ctx, local);

        try stdout.writeAll("[Value]\n");
        try ctx.write(tex, stdout);
        try stdout.writeByte('\n');

        render_time += now() - t;
    }

    // time logging
    const stop = std.time.nanoTimestamp();
    const seconds = @intToFloat(f64, stop - start - render_time) * 1e-9;

    try stdout.print("execution finished in {d:.6}s.\n", .{seconds});
    if (render_time > 0) {
        const render_secs = @intToFloat(f64, render_time) * 1e-9;
        try stdout.print("render time {d:.6}s.\n", .{render_secs});
    }
}
