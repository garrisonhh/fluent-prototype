//! plumbing is where the pipelines are. lol
//! this is code connecting the frontend to the backend for use in main

const std = @import("std");
const builtin = @import("builtin");
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

    // parse stuff
    const ast = frontend.parse(ally, handle) catch {
        try context.flushMessages();
        return;
    };
    defer ast.deinit(ally);

    // backend
    const sexpr = try backend.translate(ally, ast);
    defer sexpr.deinit(ally);

    if (builtin.mode == .Debug) {
        const t = now();
        try stdout.print("[Translated AST]\n{}\n\n", .{sexpr});

        render_time += now() - t;
    }

    var local = Env.init(env);
    defer local.deinit();

    const any = try env.typeIdentify(backend.Type{ .any = {} });
    const texpr = try backend.analyze(&local, sexpr, any);
    defer texpr.deinit(ally);

    if (builtin.mode == .Debug) {
        const t = now();
        var arena = std.heap.ArenaAllocator.init(ally);
        defer arena.deinit();

        const tex = try texpr.render(local, arena.allocator());

        try stdout.writeAll("[Analyzed AST]\n");
        try tex.display(stdout);
        try stdout.writeByte('\n');

        render_time += now() - t;
    }

    var program = try backend.lower(ally, env, texpr);
    defer program.deinit(ally);

    if (builtin.mode == .Debug) {
        const t = now();
        var arena = std.heap.ArenaAllocator.init(ally);
        defer arena.deinit();

        const tex = try program.render(local, arena.allocator());

        try stdout.writeAll("[SSA Program]\n");
        try tex.display(stdout);
        try stdout.writeByte('\n');

        render_time += now() - t;
    }

    // time logging
    const stop = std.time.nanoTimestamp();
    const seconds = @intToFloat(f64, stop - start - render_time) * 1e-9;

    try stdout.print("execution finished in {d:.6}s.\n", .{seconds});
}
