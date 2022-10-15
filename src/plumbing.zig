//! plumbing is where the pipelines are. lol
//! this is code connecting the frontend to the backend for use in main

const std = @import("std");
const builtin = @import("builtin");
const frontend = @import("frontend.zig");
const backend = @import("backend.zig");
const context = @import("context.zig");

const Allocator = std.mem.Allocator;

const stdout = std.io.getStdOut().writer();

pub fn execute(ally: Allocator, handle: context.FileHandle) !void {
    const start = std.time.nanoTimestamp();

    // parse stuff
    const ast = frontend.parse(ally, handle) catch {
        try context.flushMessages();
        return;
    };
    defer ast.deinit(ally);

    if (builtin.mode == .Debug) {
        var arena = std.heap.ArenaAllocator.init(ally);
        defer arena.deinit();

        const tex = try ast.render(arena.allocator());

        try stdout.print("[Parsed AST]\n", .{});
        try tex.display(stdout);
        try stdout.writeByte('\n');
    }

    try context.flushMessages();

    // backend
    const sexpr = try backend.translate(ally, ast);
    defer sexpr.deinit(ally);

    if (builtin.mode == .Debug) {
        try stdout.print("[Translated AST]\n{}\n\n", .{sexpr});
    }

    var global = try backend.Env.initGlobal(ally);
    defer global.deinit();

    const any = backend.Pattern{ .any = {} };
    const texpr = try backend.analyze(&global, sexpr, any);
    defer texpr.deinit(ally);

    if (builtin.mode == .Debug) {
        try stdout.print("[Analyzed AST]\n{}\n\n", .{texpr});
    }

    // time logging
    const stop = std.time.nanoTimestamp();
    const seconds = @intToFloat(f64, stop - start) * 1e-9;

    try stdout.print("execution finished in {d:.6}s.\n", .{seconds});
}
