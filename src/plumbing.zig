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
    // frontend
    const ast = frontend.parse(ally, handle) catch {
        try context.flushMessages();
        return;
    };
    defer ast.deinit(ally);

    {
        const tex = try ast.render(ally);
        defer tex.deinit(ally);

        try stdout.print("[Parsed AST]\n", .{});
        try tex.display(stdout);
        try stdout.writeByte('\n');
    }

    try context.flushMessages();

    // TODO connect to backend
}
