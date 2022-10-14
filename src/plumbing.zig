//! plumbing is where the pipelines are. lol
//! this is code connecting the frontend to the backend for use in main

const std = @import("std");
const builtin = @import("builtin");
const frontend = @import("frontend.zig");
const backend = @import("backend.zig");
const context = @import("context.zig");

const Allocator = std.mem.Allocator;
const Env = backend.Env;
const Value = backend.Value;

const stdout = std.io.getStdOut().writer();

pub fn execute(
    ally: Allocator,
    env: *Env,
    handle: context.FileHandle,
) !?Value {
    _ = env;

    // frontend
    const ast = frontend.parse(ally, handle) catch {
        try context.flushMessages();
        return null;
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

    return Value{ .unit = {} };

    // @panic("TODO");

    // lex + parse
    // const program = try ctx.wrap_stage(frontend.parse(&ctx, ally));
    // defer program.deinit(ally);

    // run
    // return try backend.run(ally, env, program);
}
