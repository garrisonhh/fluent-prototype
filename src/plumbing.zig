//! plumbing is where the pipelines are. lol
//! this is code connecting the frontend to the backend for use in main

const std = @import("std");
const builtin = @import("builtin");
const frontend = @import("frontend.zig");
const backend = @import("backend.zig");
const FlFile = @import("file.zig");

const Allocator = std.mem.Allocator;
const Env = backend.Env;
const Value = backend.Value;

pub fn evaluate(
    ally: Allocator,
    env: *Env,
    name: []const u8,
    text: []const u8
) !Value {
    // create context
    var lfile = try FlFile.init(ally, name, text);
    defer lfile.deinit(ally);

    var ctx = lfile.context(ally);
    defer ctx.deinit();

    // lex + parse
    const exprs = try ctx.wrap_stage(frontend.parse(&ctx, ally));
    defer {
        for (exprs) |expr| expr.deinit(ally);
        ally.free(exprs);
    }

    // run
    return try backend.run(ally, env, exprs);
}
