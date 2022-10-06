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

pub fn execute(
    ally: Allocator,
    env: *Env,
    handle: context.FileHandle,
) !Value {
    _ = env;

    try frontend.parse(ally, handle);

    _ = try context.postMessage(
        .err,
        context.Loc.init(handle, 0, 0),
        "this is an error"
    );
    try context.flushMessages();

    return Value{ .unit = {} };

    // @panic("TODO");

    // lex + parse
    // const program = try ctx.wrap_stage(frontend.parse(&ctx, ally));
    // defer program.deinit(ally);

    // run
    // return try backend.run(ally, env, program);
}
