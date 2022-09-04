//! run contains the high-level implementation of running a fluent program
//! from its first SExprs to its last

const std = @import("std");
const fluent = @import("fluent.zig");
const sema = @import("sema.zig");
const Env = @import("env.zig");

const Allocator = std.mem.Allocator;
const SExpr = fluent.SExpr;

pub fn run(
    ally: Allocator,
    env: *Env,
    program: []const SExpr
) !SExpr {
    // generate typed ast
    const ast = try sema.analyze(ally, env.*, program);
    defer ast.deinit();

    // definitions

    // lower ast to IR

    @panic("reached end of run()");
}