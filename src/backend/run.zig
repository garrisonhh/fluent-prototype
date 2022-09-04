//! run contains the high-level implementation of running a fluent program
//! from its first SExprs to its last

const std = @import("std");
const fluent = @import("fluent.zig");
const sema = @import("sema.zig");
const ir = @import("ir.zig");
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

    for (ast.exprs) |expr| try expr.display(ally, "sema produced", .{});

    // TODO dependency solver phase here

    // lower ast to IR
    for (ast.exprs) |expr| {
        if (expr == .def) {
            const type_block = try ir.lower_expr(
                ally,
                env.*,
                expr.def.symbol,
                expr.def.anno.*
            );
            defer type_block.deinit(ally);

            // TODO define

            try type_block.display(ally);
        } else {
            const block = try ir.lower_expr(ally, env.*, "expr", expr);
            defer block.deinit(ally);

            // TODO run?

            try block.display(ally);
        }
    }

    @panic("reached end of run()");
}