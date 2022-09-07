//! run contains the high-level implementation of running a fluent program
//! from its first SExprs to its last
//!
//! TODO dependency solving?

const std = @import("std");
const fluent = @import("fluent.zig");
const sema = @import("sema.zig");
const ir = @import("ir.zig");
const Vm = @import("vm.zig").Vm;
const Env = @import("env.zig");

const Allocator = std.mem.Allocator;
const SType = fluent.SType;
const SExpr = fluent.SExpr;

pub fn run(
    ally: Allocator,
    env: *Env,
    program: []const SExpr
) !SExpr {
    // cyclically lower ast to IR
    var vm = Vm.init(ally);
    var last_value = SExpr{ .unit = {} };

    for (program) |sexpr| {
        const ast = try sema.analyze(ally, env.*, sexpr, SType{ .undef = {} });
        defer ast.deinit();

        // TODO must traverse TypedExpr and execute all types here

        const expr = ast.root;

        if (expr == .def) {
            // TODO great starting point for optimizing arena cycles + memory
            // usage in the backend
            const def = expr.def;

            // analyze type
            const anno = def.anno.*;

            const type_block = try ir.lower_expr(ally, env.*, "def type", anno);
            defer type_block.deinit(ally);

            // execute type
            const type_expr = try vm.execute(type_block, &.{});
            defer type_expr.deinit(ally);

            const stype = type_expr.stype;

            // analyze body
            const body_ast = try sema.analyze(ally, env.*, def.body.*, stype);
            defer body_ast.deinit();

            const body = body_ast.root;

            const body_block = try ir.lower_expr(ally, env.*, "def body", body);
            defer body_block.deinit(ally);

            // execute body
            const value = try vm.execute(body_block, &.{});
            defer value.deinit(ally);

            const analyzed = try sema.analyze(ally, env.*, value, stype);
            defer analyzed.deinit();

            // define in env
            try env.define_value(def.symbol, stype, analyzed.root);
        } else {
            // lower expr
            const block = try ir.lower_expr(ally, env.*, "expr", expr);
            defer block.deinit(ally);

            // execute
            last_value.deinit(ally);
            last_value = try vm.execute(block, &.{});
        }
    }

    return last_value;
}