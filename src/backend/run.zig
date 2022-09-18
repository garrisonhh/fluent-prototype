//! run contains the high-level implementation of running a fluent program
//! from its first SExprs to its last
//!
//! TODO type + function dependency solving before execution? hard but necessary
//! problem

const std = @import("std");
const fluent = @import("fluent.zig");
const frontend = @import("../frontend.zig");
const sema = @import("sema.zig");
const ir = @import("ir.zig");
const Env = @import("env.zig");

const Allocator = std.mem.Allocator;
const TypedExpr = sema.TypedExpr;
const Type = fluent.Type;
const Value = fluent.Value;
const AstExpr = frontend.AstExpr;

fn eval_expr(ally: Allocator, env: *Env, expr: TypedExpr) !Value {
    // TODO replace this with a type dependency solver at some point
    if (expr == .def) {
        const def = expr.def;

        // eval type
        const type_name =
            try std.fmt.allocPrint(ally, "`{s}` type", .{def.symbol});
        defer ally.free(type_name);

        var type_block = try ir.lower_expr(ally, env, type_name, def.anno.*);
        defer type_block.deinit(ally);

        const type_expr = try env.execute(ally, type_block, &.{});
        defer type_expr.deinit(ally);

        const stype = type_expr.stype;

        // eval value
        const body_expr = try sema.analyze(ally, env.*, def.body.*, stype);
        defer body_expr.deinit(ally);

        const body_name =
            try std.fmt.allocPrint(ally, "`{s}` body", .{def.symbol});
        defer ally.free(body_name);

        // define value
        var block = try ir.lower_expr(ally, env, body_name, body_expr);
        defer block.deinit(ally);

        const value = try env.execute(ally, block, &.{});
        defer value.deinit(ally);

        if (env.contains(def.symbol)) return error.SymbolRedef;
        try env.define_value(def.symbol, stype, value);

        return Value{ .unit = {} };
    } else {
        // eval this
        var block = try ir.lower_expr(ally, env, "expr", expr);
        defer block.deinit(ally);

        return try env.execute(ally, block, &.{});
    }
}

/// returns value allocated on ally
pub fn run(
    ally: Allocator,
    env: *Env,
    program: []const AstExpr
) !Value {
    // lower and execute exprs
    var final_value = Value{ .unit = {} };
    for (program) |ast_expr| {
        const expr = try sema.analyze(ally, env.*, ast_expr, null);

        // TODO remove vvv
        try expr.display(ally, "running", .{});

        final_value.deinit(ally);
        final_value = try eval_expr(ally, env, expr);
    }

    return final_value;
}