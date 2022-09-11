//! run contains the high-level implementation of running a fluent program
//! from its first SExprs to its last
//!
//! TODO type + function dependency solving before execution? hard but necessary
//! problem

const std = @import("std");
const fluent = @import("fluent.zig");
const sema = @import("sema.zig");
const ir = @import("ir.zig");
const Env = @import("env.zig");

const Allocator = std.mem.Allocator;
const TypedExpr = sema.TypedExpr;
const SType = fluent.Type;
const SExpr = fluent.Value;

// TODO I don't like that I'm manually doing defs here. maybe there is a cleaner
// design for that?
fn eval_def(env: *Env, symbol: []const u8, stype: SType, value: SExpr) !void {
    if (env.contains(symbol)) return error.SymbolRedef;

    // functions require more compilation steps
    if (stype == .func) {
        @panic("TODO def fn");
    } else {
        try env.define_value(symbol, stype, value);
    }
}

fn eval_expr(ally: Allocator, env: *Env, expr: TypedExpr) !SExpr {
    if (expr == .def) {
        @panic("TODO");

        // const def = expr.def;

        // eval type
        // const type_name =
            // try std.fmt.allocPrint(ally, "`{s}` type", .{def.symbol});
        // defer ally.free(type_name);

        // var type_block = try ir.lower_expr(ally, env, type_name, def.anno.*);
        // defer type_block.deinit(ally);

        // const type_expr = try env.execute(ally, type_block, &.{});
        // defer type_expr.deinit(ally);

        // const stype = type_expr.stype;

        // eval value
        // const body_expr = try sema.analyze(ally, env.*, def.body.*, stype);
        // defer body_expr.deinit(ally);

        // const body_name =
            // try std.fmt.allocPrint(ally, "`{s}` body", .{def.symbol});
        // defer ally.free(body_name);

        // TODO functions as values
        // if (stype == .func) {
            // fn
            // var block = try ir.lower_func(ally, env, body_name, body_expr);
            // defer block.deinit(ally);

            // _ = try env.define_block(def.symbol, stype, block);
        // } else {
            // value
            // var block = try ir.lower_expr(ally, env, body_name, body_expr);
            // defer block.deinit(ally);

            // const value = try env.execute(ally, block, &.{});
            // defer value.deinit(ally);

            // try env.define_value(def.symbol, stype, value);
        // }

        // return SExpr{ .unit = {} };
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
    program: []const TypedExpr
) !SExpr {
    // lower and execute exprs
    var final_value = SExpr{ .unit = {} };
    for (program) |expr| {
        final_value.deinit(ally);
        final_value = try eval_expr(ally, env, expr);
    }

    return final_value;
}