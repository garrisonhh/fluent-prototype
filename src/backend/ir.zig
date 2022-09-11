//! compiling SExprs into three-address-code quadruples.
//!
//! Ops are the 'quadruple' format of three address code. every Block contains
//! some number of statically known local variables, which ops reference in
//! their addressing.

const std = @import("std");
const kz = @import("kritzler");
const util = @import("../util/util.zig");
const fluent = @import("fluent.zig");
const sema = @import("sema.zig");
const Env = @import("env.zig");
const ops = @import("ir/ops.zig");
const blocks = @import("ir/blocks.zig");

const Allocator = std.mem.Allocator;
const FlatType = fluent.FlatType;
const SType = fluent.Type;
const SExpr = fluent.Value;
const TypedExpr = sema.TypedExpr;
const Mason = blocks.Mason;
const stdout = std.io.getStdOut().writer();

pub const Op = ops.Op;
pub const OpCode = ops.OpCode;
pub const Block = blocks.Block;

/// *you don't need to clone the passed in value*
fn build_const(mason: *Mason, value: TypedExpr) anyerror!Op.UInt {
    const expects = try value.find_type(mason.ally);
    defer expects.deinit(mason.ally);

    return try mason.add_op(Op{
        .code = .@"const",
        .a = try mason.add_const(try value.to_value(mason.ally)),
        .to = try mason.add_local(try value.find_type(mason.ally))
    });
}

fn build_symbol(
    mason: *Mason,
    env: *Env,
    sym: TypedExpr.TypedSymbol
) anyerror!Op.UInt {
    return switch (env.get_data(sym.symbol).?) {
        .local, .block => |index| @intCast(Op.UInt, index),
        .value => |value| try mason.add_op(Op{
            .code = .@"const",
            .a = try mason.add_const(value),
            .to = try mason.add_local(try sym.stype.clone(mason.ally)),
        }),
    };
}

/// lowers function call
fn build_call(mason: *Mason, env: *Env, expr: TypedExpr) anyerror!Op.UInt {
    // lower call exprs
    const block_ref = try build_expr(mason, env, expr.call.exprs[0]);

    const params = expr.call.exprs[1..];
    const param_refs = try mason.ally.alloc(Op.UInt, params.len);
    for (params) |param, i| {
        param_refs[i] = try build_expr(mason, env, param);
    }

    // lower call
    _ = try mason.add_op(Op{
        .code = .frame,
        .a = @intCast(Op.UInt, env.blocks.items[block_ref].locals.len)
    });

    for (param_refs) |ref, i| {
        _ = try mason.add_op(Op{
            .code = .param,
            .a = @intCast(Op.UInt, i),
            .b = ref
        });
    }

    const block_type = env.blocks.items[block_ref].output_type();

    return try mason.add_op(Op{
        .code = .call,
        .a = block_ref,
        .to = try mason.add_local(try block_type.clone(mason.ally))
    });
}

fn build_list(mason: *Mason, env: *Env, expr: TypedExpr) anyerror!Op.UInt {
    const ally = mason.ally;
    _ = env;

    const subtype = expr.list.subtype;
    const exprs = expr.list.exprs;

    // type and size
    const size = @intCast(i64, exprs.len);
    const type_ref = try build_const(mason, TypedExpr{ .stype = subtype });
    const size_ref = try build_const(mason, TypedExpr{ .int = size });

    // list allocation
    const list_type = try SType.init_list(ally, try subtype.clone(ally));
    const list_ref = try mason.add_op(Op{
        .code = .alloc,
        .a = type_ref,
        .b = size_ref,
        .to = try mason.add_local(list_type)
    });

    // set indices
    const ptr_type = try SType.init_ptr(ally, try subtype.clone(ally));
    const ptr_ref = try mason.add_op(Op{
        .code = .list_ptr,
        .a = list_ref,
        .to = try mason.add_local(ptr_type)
    });

    for (exprs) |child, i| {
        // increment pointer
        if (i > 0) {
            _ = try mason.add_op(Op{
                .code = .pinc,
                .a = ptr_ref,
                .to = ptr_ref
            });
        }

        // poke value
        _ = try mason.add_op(Op{
            .code = .poke,
            .a = ptr_ref,
            .b = try build_expr(mason, env, child)
        });
    }

    return list_ref;
}

/// returns block ref
/// TODO unsure how this will work with function as value semantics. I think I
/// will have to implement transitioning SExprs to only values, and storing
/// the function index as the data
fn build_func(mason: *Mason, env: *Env, expr: TypedExpr) anyerror!Op.UInt {
    const name = try env.next_anon_func_name();
    defer mason.ally.free(name);

    const block = try lower_func(mason.ally, env, name, expr);
    defer block.deinit(mason.ally);

    const stype = try expr.find_type(mason.ally);
    defer stype.deinit(mason.ally);

    const index = try env.define_block(name, stype, block);

    return @intCast(Op.UInt, index);
}

/// returns where this expr produces its value (`to`)
fn build_expr(mason: *Mason, env: *Env, expr: TypedExpr) anyerror!Op.UInt {
    if (expr.is_literal()) return try build_const(mason, expr);

    return switch (expr) {
        // these are literals
        .unit, .undef, .int, .stype => unreachable,
        // def is pure syntax
        .def => unreachable,
        .symbol => |sym| try build_symbol(mason, env, sym),
        .call => try build_call(mason, env, expr),
        .list => try build_list(mason, env, expr),
        .func => try build_func(mason, env, expr),
    };
}

/// lowers a function to a block
/// TODO once functions as values exists, this can be private
pub fn lower_func(
    ally: Allocator,
    env: *Env,
    name: []const u8,
    fn_expr: TypedExpr
) anyerror!Block {
    const func = fn_expr.func;

    // construct param env
    var sub_env = Env.init(ally, env);
    defer sub_env.deinit();

    for (func.params) |param, i| {
        try sub_env.define_local(param.symbol, param.stype, i);
    }

    // do masonry
    const param_types = try ally.alloc(SType, func.params.len);
    defer ally.free(param_types);

    for (func.params) |param, i| param_types[i] = try param.stype.clone(ally);

    var mason = try Mason.init(ally, name, param_types);
    const output = try build_expr(&mason, &sub_env, func.body.*);

    return mason.build(output);
}

/// lower a program (file or repl-level) expression to a block
pub fn lower_expr(
    ally: Allocator,
    env: *Env,
    name: []const u8,
    expr: TypedExpr
) !Block {
    var mason = try Mason.init(ally, name, &.{});
    return mason.build(try build_expr(&mason, env, expr));
}
