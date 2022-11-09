//! implements lowering the analyzed AST into SSA IR

const std = @import("std");
const util = @import("util");
const builtin = @import("builtin");
const ssa = @import("ssa.zig");
const Env = @import("env.zig");
const TExpr = @import("texpr.zig");

const Allocator = std.mem.Allocator;
const asBytes = std.mem.asBytes;
const Symbol = util.Symbol;
const Const = ssa.Const;
const Local = ssa.Local;
const Op = ssa.Op;

pub const LowerError =
    Allocator.Error;

fn lowerBool(block: *ssa.BlockBuilder, expr: TExpr) LowerError!Local {
    const byte: u8 = if (expr.data.@"bool") 1 else 0;

    // load const
    const value = try block.addConst(&.{byte});
    const out = try block.addLocal(expr.ty);
    try block.addOp(Op{ .ldc = .{ .a = value, .to = out } });

    return out;
}

fn lowerNumber(block: *ssa.BlockBuilder, expr: TExpr) LowerError!Local {
    const ally = block.ally;

    // get raw bytes
    const data = try expr.data.number.asBytes(ally);
    defer ally.free(data);

    // load const
    const value = try block.addConst(data);
    const out = try block.addLocal(expr.ty);
    try block.addOp(Op{ .ldc = .{ .a = value, .to = out } });

    return out;
}

fn lowerCast(
    env: Env,
    prog: *ssa.ProgramBuilder,
    block: *ssa.BlockBuilder,
    expr: TExpr
) LowerError!Local {
    const in = try lowerExpr(env, prog, block, expr.data.cast.*);
    const out = try block.addLocal(expr.ty);
    try block.addOp(Op{ .cast = .{ .a = in, .to = out } });

    return out;
}

fn lowerBuiltinOp(
    block: *ssa.BlockBuilder,
    out: Local,
    builtin_op: Env.BuiltinOp,
    args: []const Local
) LowerError!void {
    // can assume args are proper length; this is handled in sema
    const op = switch (builtin_op) {
        .add => Op{ .add = .{ .to = out, .a = args[0], .b = args[1] } },
        .sub => Op{ .sub = .{ .to = out, .a = args[0], .b = args[1] } },
        .mul => Op{ .mul = .{ .to = out, .a = args[0], .b = args[1] } },
        .div => Op{ .div = .{ .to = out, .a = args[0], .b = args[1] } },
        .@"and" => Op{ .@"and" = .{ .to = out, .a = args[0], .b = args[1] } },
        .@"or" => Op{ .@"or" = .{ .to = out, .a = args[0], .b = args[1] } },
        .not => Op{ .not = .{ .to = out, .a = args[0] } },
    };

    try block.addOp(op);
}

fn lowerCall(
    env: Env,
    prog: *ssa.ProgramBuilder,
    block: *ssa.BlockBuilder,
    expr: TExpr
) LowerError!Local {
    const ally = block.ally;
    const exprs = expr.data.call;
    std.debug.assert(exprs.len > 0);

    const head = exprs[0];
    const tail = exprs[1..];

    // lower args
    const args = try ally.alloc(Local, tail.len);
    defer ally.free(args);

    for (tail) |param_expr, i| {
        args[i] = try lowerExpr(env, prog, block, param_expr);
    }

    // output local
    const out = try block.addLocal(expr.ty);

    // ops are special functions
    // TODO is this the right way to do this?
    if (head.data == .symbol) detect_op: {
        const bound = env.getBound(head.data.symbol) orelse break :detect_op;

        if (bound.* == .builtin_op) {
            try lowerBuiltinOp(block, out, bound.builtin_op, args);
            return out;
        }
    }

    // regular functions

    @panic("TODO functions for real");
}

fn lowerDo(
    env: Env,
    prog: *ssa.ProgramBuilder,
    block: *ssa.BlockBuilder,
    expr: TExpr
) LowerError!Local {
    var final: Local = undefined;

    // lower each child in order
    const children = expr.getChildren();
    std.debug.assert(children.len > 0);
    for (children) |child| {
        final = try lowerExpr(env, prog, block, child);
    }

    return final;
}

fn lowerExpr(
    env: Env,
    prog: *ssa.ProgramBuilder,
    block: *ssa.BlockBuilder,
    expr: TExpr
) LowerError!Local {
    return switch (expr.data) {
        .@"bool" => try lowerBool(block, expr),
        .number => try lowerNumber(block, expr),
        .cast => try lowerCast(env, prog, block, expr),
        .call => try lowerCall(env, prog, block, expr),
        .do => try lowerDo(env, prog, block, expr),
        else => std.debug.panic(
            "TODO lower {s} exprs\n",
            .{@tagName(expr.data)}
        )
    };
}

pub fn lower(ally: Allocator, env: Env, expr: TExpr) LowerError!ssa.Program {
    var prog = ssa.ProgramBuilder.init(ally);
    var block = try ssa.BlockBuilder.init(ally, comptime Symbol.init("(root)"));

    // lower expr and return value :)
    const final = try lowerExpr(env, &prog, &block, expr);
    try block.addOp(Op{ .ret = .{ .a = final } });

    const entry = try prog.addBlock(block.build());
    return try prog.build(entry);
}
