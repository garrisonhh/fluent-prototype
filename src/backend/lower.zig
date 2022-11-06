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

fn lowerNumber(block: *ssa.BlockBuilder, expr: TExpr) LowerError!Local {
    const ally = block.ally;

    // get raw bytes
    const data = try expr.data.number.asBytes(ally);
    defer ally.free(data);

    // load const
    const value = try block.addConst(data);
    const local = try block.addLocal(expr.ty);
    try block.addOp(Op{
        .ldc = Op.LoadConst{
            .a = value,
            .to = local
        }
    });

    return local;
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
        .number => try lowerNumber(block, expr),
        .do => try lowerDo(env, prog, block, expr),
        else => @panic("TODO")
    };
}

pub fn lower(ally: Allocator, env: Env, expr: TExpr) LowerError!ssa.Program {
    var prog = ssa.ProgramBuilder.init(ally);
    var block = try ssa.BlockBuilder.init(ally, comptime Symbol.init("(root)"));

    _ = try lowerExpr(env, &prog, &block, expr);

    try prog.addBlock(block.build());
    return try prog.build();
}
