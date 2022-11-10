//! implements lowering the analyzed AST into SSA IR

const std = @import("std");
const util = @import("util");
const builtin = @import("builtin");
const ssa = @import("ssa.zig");
const types = @import("types.zig");
const Env = @import("env.zig");
const TExpr = @import("texpr.zig");

const Allocator = std.mem.Allocator;
const asBytes = std.mem.asBytes;
const Symbol = util.Symbol;
const Const = ssa.Const;
const Local = ssa.Local;
const Op = ssa.Op;
const Type = types.Type;
const TypeId = types.TypeId;

pub const LowerError =
    Allocator.Error;

fn lowerLoadConst(
    func: *ssa.FuncBuilder,
    block: *ssa.BlockBuilder,
    ty: TypeId,
    data: []const u8,
) LowerError!Local {
    const value = try func.addConst(data);
    const out = try func.addLocal(ty);
    try block.addOp(Op{ .ldc = .{ .a = value, .to = out } });

    return out;
}

fn lowerBool(
    func: *ssa.FuncBuilder,
    block: *ssa.BlockBuilder,
    expr: TExpr
) LowerError!Local {
    const byte: u8 = if (expr.data.@"bool") 1 else 0;
    return lowerLoadConst(func, block, expr.ty, &.{byte});
}

fn lowerNumber(
    func: *ssa.FuncBuilder,
    block: *ssa.BlockBuilder,
    expr: TExpr
) LowerError!Local {
    // get raw bytes TODO do I really need an allocation here ..?
    const data = try expr.data.number.asBytes(func.ally);
    defer func.ally.free(data);

    return lowerLoadConst(func, block, expr.ty, data);
}

fn lowerCast(
    env: *Env,
    prog: *ssa.ProgramBuilder,
    func: *ssa.FuncBuilder,
    block: *ssa.BlockBuilder,
    expr: TExpr
) LowerError!Local {
    const in = try lowerExpr(env, prog, func, block, expr.data.cast.*);
    const out = try func.addLocal(expr.ty);
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

fn lowerIf(
    env: *Env,
    prog: *ssa.ProgramBuilder,
    func: *ssa.FuncBuilder,
    block: *ssa.BlockBuilder,
    expr: TExpr
) LowerError!Local {
    const exprs = expr.data.call[1..];

    const cond = try lowerExpr(env, prog, func, block, exprs[0]);

    // alloca correct amount of memory for branch output
    const mem_size = env.typeGet(expr.ty).sizeOf(env.typewelt.*);
    const mem_size_data = std.mem.asBytes(&mem_size);
    const u64_ty = try env.typeIdentifyNumber(.uint, 64);
    const mem_req = try lowerLoadConst(func, block, u64_ty, mem_size_data);

    const ptr_ty = try env.typeIdentify(Type{ .ptr = expr.ty });
    const out_ptr = try func.addLocal(ptr_ty);

    try block.addOp(Op{ .alloca = .{ .to = out_ptr, .a = mem_req } });

    // create final block dest
    var merge = try func.newBlockBuilder();

    // create branches which store value on allocated stack memory
    var branches: [2]ssa.BlockRef = undefined;
    for (exprs[1..]) |branch_expr, i| {
        var branch_block = try func.newBlockBuilder();
        defer branch_block.build();

        const out = try lowerExpr(env, prog, func, &branch_block, branch_expr);
        try branch_block.addOp(Op{ .store = .{ .a = out_ptr, .b = out } });
        try branch_block.addOp(Op{ .jmp = .{ .dst = merge.ref } });

        branches[i] = branch_block.ref;
    }

    // add branch op and replace block builder
    try block.addOp(Op{
        .br = .{ .cond = cond, .a = branches[0], .b = branches[1] }
    });
    block.replace(merge);

    // finally, load value from stack
    const final = try func.addLocal(expr.ty);
    try block.addOp(Op{ .load = .{ .to = final, .a = out_ptr } });

    return final;
}

fn lowerCall(
    env: *Env,
    prog: *ssa.ProgramBuilder,
    func: *ssa.FuncBuilder,
    block: *ssa.BlockBuilder,
    expr: TExpr
) LowerError!Local {
    const ally = func.ally;
    const exprs = expr.data.call;
    std.debug.assert(exprs.len > 0);

    const head = exprs[0];
    const tail = exprs[1..];

    // builtin functions and operators have their own logic
    // TODO is this the right way to do this?
    if (head.data == .symbol) detect_op: {
        const bound = env.getBound(head.data.symbol) orelse break :detect_op;

        switch (bound.*) {
            .builtin_op => {
                // lower args
                const args = try ally.alloc(Local, tail.len);
                defer ally.free(args);

                for (tail) |param_expr, i| {
                    args[i] = try lowerExpr(env, prog, func, block, param_expr);
                }

                // output local
                const out = try func.addLocal(expr.ty);

                try lowerBuiltinOp(block, out, bound.builtin_op, args);

                return out;
            },
            .builtin_flow => |flow| return switch (flow) {
                .@"if" => try lowerIf(env, prog, func, block, expr),
            },
            else => @panic("TODO lower a call to any symbol")
        }
    }

    // regular functions

    @panic("TODO functions for real");
}

fn lowerDo(
    env: *Env,
    prog: *ssa.ProgramBuilder,
    func: *ssa.FuncBuilder,
    block: *ssa.BlockBuilder,
    expr: TExpr
) LowerError!Local {
    var final: Local = undefined;

    // lower each child in order
    const children = expr.getChildren();
    std.debug.assert(children.len > 0);
    for (children) |child| {
        final = try lowerExpr(env, prog, func, block, child);
    }

    return final;
}

fn lowerExpr(
    env: *Env,
    prog: *ssa.ProgramBuilder,
    func: *ssa.FuncBuilder,
    block: *ssa.BlockBuilder,
    expr: TExpr
) LowerError!Local {
    return switch (expr.data) {
        .@"bool" => try lowerBool(func, block, expr),
        .number => try lowerNumber(func, block, expr),
        .cast => try lowerCast(env, prog, func, block, expr),
        .call => try lowerCall(env, prog, func, block, expr),
        .do => try lowerDo(env, prog, func, block, expr),
        else => std.debug.panic(
            "TODO lower {s} exprs\n",
            .{@tagName(expr.data)}
        )
    };
}

pub fn lower(ally: Allocator, env: *Env, expr: TExpr) LowerError!ssa.Program {
    // set up context
    var prog = ssa.ProgramBuilder.init(ally);
    const root = comptime Symbol.init("root");
    var func = try ssa.FuncBuilder.init(ally, root, &.{}, expr.ty);
    var block = try func.newBlockBuilder();
    const func_entry = block.ref;

    // lower program expr
    const final = try lowerExpr(env, &prog, &func, &block, expr);
    try block.addOp(Op{ .ret = .{ .a = final } });

    // build program with root as entry point
    block.build();

    const prog_entry = try prog.addFunc(func.build(func_entry));
    return try prog.build(prog_entry);
}
