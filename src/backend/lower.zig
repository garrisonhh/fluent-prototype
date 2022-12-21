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
    ty: TypeId,
    data: []const u8,
) LowerError!Local {
    const value = try func.addConst(data);
    const out = try func.addLocal(ty);
    try func.addOp(Op{ .ldc = .{ .a = value, .to = out } });

    return out;
}

fn lowerBool(func: *ssa.FuncBuilder, expr: TExpr) LowerError!Local {
    const byte: u8 = if (expr.data.@"bool") 1 else 0;
    return lowerLoadConst(func, expr.ty, &.{byte});
}

fn lowerNumber(func: *ssa.FuncBuilder, expr: TExpr) LowerError!Local {
    var buf: [8]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buf);
    const data = try expr.data.number.asBytes(fba.allocator());

    return lowerLoadConst(func, expr.ty, data);
}

fn lowerCast(
    env: *Env,
    prog: *ssa.ProgramBuilder,
    func: *ssa.FuncBuilder,
    expr: TExpr
) LowerError!Local {
    const in = try lowerExpr(env, prog, func, expr.data.cast.*);
    const out = try func.addLocal(expr.ty);
    try func.addOp(Op{ .cast = .{ .a = in, .to = out } });

    return out;
}

fn lowerBuiltinOp(
    func: *ssa.FuncBuilder,
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

    try func.addOp(op);
}

fn lowerIf(
    env: *Env,
    prog: *ssa.ProgramBuilder,
    func: *ssa.FuncBuilder,
    expr: TExpr
) LowerError!Local {
    const exprs = expr.data.call[1..];

    const cond = try lowerExpr(env, prog, func, exprs[0]);

    // alloca correct amount of memory for branch output
    const mem_size = env.typeGet(expr.ty).sizeOf(env.typewelt.*);
    const ptr_ty = try env.typeIdentify(Type{ .ptr = expr.ty });
    const out_ptr = try func.addLocal(ptr_ty);

    try func.addOp(Op{ .alloca = .{ .to = out_ptr, .size = mem_size } });

    // branch op with labels
    const branches = [2]ssa.Label{try func.addLabel(), try func.addLabel()};
    try func.addOp(Op{
        .br = .{ .cond = cond, .a = branches[0], .b = branches[1] }
    });

    // create final block dest
    var merge_at = try func.addLabel();

    // create branches which store value on allocated stack memory
    for (exprs[1..]) |branch_expr, i| {
        try func.replaceLabel(branches[i]);

        const out = try lowerExpr(env, prog, func, branch_expr);
        try func.addOp(Op{ .store = .{ .a = out, .b = out_ptr } });
        try func.addOp(Op{ .jmp = .{ .dst = merge_at } });
    }

    // finally, merge and load value from stack
    try func.replaceLabel(merge_at);

    const final = try func.addLocal(expr.ty);
    try func.addOp(Op{ .load = .{ .to = final, .a = out_ptr } });

    return final;
}

fn lowerCall(
    env: *Env,
    prog: *ssa.ProgramBuilder,
    func: *ssa.FuncBuilder,
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
                    args[i] = try lowerExpr(env, prog, func, param_expr);
                }

                // output local
                const out = try func.addLocal(expr.ty);

                try lowerBuiltinOp(func, out, bound.builtin_op, args);

                return out;
            },
            .builtin_flow => |flow| return switch (flow) {
                .@"if" => try lowerIf(env, prog, func, expr),
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
    expr: TExpr
) LowerError!Local {
    var final: Local = undefined;

    // lower each child in order
    const children = expr.getChildren();
    std.debug.assert(children.len > 0);
    for (children) |child| {
        final = try lowerExpr(env, prog, func, child);
    }

    return final;
}

fn lowerExpr(
    env: *Env,
    prog: *ssa.ProgramBuilder,
    func: *ssa.FuncBuilder,
    expr: TExpr
) LowerError!Local {
    return switch (expr.data) {
        .@"bool" => try lowerBool(func, expr),
        .number => try lowerNumber(func, expr),
        .cast => try lowerCast(env, prog, func, expr),
        .call => try lowerCall(env, prog, func, expr),
        .do => try lowerDo(env, prog, func, expr),
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
    const func_entry = try func.addLabel();

    // lower program expr
    const final = try lowerExpr(env, &prog, &func, expr);
    try func.addOp(Op{ .ret = .{ .a = final } });

    // build program with root as entry point
    const prog_entry = try prog.addFunc(func.build(func_entry));
    return try prog.build(prog_entry);
}
