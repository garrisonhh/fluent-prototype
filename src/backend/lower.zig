//! implements lowering the analyzed AST into SSA IR

const std = @import("std");
const Allocator = std.mem.Allocator;
const util = @import("util");
const Symbol = util.Symbol;
const Name = util.Name;
const builtin = @import("builtin");
const ssa = @import("ssa.zig");
const Const = ssa.Const;
const Local = ssa.Local;
const Op = ssa.Op;
const Label = ssa.Label;
const Func = ssa.Func;
const types = @import("types.zig");
const TypeId = types.TypeId;
const Type = types.Type;
const Env = @import("env.zig");
const TExpr = @import("texpr.zig");
const Value = @import("value.zig");
const canon = @import("canon.zig");

pub const LowerError = Allocator.Error;

fn lowerLoadConst(
    ally: Allocator,
    func: *Func,
    block: Label,
    ty: TypeId,
    value: Value
) LowerError!Local {
    const @"const" = try func.addConst(ally, value);
    const out = try func.addLocal(ally, ty);
    try func.addOp(ally, block, Op{ .ldc = .{ .a = @"const", .to = out } });

    return out;
}

fn lowerBool(
    ally: Allocator,
    func: *Func,
    block: Label,
    expr: TExpr
) LowerError!Local {
    const byte: u8 = if (expr.data.@"bool") 1 else 0;
    const value = try Value.init(ally, std.mem.asBytes(&byte));

    return lowerLoadConst(ally, func, block, expr.ty, value);
}

fn lowerNumber(
    ally: Allocator,
    func: *Func,
    block: Label,
    expr: TExpr
) LowerError!Local {
    const value = try expr.data.number.asValue(ally);
    return lowerLoadConst(ally, func, block, expr.ty, value);
}

fn lowerCall(
    env: *Env,
    func: *Func,
    block: Label,
    expr: TExpr
) LowerError!Local {
    _ = env;
    _ = func;
    _ = block;
    _ = expr;

    @panic("TODO lower function calls");
}

fn lowerExpr(
    env: *Env,
    func: *Func,
    block: Label,
    expr: TExpr
) LowerError!Local {
    return switch (expr.data) {
        .@"bool" => try lowerBool(env.ally, func, block, expr),
        .number => try lowerNumber(env.ally, func, block, expr),
        .name => |name| {
            std.debug.panic("TODO lower name {}", .{name});
        },
        .call => try lowerCall(env, func, block, expr),
        else => {
            const tag = @tagName(expr.data);
            std.debug.panic("TODO lower {s} exprs\n", .{tag});
        }
    };
}

/// lowers expression as if it is the body of a function with no args
pub fn lower(env: *Env, scope: Name, expr: TExpr) LowerError!ssa.Func {
    // set up context
    var func = try Func.init(env.ally, scope, &.{}, expr.ty);

    // lower expr as a function
    const block = try func.addBlock(env.ally);
    const final = try lowerExpr(env, &func, block, expr);

    // TODO add return stmt
    _ = final;

    return func;
}
