//! implements lowering the analyzed AST into SSA IR

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const util = @import("util");
const Symbol = util.Symbol;
const Name = util.Name;
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
const Builtin = canon.Builtin;

pub const Error = Allocator.Error;

fn lowerLoadConst(
    ally: Allocator,
    func: *Func,
    block: Label,
    ty: TypeId,
    value: Value
) Error!Local {
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
) Error!Local {
    const byte: u8 = if (expr.data.@"bool") 1 else 0;
    const value = try Value.init(ally, std.mem.asBytes(&byte));

    return lowerLoadConst(ally, func, block, expr.ty, value);
}

fn lowerNumber(
    ally: Allocator,
    func: *Func,
    block: Label,
    expr: TExpr
) Error!Local {
    const value = try expr.data.number.asValue(ally);
    return lowerLoadConst(ally, func, block, expr.ty, value);
}

/// lower builtins that connect directly to ssa IR ops
fn lowerOperator(
    env: *Env,
    func: *Func,
    block: *Label,
    b: Builtin,
    args: []const TExpr,
    returns: TypeId
) Error!Local {
    const ally = env.ally;

    // lower args
    const locals = try ally.alloc(Local, args.len);
    defer ally.free(locals);

    for (args) |arg, i| {
        locals[i] = try lowerExpr(env, func, block, arg);
    }

    // generate operation
    return switch (b) {
        // binary ops
        inline .add, .sub, .mul, .div, .mod, .@"and", .@"or" => |tag| bin: {
            // TODO make sure this is checked in sema
            std.debug.assert(args.len == 2);

            const to = try func.addLocal(ally, returns);
            const op = @unionInit(Op, @tagName(tag), Op.Binary{
                .a = locals[0],
                .b = locals[1],
                .to = to
            });
            try func.addOp(ally, block.*, op);

            break :bin to;
        },
        // unary ops
        inline .not, .cast => |tag| un: {
            // TODO make sure this is checked in sema
            std.debug.assert(args.len == 1);

            const to = try func.addLocal(ally, returns);
            const op = @unionInit(Op, @tagName(tag), Op.Unary{
                .a = locals[0],
                .to = to
            });
            try func.addOp(ally, block.*, op);

            break :un to;
        },
        else => unreachable
    };
}

/// functions get lowered as their own Func and then stored in the env. for
/// the current block, their FuncRef can then get lowered in.
fn lowerFn(env: *Env, func: *Func, block: *Label, expr: TExpr) Error!Local {
    _ = env;
    _ = func;
    _ = block;
    _ = expr;

    @panic("TODO lower `fn` expr");
}

fn lowerCall(env: *Env, func: *Func, block: *Label, expr: TExpr) Error!Local {
    const exprs = expr.data.call;
    std.debug.assert(exprs.len > 0);

    const head_expr = exprs[0];
    const args = exprs[1..];

    if (head_expr.data != .name) {
        std.debug.panic(
            "TODO lower calls to {s} texprs\n",
            .{@tagName(head_expr.data)}
        );
    }

    // compile function calls to a name
    const head = env.get(head_expr.data.name);

    // delegate operators
    if (head.data == .builtin) {
        return switch (head.data.builtin) {
            .add, .sub, .mul, .div, .mod, .@"and", .@"or", .not, .cast
                => |b| try lowerOperator(env, func, block, b, args, expr.ty),
            .@"fn" => try lowerFn(env, func, block, expr),
            else => |b| {
                std.debug.panic("TODO lower builtin {s}", .{@tagName(b)});
            }
        };
    }

    @panic("TODO lower function calls");
}

fn lowerExpr(env: *Env, func: *Func, block: *Label, expr: TExpr) Error!Local {
    return switch (expr.data) {
        .@"bool" => try lowerBool(env.ally, func, block.*, expr),
        .number => try lowerNumber(env.ally, func, block.*, expr),
        .name => |name| name: {
            const value = env.get(name);
            // TODO how will this break?
            std.debug.assert(value.isValue());

            break :name try lowerExpr(env, func, block, value);
        },
        .call => try lowerCall(env, func, block, expr),
        else => {
            const tag = @tagName(expr.data);
            std.debug.panic("TODO lower {s} exprs\n", .{tag});
        }
    };
}

/// lowers expression as if it is the body of a function with no args
pub fn lower(env: *Env, scope: Name, expr: TExpr) Error!Func {
    // set up context
    var func = try Func.init(env.ally, scope, &.{}, expr.ty);

    // lower expr as a function
    var block = try func.addBlock(env.ally);
    const final = try lowerExpr(env, &func, &block, expr);

    // TODO add return stmt
    _ = final;

    return func;
}
