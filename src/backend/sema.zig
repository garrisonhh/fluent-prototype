const std = @import("std");
const util = @import("util");
const kz = @import("kritzler");
const context = @import("../context.zig");
const types = @import("types.zig");
const fluent = @import("fluent.zig");
const Env = @import("env.zig");
const SExpr = @import("sexpr.zig");
const TExpr = @import("texpr.zig");

const Allocator = std.mem.Allocator;
const Loc = context.Loc;
const TypeId = types.TypeId;
const Type = types.Type;
const Pattern = types.Pattern;
const FluentError = context.FluentError;
const Symbol = util.Symbol;

pub const SemaError =
    Allocator.Error
 || context.MessageError
 || context.FluentError;

fn holeError(env: Env, loc: Loc, ty: Type) SemaError {
    const ty_text = try ty.writeAlloc(env.ally, env.typewelt.*);
    defer env.ally.free(ty_text);

    _ = try context.post(.note, loc, "this hole expects {s}", .{ty_text});
    return error.FluentError;
}

fn expectError(env: Env, loc: Loc, expected: Type, found: Type) SemaError {
    const exp_text = try expected.writeAlloc(env.ally, env.typewelt.*);
    const found_text = try found.writeAlloc(env.ally, env.typewelt.*);
    defer env.ally.free(exp_text);
    defer env.ally.free(found_text);

    const msg = try context.post(.err, loc, "expected {s}", .{exp_text});
    _ = try msg.annotate(null, "found {s}", .{found_text});
    return error.FluentError;
}

fn typeOfNumber(env: *Env, num: SExpr.Number) SemaError!TypeId {
    if (num.bits) |bits| {
        return try env.typeIdentify(Type{
            .number = .{
                .bits = bits,
                .layout = num.data,
            }
        });
    } else {
        // TODO there has to be a better way to do this, lol
        const typename = switch (num.data) {
            .int => comptime Symbol.init("Int"),
            .uint => comptime Symbol.init("UInt"),
            .float => comptime Symbol.init("Float"),
        };

        return env.getBound(typename).?.ty;
    }
}

fn typeOfSymbol(env: *Env, expr: SExpr) SemaError!TypeId {
    const symbol = expr.data.symbol;

    // holes
    if (symbol.str[0] == '_') {
        return try env.typeIdentify(Type{ .hole = {} });
    }

    // normal symbol
    return env.getType(symbol) orelse {
        _ = try context.post(.err, expr.loc, "unknown symbol `{}`", .{symbol});
        return error.FluentError;
    };
}

fn analyzeDo(env: *Env, expr: SExpr, expects: Type) SemaError!TExpr {
    const ally = env.ally;

    // verify form
    const exprs = expr.data.call;

    if (exprs.len < 2) {
        const text = "block requires at least one expression";
        _ = try context.post(.err, expr.loc, text, .{});
        return error.FluentError;
    }

    const texprs = try ally.alloc(TExpr, exprs.len - 1);
    errdefer ally.free(texprs);

    // analyze statements
    const stmts = exprs[1..exprs.len - 1];
    for (stmts) |stmt, i| {
        errdefer for (texprs[0..i]) |texpr| texpr.deinit(ally);
        texprs[i] = try analyze(env, stmt, Type{ .unit = {} });
    }

    errdefer for (texprs[0..stmts.len]) |texpr| texpr.deinit(ally);

    // analyze return expr
    const final = exprs[exprs.len - 1];
    const ret_expr = &texprs[texprs.len - 1];
    ret_expr.* = try analyze(env, final, expects);

    // create TExpr
    const data = TExpr.Data{ .do = texprs };
    return try unifyTExpr(env, expr.loc, data, ret_expr.ty, expects);
}

fn analyzeList(env: *Env, expr: SExpr, expects: Type) SemaError!TExpr {
    const ally = env.ally;

    const elements = expr.data.call[1..];
    const texprs = try ally.alloc(TExpr, elements.len);
    errdefer ally.free(texprs);

    var i: usize = 0;

    // determine type of the list elements
    const elem_tid = if (expects == .list) expects.list else expect_fst: {
        // use the first element to determine expectation
        texprs[i] = try analyze(env, elements[i], expects);
        i += 1;

        break :expect_fst texprs[0].ty;
    };

    const elem_ty = env.typeGet(elem_tid);

    // analyze each element
    while (i < elements.len) : (i += 1) {
        errdefer for (texprs[0..i]) |texpr| texpr.deinit(ally);
        texprs[i] = try analyze(env, elements[i], elem_ty.*);
    }

    // create TExpr
    const list_tid = try env.typeIdentify(Type{ .list = elem_tid });
    const data = TExpr.Data{ .list = texprs };
    return unifyTExpr(env, expr.loc, data, list_tid, expects);
}

fn analyzeCall(env: *Env, expr: SExpr, expects: Type) SemaError!TExpr{
    const ally = env.ally;
    const exprs = expr.data.call;
    const head = exprs[0];
    const tail = exprs[1..];

    if (head.data == .symbol) {
        // builtins require special type analysis
        if (fluent.Builtin.get(head.data.symbol)) |tag| {
            return switch (tag) {
                .do => try analyzeDo(env, expr, expects),
                .list => try analyzeList(env, expr, expects),
            };
        }

        // regular call
        const texprs = try ally.alloc(TExpr, exprs.len);
        errdefer ally.free(texprs);

        // analyze head
        texprs[0] = try analyze(env, head, Type{ .any = {} });
        errdefer texprs[0].deinit(ally);

        const fn_ty = env.typeGet(texprs[0].ty);
        if (fn_ty.* != .func) {
            const ty_text = try fn_ty.writeAlloc(ally, env.typewelt.*);
            defer ally.free(ty_text);

            const text = "expected function, found {s}";
            _ = try context.post(.err, texprs[0].loc, text, .{ty_text});
            return error.FluentError;
        }

        // analyze tail with param expectations
        const param_tys = fn_ty.func.takes;
        if (param_tys.len != tail.len) {
            _ = try context.post(
                .err,
                expr.loc,
                "expected {} parameters, found {}",
                .{param_tys.len, tail.len}
            );
            return error.FluentError;
        }

        for (param_tys) |param_tid, i| {
            errdefer for (texprs[1..i + 1]) |texpr| texpr.deinit(ally);

            const param_expects = env.typeGet(param_tid);
            texprs[i + 1] = try analyze(env, tail[i], param_expects.*);
        }

        // create TExpr
        const data = TExpr.Data{ .call = texprs };
        return unifyTExpr(env, expr.loc, data, fn_ty.func.returns, expects);
    } else {
        @panic("TODO lambdas and other function expressions");
    }
}

/// unifies type, constructs TExpr, and provides consistent errors
fn unifyTExpr(
    env: *Env,
    loc: Loc,
    data: TExpr.Data,
    inward: TypeId,
    expects: Type
) SemaError!TExpr {
    errdefer data.deinit(env.ally);

    const in = env.typeGet(inward);

    // create an error for holes
    if (in.* == .hole) {
        return holeError(env.*, loc, expects);
    } else if (expects == .hole) {
        return holeError(env.*, loc, in.*);
    }

    // unify inward and outward types
    const outward = try env.typeIdentify(expects);
    const ty = (try env.typeUnify(outward, inward)) orelse {
        return expectError(env.*, loc, expects, env.typeGet(inward).*);
    };

    return TExpr{
        .data = data,
        .loc = loc,
        .ty = ty,
    };
}

fn analyzeExpr(env: *Env, expr: SExpr, expects: Type) SemaError!TExpr {
    // calls (and builtins)
    if (expr.data == .call) {
        return try analyzeCall(env, expr, expects);
    }

    // literals
    const inward = switch (expr.data) {
        .number => |num| try typeOfNumber(env, num),
        .symbol => try typeOfSymbol(env, expr),
        .string => try env.typeIdentify(Type{
            .list = try env.typeIdentify(Type{
                .number = .{ .layout = .uint, .bits = 8 }
            })
        }),
        .call => unreachable
    };

    const data = try TExpr.Data.fromSExprData(env.ally, expr.data);
    return unifyTExpr(env, expr.loc, data, inward, expects);
}

/// after analysis, it's possible that types that fluent can't actually lower
/// are produced (e.g. Any and common type sets like Int). this
fn verifyDynamic(env: *Env, texpr: TExpr) SemaError!void {
    const ty = env.typeGet(texpr.ty);

    if (ty.classifyRuntime(env.typewelt.*) == .analysis) {
        const ty_text = try ty.writeAlloc(env.ally, env.typewelt.*);
        defer env.ally.free(ty_text);

        const text = "inferred type `{s}`, which cannot be executed";
        _ = try context.post(.err, texpr.loc, text, .{ty_text});

        return error.FluentError;
    }
}

pub fn analyze(env: *Env, expr: SExpr, expects: Type) SemaError!TExpr {
    const texpr = try analyzeExpr(env, expr, expects);

    try texpr.traverse(env, SemaError, verifyDynamic);

    return texpr;
}