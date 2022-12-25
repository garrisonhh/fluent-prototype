const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const util = @import("util");
const Symbol = util.Symbol;
const Name = util.Name;
const kz = @import("kritzler");
const context = @import("../context.zig");
const FluentError = context.FluentError;
const Loc = context.Loc;
const types = @import("types.zig");
const TypeId = types.TypeId;
const Type = types.Type;
const Env = @import("env.zig");
const SExpr = @import("sexpr.zig");
const TExpr = @import("texpr.zig");
const Number = @import("canon.zig").Number;
const eval = @import("eval.zig").eval;

pub const SemaError =
    Allocator.Error
 || context.MessageError
 || context.FluentError
 || Env.DefError;

fn holeError(env: Env, loc: ?Loc, ty: TypeId) SemaError {
    const ty_text = try ty.writeAlloc(env.ally, env.tw);
    defer env.ally.free(ty_text);

    _ = try context.post(.note, loc, "this hole expects {s}", .{ty_text});
    return error.FluentError;
}

fn expectError(env: Env, loc: ?Loc, expected: TypeId, found: TypeId) SemaError {
    const exp_text = try expected.writeAlloc(env.ally, env.tw);
    defer env.ally.free(exp_text);
    const found_text = try found.writeAlloc(env.ally, env.tw);
    defer env.ally.free(found_text);

    const msg = try context.post(.err, loc, "expected type {s}", .{exp_text});
    _ = try msg.annotate(null, "found {s}", .{found_text});
    return error.FluentError;
}

fn typeOfNumber(env: *Env, num: Number) SemaError!TypeId {
    return try env.identify(Type{
        .number = .{
            .bits = num.bits,
            .layout = num.data,
        }
    });
}

fn analyzeSymbol(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId
) SemaError!TExpr {
    const symbol = expr.data.symbol;

    // holes
    if (symbol.str[0] == '_') {
        return holeError(env.*, expr.loc, outward);
    }

    // normal symbol
    var name: Name = undefined;
    const ty = if (env.seek(scope, symbol, &name)) |value| value.ty else {
        _ = try context.post(.err, expr.loc, "unknown symbol `{}`", .{symbol});
        return error.FluentError;
    };

    const inner_expr = TExpr{
        .ty = ty,
        .loc = expr.loc,
        .data = .{ .name = name }
    };

    return try unifyTExpr(env, inner_expr, outward);
}

fn analyzeDo(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId
) SemaError!TExpr {
    const ally = env.ally;

    // verify form
    const exprs = expr.data.call;

    if (exprs.len < 2) {
        const text = "block requires at least one expression";
        _ = try context.post(.err, expr.loc, text, .{});
        return error.FluentError;
    }

    const texprs = try env.ally.alloc(TExpr, exprs.len);
    errdefer env.ally.free(texprs);

    // do expr
    const builtin_ty = try env.identify(Type{ .builtin = {} });
    texprs[0] = TExpr.initBuiltin(exprs[0].loc, builtin_ty, .do);

    // analyze statements
    const unit = try env.identify(Type{ .unit = {} });

    const stmts = exprs[1..exprs.len - 1];
    for (stmts) |stmt, i| {
        errdefer for (texprs[1..i + 1]) |texpr| texpr.deinit(ally);
        texprs[i + 1] = try analyzeExpr(env, scope, stmt, unit);
    }

    errdefer for (texprs[0..stmts.len]) |texpr| {
        texpr.deinit(ally);
    };

    // analyze return expr
    const final = exprs[exprs.len - 1];
    const ret_expr = &texprs[texprs.len - 1];
    ret_expr.* = try analyzeExpr(env, scope, final, outward);

    // create TExpr
    const texpr = TExpr{
        .ty = ret_expr.ty,
        .loc = expr.loc,
        .data = .{ .call = texprs }
    };
    return try unifyTExpr(env, texpr, outward);
}

fn analyzeAs(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId
) SemaError!TExpr {
    const exprs = expr.data.call;

    if (exprs.len != 3) {
        const text = "`as` expression requires a type and a body";
        _ = try context.post(.err, expr.loc, text, .{});
        return error.FluentError;
    }

    const type_expr = exprs[1];
    const body_expr = exprs[2];

    // compile type
    const type_val = try eval(env, scope, type_expr);

    const tyty = try env.identify(Type{ .ty = {} });
    if (!type_val.ty.eql(tyty)) {
        return expectError(env.*, type_val.loc, tyty, type_val.ty);
    }

    const anno = type_val.data.ty;

    // generate inner expr and unify
    const texpr = try analyzeExpr(env, scope, body_expr, anno);
    return try unifyTExpr(env, texpr, outward);
}

fn analyzeList(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId
) SemaError!TExpr {
    const ally = env.ally;

    const exprs = expr.data.call;
    const texprs = try ally.alloc(TExpr, exprs.len);
    errdefer ally.free(texprs);

    // list expr
    const builtin_ty = try env.identify(Type{ .builtin = {} });
    texprs[0] = TExpr.initBuiltin(exprs[0].loc, builtin_ty, .do);

    // determine type expectations
    const any = try env.identify(Type{ .any = {} });
    const expected = env.tw.get(outward);
    const elem_outward = if (expected.* == .list) expected.list else any;

    // analyze each element
    for (exprs[1..]) |elem, i| {
        errdefer for (texprs[1..i + 1]) |texpr| {
            texpr.deinit(ally);
        };
        texprs[i + 1] = try analyzeExpr(env, scope, elem, elem_outward);
    }

    // TODO list elements must be retyped again either here or as a verification
    // step to ensure consistency across the list

    // create TExpr
    const final_subty = if (texprs.len > 1) texprs[1].ty else elem_outward;
    const texpr = TExpr{
        .ty = try env.identify(Type{ .list = final_subty }),
        .loc = expr.loc,
        .data = .{ .call = texprs },
    };
    return unifyTExpr(env, texpr, outward);
}

fn analyzeCall(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId
) SemaError!TExpr{
    const ally = env.ally;
    const exprs = expr.data.call;
    const head = exprs[0];
    const tail = exprs[1..];

    if (head.data == .symbol) {
        // builtins require special type analysis
        const sym = head.data.symbol;

        if (sym.eql(comptime Symbol.init("do"))) {
            return try analyzeDo(env, scope, expr, outward);
        } else if (sym.eql(comptime Symbol.init("as"))) {
            return try analyzeAs(env, scope, expr, outward);
        } else if (sym.eql(comptime Symbol.init("list"))) {
            return try analyzeList(env, scope, expr, outward);
        }
    }

    const texprs = try ally.alloc(TExpr, exprs.len);
    errdefer ally.free(texprs);

    // analyze head
    const any = try env.identify(Type{ .any = {} });

    texprs[0] = try analyzeExpr(env, scope, head, any);
    errdefer texprs[0].deinit(ally);

    // get called expr type, ensure it is a function
    const fn_ty = env.tw.get(texprs[0].ty);
    if (fn_ty.* != .func) {
        const ty_text = try fn_ty.writeAlloc(ally, env.tw);
        defer ally.free(ty_text);

        const text = "expected function, found {s}";
        _ = try context.post(.err, texprs[0].loc, text, .{ty_text});
        return error.FluentError;
    }

    // analyze tail with fn param expectations
    const takes = fn_ty.func.takes;
    if (takes.len != tail.len) {
        const text = "expected {} parameters, found {}";
        _ = try context.post(.err, expr.loc, text, .{takes.len, tail.len});
        return error.FluentError;
    }

    // infer params
    for (takes) |tid, i| {
        errdefer for (texprs[1..i + 1]) |texpr| {
            texpr.deinit(ally);
        };

        texprs[i + 1] = try analyzeExpr(env, scope, tail[i], tid);
    }

    // create TExpr
    const texpr = TExpr{
        .ty = fn_ty.func.returns,
        .loc = expr.loc,
        .data = TExpr.Data{ .call = texprs },
    };
    return try unifyTExpr(env, texpr, outward);
}

/// given a TExpr, ensure that it will properly coerce to the outward
/// expectation
///
/// in practice, this should either:
/// a) do nothing (return the same expr)
/// b) make an implicit cast explicitly expressed in the AST
/// c) identify that an expectation was violated and produce an appropriate
///    error
fn unifyTExpr(env: *Env, texpr: TExpr, outward: TypeId) SemaError!TExpr {
    const inner = env.tw.get(texpr.ty);

    // check for already unified texprs
    const outer = env.tw.get(outward);
    const is_unified = switch (outer.*) {
        .any => true,
        .set => try inner.coercesTo(env.ally, &env.tw, outer.*),
        else => texpr.ty.eql(outward)
    };

    if (is_unified) return texpr;

    // check for coercion
    if (try inner.coercesTo(env.ally, &env.tw, outer.*)) {
        // cast is possible
        const builtin_ty = try env.identify(Type{ .builtin = {} });
        const cast = TExpr.initBuiltin(texpr.loc, builtin_ty, .cast);
        const final = try TExpr.initCall(env.ally, texpr.loc, outward, cast, 1);
        final.data.call[1] = try texpr.clone(env.ally);

        return final;
    }

    // bad unification :(
    return expectError(env.*, texpr.loc, outward, texpr.ty);
}

fn analyzeExpr(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId
) SemaError!TExpr {
    return switch (expr.data) {
        .call => try analyzeCall(env, scope, expr, outward),
        .symbol => try analyzeSymbol(env, scope, expr, outward),
        else => lit: {
            const texpr = TExpr{
                .ty = switch (expr.data) {
                    .@"bool" => try env.identify(Type{ .@"bool" = {} }),
                    .number => |num| try typeOfNumber(env, num),
                    // string literals are (List u8)
                    .string => try env.identify(Type{
                        .list = try env.identify(Type{
                            .number = .{ .layout = .uint, .bits = 8 }
                        })
                    }),
                    .call, .symbol => unreachable
                },
                .loc = expr.loc,
                .data = try TExpr.Data.fromSExprData(env.ally, expr.data),
            };

            break :lit unifyTExpr(env, texpr, outward);
        }
    };
}

/// flattens `do` blocks with one expression into the expression
fn pruneDoBlocks(env: Env, texpr: *TExpr) SemaError!void {
    if (texpr.data == .call and texpr.data.call[0].isBuiltin(.do)) {
        const xs = texpr.data.call;
        if (xs.len == 2) {
            const child = xs[1];
            env.ally.free(xs);

            texpr.* = child;
        }
    }

    for (texpr.getChildren()) |*child| {
        try pruneDoBlocks(env, child);
    }
}

/// after analysis, it's possible that types that fluent can't actually lower
/// are produced (e.g. Any and common type sets like Int). this
fn verifyDynamic(env: Env, texpr: TExpr) SemaError!void {
    // check that the class isn't analysis
    const class = env.tw.get(texpr.ty).classifyRuntime(env.tw);

    if (class == .analysis) {
        const ty_text = try texpr.ty.writeAlloc(env.ally, env.tw);
        defer env.ally.free(ty_text);

        const text = "inferred type `{s}`, which cannot be executed";
        _ = try context.post(.err, texpr.loc, text, .{ty_text});

        return error.FluentError;
    }

    // recurse
    for (texpr.getChildren()) |child| {
        try verifyDynamic(env, child);
    }
}

pub fn analyze(
    env: *Env,
    scope: Name,
    expr: SExpr,
    expects: TypeId
) SemaError!TExpr {
    var texpr = try analyzeExpr(env, scope, expr, expects);

    // postprocessing
    try pruneDoBlocks(env.*, &texpr);
    try verifyDynamic(env.*, texpr);

    return texpr;
}