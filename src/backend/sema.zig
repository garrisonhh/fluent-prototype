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

fn holeError(env: Env, loc: Loc, ty: TypeId) SemaError {
    const ty_text = try ty.writeAlloc(env.ally, env.typewelt.*);
    defer env.ally.free(ty_text);

    _ = try context.post(.note, loc, "this hole expects {s}", .{ty_text});
    return error.FluentError;
}

fn expectError(env: Env, loc: Loc, expected: TypeId, found: TypeId) SemaError {
    const exp_text = try expected.writeAlloc(env.ally, env.typewelt.*);
    const found_text = try found.writeAlloc(env.ally, env.typewelt.*);
    defer env.ally.free(exp_text);
    defer env.ally.free(found_text);

    const msg = try context.post(.err, loc, "expected type {s}", .{exp_text});
    _ = try msg.annotate(null, "found {s}", .{found_text});
    return error.FluentError;
}

fn typeOfNumber(env: *Env, num: SExpr.Number) SemaError!TypeId {
    if (num.bits) |bits| {
        // known width number
        return try env.typeIdentify(Type{
            .number = .{
                .bits = bits,
                .layout = num.data,
            }
        });
    }

    // compiler number
    return try env.typeIdentifyNumber(num.data, null);
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

fn analyzeDo(env: *Env, expr: SExpr, outward: TypeId) SemaError!TExpr {
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
    const unit = try env.typeIdentify(Type{ .unit = {} });

    const stmts = exprs[1..exprs.len - 1];
    for (stmts) |stmt, i| {
        errdefer for (texprs[0..i]) |texpr| texpr.deinit(ally);
        texprs[i] = try analyzeExpr(env, stmt, unit);
    }

    errdefer for (texprs[0..stmts.len]) |texpr| texpr.deinit(ally);

    // analyze return expr
    const final = exprs[exprs.len - 1];
    const ret_expr = &texprs[texprs.len - 1];
    ret_expr.* = try analyzeExpr(env, final, outward);

    // create TExpr
    const texpr = TExpr{
        .ty = ret_expr.ty,
        .loc = expr.loc,
        .data = .{ .do = texprs },
    };
    return try unifyTExpr(env, texpr, outward);
}

// badass error haha
fn badAsError(loc: Loc) SemaError {
    const text = "`as` expression requires a type and a body";
    _ = try context.post(.err, loc, text, .{});
    return error.FluentError;
}

fn analyzeAs(env: *Env, expr: SExpr, outward: TypeId) SemaError!TExpr {
    const exprs = expr.data.call;

    if (exprs.len != 3) return badAsError(expr.loc);

    const type_expr = exprs[1];
    const body_expr = exprs[2];

    // TODO this is a temporary hack before I can compile types by execution
    const anno_ty = if (type_expr.data == .symbol) sym: {
        break :sym env.getBound(type_expr.data.symbol).?.ty;
    } else @panic("TODO generalized type annotations");

    // generate inner expr
    const texpr = try analyzeExpr(env, body_expr, anno_ty);

    // TODO remove
    const tex = texpr.render(env.*, env.ally) catch unreachable;
    defer tex.deinit(env.ally);

    const anno_text = env.typeGet(anno_ty).writeAlloc(env.ally, env.typewelt.*) catch unreachable;
    defer env.ally.free(anno_text);

    const stdout = std.io.getStdOut().writer();
    stdout.print("`as` expr body (to {s}):\n", .{anno_text}) catch unreachable;
    tex.display(stdout) catch unreachable;

    // unify with outer expr
    return try unifyTExpr(env, texpr, outward);
}

fn analyzeList(env: *Env, expr: SExpr, outward: TypeId) SemaError!TExpr {
    const ally = env.ally;

    const elements = expr.data.call[1..];
    const texprs = try ally.alloc(TExpr, elements.len);
    errdefer ally.free(texprs);

    // determine type expectations
    const any = try env.typeIdentify(Type{ .any = {} });
    const expected = env.typeGet(outward);
    const elem_outward = if (expected.* == .list) expected.list else any;

    // analyze each element
    for (elements) |elem, i| {
        errdefer for (texprs[0..i]) |texpr| texpr.deinit(ally);
        texprs[i] = try analyzeExpr(env, elem, elem_outward);
    }

    // TODO list elements must be retyped again either here or as a verification
    // step to ensure consistency across the list

    // create TExpr
    const final_subty = if (texprs.len > 0) texprs[0].ty else elem_outward;
    const texpr = TExpr{
        .ty = try env.typeIdentify(Type{ .list = final_subty }),
        .loc = expr.loc,
        .data = .{ .list = texprs },
    };
    return unifyTExpr(env, texpr, outward);
}

fn analyzeCall(env: *Env, expr: SExpr, outward: TypeId) SemaError!TExpr{
    const ally = env.ally;
    const exprs = expr.data.call;
    const head = exprs[0];
    const tail = exprs[1..];

    if (head.data == .symbol) {
        // builtins require special type analysis
        if (fluent.Builtin.get(head.data.symbol)) |tag| {
            return switch (tag) {
                .do => try analyzeDo(env, expr, outward),
                .as => try analyzeAs(env, expr, outward),
                .list => try analyzeList(env, expr, outward),
            };
        }
    }

    const texprs = try ally.alloc(TExpr, exprs.len);
    errdefer ally.free(texprs);

    // analyze head
    const any = try env.typeIdentify(Type{ .any = {} });

    texprs[0] = try analyzeExpr(env, head, any);
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
        texprs[i + 1] = try analyzeExpr(env, tail[i], param_tid);
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
    const inner = env.typeGet(texpr.ty);

    // identify holes
    if (inner.* == .hole) {
        return holeError(env.*, texpr.loc, outward);
    }

    // check for already unified texprs
    const outer = env.typeGet(outward);
    const is_unified = switch (outer.*) {
        .any => true,
        .set => @panic("TODO unify type set"),
        else => texpr.ty.eql(outward)
    };

    if (is_unified) return texpr;

    // check for coercion
    if (try inner.coercesTo(env.typewelt, outer.*)) {
        // cast is possible
        const cloned = try texpr.clone(env.ally);
        return TExpr{
            .ty = outward,
            .loc = texpr.loc,
            .data = .{ .cast = try util.placeOn(env.ally, cloned) },
        };
    }

    // bad unification :(
    return expectError(env.*, texpr.loc, outward, texpr.ty);
}

fn analyzeExpr(env: *Env, expr: SExpr, outward: TypeId) SemaError!TExpr {
    // calls (and builtins)
    if (expr.data == .call) {
        return try analyzeCall(env, expr, outward);
    }

    // literals
    const texpr = TExpr{
        .ty = switch (expr.data) {
            .number => |num| try typeOfNumber(env, num),
            .symbol => try typeOfSymbol(env, expr),
            // string literals are (List u8)
            .string => try env.typeIdentify(Type{
                .list = try env.typeIdentify(Type{
                    .number = .{ .layout = .uint, .bits = 8 }
                })
            }),
            .call => unreachable
        },
        .loc = expr.loc,
        .data = try TExpr.Data.fromSExprData(env.ally, expr.data),
    };

    return unifyTExpr(env, texpr, outward);
}

/// after analysis, it's possible that types that fluent can't actually lower
/// are produced (e.g. Any and common type sets like Int). this
fn verifyDynamic(env: Env, texpr: TExpr) SemaError!void {
    // check that the class isn't analysis
    const class = env.typeGet(texpr.ty).classifyRuntime(env.typewelt.*);

    if (class == .analysis) {
        const ty_text = try texpr.ty.writeAlloc(env.ally, env.typewelt.*);
        defer env.ally.free(ty_text);

        const text = "inferred type `{s}`, which cannot be executed";
        _ = try context.post(.err, texpr.loc, text, .{ty_text});

        return error.FluentError;
    }

    // recurse
    for (texpr.getChildren()) |child| try verifyDynamic(env, child);
}

pub fn analyze(env: *Env, expr: SExpr, expects: TypeId) SemaError!TExpr {
    const texpr = try analyzeExpr(env, expr, expects);

    try verifyDynamic(env.*, texpr);

    return texpr;
}