const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const util = @import("util");
const Symbol = util.Symbol;
const Name = util.Name;
const NameMap = util.NameMap;
const Loc = util.Loc;
const Message = util.Message;
const kz = @import("kritzler");
const types = @import("types.zig");
const TypeId = types.TypeId;
const Type = types.Type;
const Env = @import("env.zig");
const SExpr = @import("sexpr.zig");
const TExpr = @import("texpr.zig");
const canon = @import("canon.zig");
const Number = canon.Number;
const Builtin = canon.Builtin;
const eval = @import("eval.zig");
const postprocess = @import("sema/postprocessing.zig").postprocess;

pub const SemaError = eval.Error;

const Result = Message.Result(TExpr);

fn holeError(env: Env, loc: ?Loc, ty: TypeId) Allocator.Error!Result {
    const ty_text = try ty.writeAlloc(env.ally, env.tw);
    defer env.ally.free(ty_text);

    const fmt = "this hole expects {s}";
    return try Message.err(env.ally, TExpr, loc, fmt, .{ty_text});
}

fn expectError(
    env: Env,
    loc: ?Loc,
    expected: TypeId,
    found: TypeId
) Allocator.Error!Result {
    const exp_text = try expected.writeAlloc(env.ally, env.tw);
    defer env.ally.free(exp_text);
    const found_text = try found.writeAlloc(env.ally, env.tw);
    defer env.ally.free(found_text);

    const fmt = "expected {s}, found {s}";
    return try Message.err(env.ally, TExpr, loc, fmt, .{exp_text, found_text});
}

fn wrongNArgsError(
    ally: Allocator,
    loc: ?Loc,
    expected: usize,
    found: usize
) Allocator.Error!Result {
    const fmt = "expected {} parameters, found {}";
    return try Message.err(ally, TExpr, loc, fmt, .{expected, found});
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
) SemaError!Result {
    const symbol = expr.data.symbol;

    // holes
    if (symbol.str[0] == '_') {
        return try holeError(env.*, expr.loc, outward);
    }

    // normal symbol
    var name: Name = undefined;
    const ty = if (env.seek(scope, symbol, &name)) |value| value.ty else {
        const fmt = "unknown symbol `{}`";
        return try Message.err(env.ally, TExpr, expr.loc, fmt, .{symbol});
    };

    const inner_expr = TExpr.init(expr.loc, false, ty, .{ .name = name });
    return try unifyTExpr(env, inner_expr, outward);
}

fn analyzeAs(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId
) SemaError!Result {
    const exprs = expr.data.call;

    if (exprs.len != 3) {
        const fmt = "`as` expression requires a type and a body";
        return try Message.err(env.ally, TExpr, expr.loc, fmt, .{});
    }

    const type_expr = exprs[1];
    const body_expr = exprs[2];

    // compile type
    const tyty = try env.identify(Type{ .ty = {} });
    const type_res = try eval.evalTyped(env, scope, type_expr, tyty);
    const type_val = type_res.get() orelse return type_res;
    defer type_val.deinit(env.ally);

    const anno = type_val.data.ty;

    // generate inner expr and unify
    const res = try analyzeExpr(env, scope, body_expr, anno);
    const texpr = res.get() orelse return res;
    return try unifyTExpr(env, texpr, outward);
}

fn analyzeAddrOf(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId
) SemaError!Result {
    const ally = env.ally;
    const exprs = expr.data.call;

    if (exprs.len != 2) {
        const fmt = "`&` expression expects a single argument";
        return try Message.err(ally, TExpr, expr.loc, fmt, .{});
    }

    // analyze subexpr with any expectations I can pass through
    const outer = env.tw.get(outward);
    const expect =
        if (outer.* == .ptr and outer.ptr.kind == .single)
            outer.ptr.to
        else
            try env.identify(Type{ .any = {} });

    const sub_res = try analyzeExpr(env, scope, exprs[1], expect);
    const subexpr = sub_res.get() orelse return sub_res;

    // generate this texpr
    const ptr_ty = try env.identify(Type.initPtr(.single, subexpr.ty));

    const texpr = TExpr.init(expr.loc, false, ptr_ty, TExpr.Data{
        .ptr = try util.placeOn(ally, subexpr)
    });
    return try unifyTExpr(env, texpr, outward);
}

fn analyzeDo(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId
) SemaError!Result {
    const ally = env.ally;
    const exprs = expr.data.call;

    // verify form
    if (exprs.len < 2) {
        const fmt = "`do` block requires at least one expression";
        return try Message.err(ally, TExpr, expr.loc, fmt, .{});
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
        const res = try analyzeExpr(env, scope, stmt, unit);
        texprs[i + 1] = res.get() orelse return res;
    }

    errdefer for (texprs[0..stmts.len]) |texpr| {
        texpr.deinit(ally);
    };

    // analyze return expr
    const final = exprs[exprs.len - 1];
    const ret_expr = &texprs[texprs.len - 1];
    const ret_res = try analyzeExpr(env, scope, final, outward);
    ret_expr.* = ret_res.get() orelse return ret_res;

    // create TExpr
    const texpr = TExpr.init(expr.loc, false, ret_expr.ty, .{ .call = texprs });
    return try unifyTExpr(env, texpr, outward);
}

fn analyzeIf(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId
) SemaError!Result {
    const ally = env.ally;
    const exprs = expr.data.call;

    // verify form
    if (exprs.len != 4) {
        const fmt = "`if` statement requires a condition and two branches";
        return try Message.err(ally, TExpr, expr.loc, fmt, .{});
    }

    // analyze children
    const builtin_ty = try env.identify(Type{ .builtin = {} });
    const @"bool" = try env.identify(Type{ .@"bool" = {} });

    var texprs: [4]TExpr = undefined;
    texprs[0] = TExpr.initBuiltin(exprs[0].loc, builtin_ty, .@"if");

    const expects = [_]TypeId{@"bool", outward, outward};
    var i: usize = 0;
    while (i < 3) : (i += 1) {
        const res = try analyzeExpr(env, scope, exprs[i + 1], expects[i]);
        texprs[i + 1] = res.get() orelse return res;
    }

    // TODO peer resolve branches, could also use peer type resolution in arrays

    return Result.ok(try TExpr.initCall(ally, expr.loc, outward, &texprs));
}

fn analyzeArray(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId
) SemaError!Result {
    const ally = env.ally;

    const exprs = expr.data.array;
    const texprs = try ally.alloc(TExpr, exprs.len);
    errdefer ally.free(texprs);

    // determine type expectations
    const any = try env.identify(Type{ .any = {} });
    const expected = env.tw.get(outward);
    const elem_outward = if (expected.* == .array) expected.array.of else any;

    // analyze each element
    for (exprs) |elem, i| {
        errdefer for (texprs[0..i]) |texpr| {
            texpr.deinit(ally);
        };
        const res = try analyzeExpr(env, scope, elem, elem_outward);
        texprs[i] = res.get() orelse return res;
    }

    // TODO array elements must be retyped again either here or as a
    // verification step to ensure consistency across the array

    // create TExpr
    const final_subty = if (texprs.len > 0) texprs[0].ty else elem_outward;
    const ty = try env.identify(Type{
        .array = Type.Array{
            .size = texprs.len,
            .of = final_subty,
        }
    });
    const texpr = TExpr.init(expr.loc, false, ty, .{ .array = texprs });
    return try unifyTExpr(env, texpr, outward);
}

fn filterDefError(
    ally: Allocator,
    loc: Loc,
    err: (SemaError || Env.DefError)
) SemaError!Result {
    return switch (err) {
        error.NameNoRedef => unreachable,
        error.NameTooLong => err: {
            const fmt = "name is nested too deeply in namespaces";
            break :err try Message.err(ally, TExpr, loc, fmt, .{});
        },
        error.NameRedef, error.RenamedType => err: {
            const fmt = "this name already exists";
            break :err try Message.err(ally, TExpr, loc, fmt, .{});
        },
        else => |e| @errSetCast(SemaError, e),
    };
}

const DeferredDef = struct {
    name: Name,
    ty: TypeId,
    body: SExpr,

    const Result = Message.Result(@This());
};

/// the first pass over a `def` declaration. evaluates type info and stores
/// a pie stone.
fn firstDefPass(
    env: *Env,
    scope: Name,
    expr: SExpr
) SemaError!DeferredDef.Result {
    const args = expr.data.call[1..];

    // check syntax form
    if (args.len != 3) {
        const fmt = "`def` expression requires a name, a type, and a body";
        return try Message.err(env.ally, DeferredDef, expr.loc, fmt, .{});
    }

    const name_expr = args[0];
    const type_expr = args[1];
    const body_expr = args[2];

    if (name_expr.data != .symbol) {
        const fmt = "expected identifier";
        return try Message.err(env.ally, DeferredDef, name_expr.loc, fmt, .{});
    }

    const symbol = name_expr.data.symbol;

    // eval type expr
    const tyty = try env.identify(Type{ .ty = {} });
    const type_res = try eval.evalTyped(env, scope, type_expr, tyty);
    const type_value = type_res.get() orelse return type_res.cast(DeferredDef);

    // store first pass info for the def
    const ty = type_value.data.ty;
    const pie_stone = TExpr.initBuiltin(expr.loc, ty, .pie_stone);
    const name = env.def(scope, symbol, pie_stone) catch |e| {
        return (try filterDefError(env.ally, expr.loc, e)).cast(DeferredDef);
    };

    return DeferredDef.Result.ok(DeferredDef{
        .name = name,
        .ty = ty,
        .body = body_expr,
    });
}

fn analyzeNamespace(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId
) SemaError!Result {
    const ally = env.ally;
    const args = expr.data.call[1..];

    // check syntax form
    if (args.len < 1) {
        const fmt = "`ns` expression requires a name";
        return try Message.err(ally, TExpr, expr.loc, fmt, .{});
    }

    const name_expr = args[0];
    const defs = args[1..];

    if (name_expr.data != .symbol) {
        const fmt = "expected identifier";
        return try Message.err(ally, TExpr, name_expr.loc, fmt, .{});
    }

    // create ns
    const symbol = name_expr.data.symbol;
    const ns = env.defNamespace(scope, symbol) catch |e| {
        return filterDefError(ally, name_expr.loc, e);
    };

    // first pass, collect all decls, eval types, and def pie stones
    const deferred = try ally.alloc(DeferredDef, defs.len);
    defer ally.free(deferred);

    for (defs) |def_expr, i| {
        // verify that this is a def
        if (def_expr.data != .call or def_expr.data.call.len == 0) {
            const fmt = "expected def";
            return try Message.err(ally, TExpr, def_expr.loc, fmt, .{});
        }

        const head = def_expr.data.call[0];
        const def_sym = comptime Symbol.init("def");
        if (head.data != .symbol or !head.data.symbol.eql(def_sym)) {
            const fmt = "expected def";
            return try Message.err(ally, TExpr, head.loc, fmt, .{});
        }

        // do first pass
        const deferred_res = try firstDefPass(env, ns, def_expr);
        deferred[i] = deferred_res.get() orelse return deferred_res.cast(TExpr);
    }

    // second pass, eval and redefine everything
    for (deferred) |dedef| {
        const res = try eval.evalTyped(env, dedef.name, dedef.body, dedef.ty);
        const texpr = res.get() orelse return res;

        if (texpr.isBuiltin(.pie_stone)) {
            std.debug.panic("pie stone!!!\n", .{});
        }

        env.redef(dedef.name, texpr) catch |e| {
            return try filterDefError(ally, dedef.body.loc, e);
        };
    }

    // evaluate to unit
    const unit = try env.identify(Type{ .unit = {} });
    const texpr = TExpr.init(expr.loc, true, unit, .{ .unit = {} });
    return try unifyTExpr(env, texpr, outward);
}

fn analyzeRecur(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId
) SemaError!Result {
    const ally = env.ally;

    // find innermost function type
    var name = scope;
    const ty = while (true) {
        const ty = env.get(name).ty;

        if (env.tw.get(ty).* == .func) {
            break ty;
        }

        name = name.drop() orelse {
            const fmt = "expected def";
            return try Message.err(ally, TExpr, expr.loc, fmt, .{});
        };
    };

    // return call to `recur` with proper type
    const sexprs = expr.data.call;
    const func = env.tw.get(ty).func;

    if (sexprs.len - 1 != func.takes.len) {
        return wrongNArgsError(ally, expr.loc, func.takes.len, sexprs.len - 1);
    }

    // analyze subexprs
    const texprs = try ally.alloc(TExpr, sexprs.len);
    texprs[0] = TExpr.initBuiltin(expr.loc, ty, .recur);

    for (sexprs[1..]) |child, i| {
        const res = try analyzeExpr(env, scope, child, func.takes[i]);
        texprs[i + 1] = res.get() orelse return res;
    }

    const final = TExpr.init(expr.loc, false, func.returns, .{
        .call = texprs
    });
    return try unifyTExpr(env, final, outward);
}

/// analyze a call to a specific builtin
fn analyzeBuiltin(
    env: *Env,
    scope: Name,
    expr: SExpr,
    b: Builtin,
    outward: TypeId,
) SemaError!Result {
    return switch (b) {
        .pie_stone => unreachable,
        .cast => try analyzeAs(env, scope, expr, outward),
        .addr_of => try analyzeAddrOf(env, scope, expr, outward),
        .def => def: {
            const fmt =
                "`def` declarations can only exist inside of a namespace.";
            break :def try Message.err(env.ally, TExpr, expr.loc, fmt, .{});
        },
        .do => try analyzeDo(env, scope, expr, outward),
        .ns => try analyzeNamespace(env, scope, expr, outward),
        .@"fn" => try analyzeFn(env, scope, expr, outward),
        .recur => try analyzeRecur(env, scope, expr, outward),
        .@"if" => try analyzeIf(env, scope, expr, outward),
        else => {
            std.debug.panic("TODO analyze builtin `{s}`", .{@tagName(b)});
        },
    };
}

/// `fn` is a lambda expression taking an array of a number of unbound symbols
/// and a body expression.
fn analyzeFn(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId
) SemaError!Result {
    const outer = env.tw.get(outward);
    if (outer.* != .func) {
        @panic("TODO compile lambdas without expectations?");
    }

    const func = outer.func;

    // check syntax form
    const exprs = expr.data.call;

    if (exprs.len != 3) {
        const fmt = "`fn` requires parameters and a single body expression";
        return try Message.err(env.ally, TExpr, expr.loc, fmt, .{});
    }

    const params_expr = exprs[1];
    if (params_expr.data != .array) {
        const fmt = "`fn` parameters expects an array";
        const loc = params_expr.loc;
        return try Message.err(env.ally, TExpr, loc, fmt, .{});
    }

    const params = params_expr.data.array;
    if (params.len != func.takes.len) {
        return try Message.err(
            env.ally,
            TExpr,
            params_expr.loc,
            "found {d} parameters, expected {d}",
            .{params.len, func.takes.len}
        );
    }

    // define function as a namespace
    const local_sym = comptime Symbol.init("lambda");
    const local = env.defNamespace(scope, local_sym) catch |e| {
        return try filterDefError(env.ally, expr.loc, e);
    };

    // declare params
    for (params) |param, i| {
        if (param.data != .symbol) {
            const fmt = "`fn` parameters should be symbols";
            return try Message.err(env.ally, TExpr, param.loc, fmt, .{});
        }

        // define parameter
        const sym = param.data.symbol;
        const ty = func.takes[i];
        const pexpr = TExpr.init(param.loc, false, ty, .{
            .param = TExpr.Param{ .func = local, .index = i }
        });
        _ = env.def(local, sym, pexpr) catch |e| {
            return try filterDefError(env.ally, param.loc, e);
        };
    }

    // analyze body
    const body_res = try analyzeExpr(env, local, exprs[2], func.returns);
    const body = body_res.get() orelse return body_res;
    const final = TExpr.init(expr.loc, false, outward, .{
        .func = TExpr.Func{
            .name = local,
            .body = try util.placeOn(env.ally, body),
        }
    });

    return Result.ok(final);
}

fn analyzeCall(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId
) SemaError!Result {
    const ally = env.ally;
    const exprs = expr.data.call;

    // check for unit expr `()`
    if (exprs.len == 0) {
        const unit = try env.identify(Type{ .unit = {} });
        const texpr = TExpr.init(expr.loc, true, unit, .{ .unit = {} });
        return try unifyTExpr(env, texpr, outward);
    }

    const head = exprs[0];
    const tail = exprs[1..];

    // analyze head
    const any = try env.identify(Type{ .any = {} });
    const flbuiltin = try env.identify(Type{ .builtin = {} });

    const head_res = try analyzeExpr(env, scope, head, any);
    const head_expr = head_res.get() orelse return head_res;
    errdefer head_expr.deinit(ally);

    // builtins get special logic
    if (head_expr.ty.eql(flbuiltin)) {
        std.debug.assert(head_expr.data == .name);

        const bound = env.get(head_expr.data.name);
        const b = bound.data.builtin;
        return try analyzeBuiltin(env, scope, expr, b, outward);
    }

    // analyze a regular call
    const texprs = try ally.alloc(TExpr, exprs.len);
    errdefer ally.free(texprs);

    texprs[0] = head_expr;

    // get called expr type, ensure it is a function
    const fn_ty = env.tw.get(texprs[0].ty);
    if (fn_ty.* != .func) {
        const ty_text = try fn_ty.writeAlloc(ally, env.tw);
        defer ally.free(ty_text);

        const fmt = "expected function, found {s}";
        return try Message.err(ally, TExpr, texprs[0].loc, fmt, .{ty_text});
    }

    // analyze tail with fn param expectations
    const takes = fn_ty.func.takes;
    if (takes.len != tail.len) {
        return wrongNArgsError(ally, expr.loc, takes.len, tail.len);
    }

    // infer params
    for (takes) |tid, i| {
        errdefer for (texprs[1..i + 1]) |texpr| {
            texpr.deinit(ally);
        };

        const res = try analyzeExpr(env, scope, tail[i], tid);
        texprs[i + 1] = res.get() orelse return res;
    }

    // create TExpr
    const ty = fn_ty.func.returns;
    const texpr = TExpr.init(expr.loc, false, ty, .{ .call = texprs });
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
fn unifyTExpr(
    env: *Env,
    texpr: TExpr,
    outward: TypeId
) SemaError!Result {
    const inner = env.tw.get(texpr.ty);
    const outer = env.tw.get(outward);

    // check for already unified texprs
    const is_unified = switch (outer.*) {
        .any => true,
        .set => try inner.coercesTo(env.ally, &env.tw, outer.*),
        else => texpr.ty.eql(outward)
    };

    if (is_unified) return Result.ok(texpr);

    // types aren't unified, check whether an implicit cast is allowed
    // TODO this is quickly revealing itself to be a source of complexity that
    // gets shunted down to the lowering and compiling stages. basically this
    // code inserts `cast` operations whenever type coercion is allowed. this
    // works well in many cases, but sema should handle the responsibility of
    // adapting to type information, and I should get rid of the idea of a
    // generic `cast` operation in the lower-level ends of the fluent compiler
    // and replace it with more accurate ideas like `bitcast` `sign-extend`
    // `truncate` etc.
    if (try inner.coercesTo(env.ally, &env.tw, outer.*)) {
        // cast is possible
        const builtin_ty = try env.identify(Type{ .builtin = {} });
        const cast = TExpr.initBuiltin(texpr.loc, builtin_ty, .cast);
        const final = try TExpr.initCall(
            env.ally,
            texpr.loc,
            outward,
            &[2]TExpr{cast, texpr}
        );

        return Result.ok(final);
    }

    // no unification :(
    return expectError(env.*, texpr.loc, outward, texpr.ty);
}

fn analyzeExpr(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId
) SemaError!Result {
    return switch (expr.data) {
        .call => try analyzeCall(env, scope, expr, outward),
        .array => try analyzeArray(env, scope, expr, outward),
        .symbol => try analyzeSymbol(env, scope, expr, outward),
        else => lit: {
            // construct equivalent literal TExpr
            const ty = switch (expr.data) {
                .@"bool" => try env.identify(Type{ .@"bool" = {} }),
                .number => |num| try typeOfNumber(env, num),
                // string literals are (Array N u8)
                .string => |str| try env.identify(Type{
                    .array = Type.Array{
                        .size = str.str.len,
                        .of = try env.identify(Type{
                            .number = .{ .layout = .uint, .bits = 8 }
                        }),
                    }
                }),
                .call, .array, .symbol => unreachable,
            };
            const data = switch (expr.data) {
                .@"bool" => |val| TExpr.Data{ .@"bool" = val },
                .number => |num| TExpr.Data{ .number = num },
                .string => |sym| TExpr.Data{
                    .string = try sym.clone(env.ally)
                },
                else => unreachable
            };

            const texpr = TExpr.init(expr.loc, true, ty, data);
            break :lit unifyTExpr(env, texpr, outward);
        }
    };
}

/// flattens `do` blocks with one expression into the expression
fn pruneDoBlocks(env: Env, texpr: *TExpr) void {
    for (texpr.getChildren()) |*child| {
        pruneDoBlocks(env, child);
    }

    if (texpr.data == .call and texpr.data.call[0].isBuiltin(.do)) {
        const xs = texpr.data.call;
        if (xs.len == 2) {
            const child = xs[1];
            env.ally.free(xs);

            texpr.* = child;
        }
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

        const fmt = "inferred type `{s}`, which cannot be executed";
        return try Message.err(env.ally, TExpr, texpr.loc, fmt, .{ty_text});
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
) SemaError!Result {
    const res = try analyzeExpr(env, scope, expr, expects);
    var texpr = res.get() orelse return res;

    const pp = try postprocess(env, &texpr);
    pp.get() orelse return pp.cast(TExpr);

    // TODO verify texpr contains no pie stones as a postprocessing step

    return Result.ok(texpr);
}