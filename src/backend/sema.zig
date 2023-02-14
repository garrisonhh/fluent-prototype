const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const com = @import("common");
const Symbol = com.Symbol;
const Name = com.Name;
const Loc = com.Loc;
const Message = com.Message;
const Env = @import("env.zig");
const Id = Env.Id;
const SExpr = @import("sexpr.zig");
const TExpr = @import("texpr.zig");
const canon = @import("canon.zig");
const TypeId = canon.TypeId;
const Type = canon.Type;
const Number = canon.Number;
const Builtin = canon.Builtin;
const eval = @import("eval.zig");
const postprocess = @import("sema/postprocessing.zig").postprocess;

pub const SemaError = eval.Error;

const Result = Message.Result(Id);

fn err(
    ally: Allocator,
    loc: ?Loc,
    comptime fmt: []const u8,
    args: anytype,
) Allocator.Error!Result {
    return Result.err(try Message.print(ally, .@"error", loc, fmt, args));
}

fn holeError(env: Env, loc: ?Loc, ty: TypeId) Allocator.Error!Result {
    const ty_text = try ty.toString(env.ally, env.tw);
    defer env.ally.free(ty_text);

    const fmt = "this hole expects {s}";
    return try err(env.ally, loc, fmt, .{ty_text});
}

fn expectError(
    env: Env,
    loc: ?Loc,
    expected: TypeId,
    found: TypeId,
) Allocator.Error!Result {
    const exp_text = try expected.toString(env.ally, env.tw);
    defer env.ally.free(exp_text);
    const found_text = try found.toString(env.ally, env.tw);
    defer env.ally.free(found_text);

    const fmt = "expected {s}, found {s}";
    return try err(env.ally, loc, fmt, .{ exp_text, found_text });
}

fn wrongNArgsError(
    ally: Allocator,
    loc: ?Loc,
    expected: usize,
    found: usize,
) SemaError!Result {
    const fmt = "expected {} parameters, found {}";
    return try err(ally, loc, fmt, .{ expected, found });
}

fn analyzeSymbol(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId,
) SemaError!Result {
    const symbol = expr.data.symbol;

    // holes
    if (symbol.str[0] == '_') {
        return try holeError(env.*, expr.loc, outward);
    }

    // normal symbol
    const stored = env.seek(scope, symbol, null) orelse {
        const fmt = "unknown symbol `{}`";
        return try err(env.ally, expr.loc, fmt, .{symbol});
    };

    const id = try env.new(expr.loc, env.get(stored).ty, .{ .alias = stored });
    return try coerce(env, id, outward);
}

fn analyzeAs(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId,
) SemaError!Result {
    const exprs = expr.data.call;

    if (exprs.len != 3) {
        const fmt = "`as` expression requires a type and a body";
        return try err(env.ally, expr.loc, fmt, .{});
    }

    const type_expr = exprs[1];
    const body_expr = exprs[2];

    // compile type
    const tyty = try env.identify(Type{ .ty = {} });
    const type_res = try eval.evalTyped(env, scope, type_expr, tyty);
    const type_val = type_res.get() orelse return type_res;
    defer env.del(type_val);

    const anno = env.get(type_val).data.ty;

    // generate inner expr and unify
    const res = try analyzeExpr(env, scope, body_expr, anno);
    const texpr = res.get() orelse return res;
    return try coerce(env, texpr, outward);
}

fn analyzeAddrOf(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId,
) SemaError!Result {
    const ally = env.ally;
    const exprs = expr.data.call;

    if (exprs.len != 2) {
        const fmt = "`&` expression expects a single argument";
        return try err(ally, expr.loc, fmt, .{});
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
    const ptr_ty = try env.identify(Type.initPtr(.single, env.get(subexpr).ty));

    const texpr = try env.new(expr.loc, ptr_ty, .{ .ptr = subexpr });
    return try coerce(env, texpr, outward);
}

fn analyzeDo(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId,
) SemaError!Result {
    const ally = env.ally;
    const exprs = expr.data.call;

    // verify form
    if (exprs.len < 2) {
        const fmt = "`do` block requires at least one expression";
        return try err(ally, expr.loc, fmt, .{});
    }

    const block = try ally.alloc(Id, exprs.len);
    errdefer ally.free(block);

    // do expr
    const builtin_ty = try env.identify(.builtin);
    block[0] = try env.new(exprs[0].loc, builtin_ty, .{ .builtin = .do });

    // analyze statements
    const unit = try env.identify(.unit);

    const stmts = exprs[1 .. exprs.len - 1];
    for (stmts) |stmt, i| {
        errdefer for (block[1 .. i + 1]) |id| env.del(id);
        const res = try analyzeExpr(env, scope, stmt, unit);
        block[i + 1] = res.get() orelse return res;
    }

    errdefer for (block[0..stmts.len]) |id| env.del(id);

    // analyze return expr
    const ret_sexpr = exprs[exprs.len - 1];
    const ret_res = try analyzeExpr(env, scope, ret_sexpr, outward);
    const ret_expr = ret_res.get() orelse return ret_res;
    block[block.len - 1] = ret_expr;

    // create TExpr
    const ty = env.get(ret_expr).ty;
    const final = try env.new(expr.loc, ty, .{ .call = block });
    return try coerce(env, final, outward);
}

fn analyzeIf(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId,
) SemaError!Result {
    const ally = env.ally;
    const exprs = expr.data.call;

    // verify form
    if (exprs.len != 4) {
        const fmt = "`if` statement requires a condition and two branches";
        return try err(ally, expr.loc, fmt, .{});
    }

    // analyze children
    const builtin_ty = try env.identify(Type{ .builtin = {} });
    const @"bool" = try env.identify(Type{ .@"bool" = {} });

    var form: [4]Id = undefined;
    form[0] = try env.new(exprs[0].loc, builtin_ty, .{ .builtin = .@"if" });

    const expects = [_]TypeId{ @"bool", outward, outward };
    var i: usize = 0;
    while (i < 3) : (i += 1) {
        errdefer for (form[1 .. i + 1]) |id| env.del(id);
        const res = try analyzeExpr(env, scope, exprs[i + 1], expects[i]);
        form[i + 1] = res.get() orelse return res;
    }

    // TODO peer resolve branches, could also use peer type resolution in arrays

    const texpr = try TExpr.initCall(env.*, expr.loc, outward, &form);
    const final = try env.from(texpr);
    return Result.ok(final);
}

fn analyzeArray(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId,
) SemaError!Result {
    const ally = env.ally;

    const exprs = expr.data.call[1..];
    const array = try ally.alloc(Id, exprs.len);
    errdefer ally.free(array);

    // determine type expectations
    const any = try env.identify(.any);
    const expected = env.tw.get(outward);
    const elem_outward = if (expected.* == .array) expected.array.of else any;

    // analyze each element
    for (exprs) |elem, i| {
        errdefer for (array[0..i]) |texpr| env.del(texpr);
        const res = try analyzeExpr(env, scope, elem, elem_outward);
        array[i] = res.get() orelse return res;
    }

    // TODO array element types must be peer resolved

    // create TExpr
    const final_subty = if (array.len > 0) fst: {
        break :fst env.get(array[0]).ty;
    } else elem_outward;

    const ty = try env.identify(Type{ .array = Type.Array{
        .size = array.len,
        .of = final_subty,
    } });
    const final = try env.new(expr.loc, ty, .{ .array = array });
    return try coerce(env, final, outward);
}

fn filterDefError(
    ally: Allocator,
    loc: Loc,
    from_err: (SemaError || Env.DefError),
) SemaError!Result {
    return switch (from_err) {
        error.NameNoRedef => unreachable,
        error.NameTooLong => try err(ally, loc, "name is too long", .{}),
        error.NameRedef,
        error.RenamedType,
        => try err(ally, loc, "this name already exists", .{}),
        else => |e| @errSetCast(SemaError, e),
    };
}

fn analyzeNamespace(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId,
) SemaError!Result {
    _ = env;
    _ = scope;
    _ = expr;
    _ = outward;

    @panic("TODO analyzeNamespace");
}

fn analyzeRecur(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId,
) SemaError!Result {
    const ally = env.ally;

    // find innermost function type
    var name = scope;
    const ty = while (true) {
        const ty = env.get(env.getId(name)).ty;
        if (env.tw.get(ty).* == .func) {
            break ty;
        }

        name = name.drop() orelse {
            const fmt = "expected def";
            return try err(ally, expr.loc, fmt, .{});
        };
    };

    // return call to `recur` with proper type
    const sexprs = expr.data.call;
    const func = env.tw.get(ty).func;

    if (sexprs.len - 1 != func.takes.len) {
        return wrongNArgsError(ally, expr.loc, func.takes.len, sexprs.len - 1);
    }

    // analyze subexprs
    const recur = try ally.alloc(Id, sexprs.len);
    errdefer ally.free(recur);
    recur[0] = try env.new(expr.loc, ty, .{ .builtin = .recur });

    for (sexprs[1..]) |child, i| {
        errdefer for (recur[0..i]) |id| env.del(id);
        const res = try analyzeExpr(env, scope, child, func.takes[i]);
        recur[i + 1] = res.get() orelse return res;
    }

    const final = try env.new(expr.loc, func.returns, .{ .call = recur });
    return try coerce(env, final, outward);
}

fn analyzeBuiltinCall(
    env: *Env,
    scope: Name,
    expr: SExpr,
    b: Builtin,
    outward: TypeId,
) SemaError!Result {
    return switch (b) {
        .pie_stone => unreachable,
        .array => try analyzeArray(env, scope, expr, outward),
        .cast => try analyzeAs(env, scope, expr, outward),
        .addr_of => try analyzeAddrOf(env, scope, expr, outward),
        .def => def: {
            const fmt = "declarations can only exist inside of a namespace.";
            break :def try err(env.ally, expr.loc, fmt, .{});
        },
        .do => try analyzeDo(env, scope, expr, outward),
        .ns => try analyzeNamespace(env, scope, expr, outward),
        .lambda => try analyzeLambda(env, scope, expr, outward),
        .recur => try analyzeRecur(env, scope, expr, outward),
        .@"if" => try analyzeIf(env, scope, expr, outward),
        else => {
            std.debug.panic("TODO analyze builtin `{s}`", .{@tagName(b)});
        },
    };
}

fn analyzeLambda(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId,
) SemaError!Result {
    const outer = env.tw.get(outward);
    if (outer.* != .func) {
        @panic("TODO compile lambdas without expectations?");
    }

    const func = outer.func;

    // check syntax form
    const exprs = expr.data.call;

    if (exprs.len != 3) {
        const fmt = "`lambda` requires parameters and a single body expression";
        return try err(env.ally, expr.loc, fmt, .{});
    }

    if (builtin.mode == .Debug) {
        @panic("help");
    }

    // TODO check params
    const params_expr = exprs[1];
    if (false) {
        const fmt = "`lambda` requires parameters";
        const loc = params_expr.loc;
        return try err(env.ally, loc, fmt, .{});
    }

    const params = params_expr.data.call[1..];
    if (params.len != func.takes.len) {
        return Result.err(try Message.print(
            env.ally,
            .@"error",
            params_expr.loc,
            "found {d} parameters, expected {d}",
            .{ params.len, func.takes.len },
        ));
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
            return try err(env.ally, param.loc, fmt, .{});
        }

        // define parameter
        const sym = param.data.symbol;
        const ty = func.takes[i];
        const pexpr = TExpr.init(param.loc, false, ty, .{
            .param = TExpr.Param{ .func = local, .index = i },
        });
        _ = env.def(local, sym, pexpr) catch |e| {
            return try filterDefError(env.ally, param.loc, e);
        };
    }

    // analyze body
    const body_res = try analyzeExpr(env, local, exprs[2], func.returns);
    const body = body_res.get() orelse return body_res;
    const final = TExpr.init(expr.loc, false, outward, .{ .func = TExpr.Func{
        .name = local,
        .body = try com.placeOn(env.ally, body),
    } });

    return Result.ok(final);
}

fn analyzeCall(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId,
) SemaError!Result {
    const ally = env.ally;
    const exprs = expr.data.call;

    // check for unit expr `()`
    if (exprs.len == 0) {
        const unit = try env.identify(.unit);
        const id = try env.new(expr.loc, unit, .unit);
        return try coerce(env, id, outward);
    }

    const head = exprs[0];
    const tail = exprs[1..];

    // analyze head
    const any = try env.identify(Type{ .any = {} });
    const flbuiltin = try env.identify(Type{ .builtin = {} });

    const head_res = try analyzeExpr(env, scope, head, any);
    const head_id = head_res.get() orelse return head_res;
    const head_expr = env.get(head_id);
    errdefer env.del(head_id);

    // builtins get special logic
    if (head_expr.ty.eql(flbuiltin)) {
        std.debug.assert(head_expr.data == .alias);

        const alias = env.get(head_expr.data.alias);
        std.debug.assert(alias.data == .builtin);

        const b = alias.data.builtin;
        return try analyzeBuiltinCall(env, scope, expr, b, outward);
    }

    // analyze a regular call
    const call = try ally.alloc(Id, exprs.len);
    errdefer ally.free(call);

    call[0] = head_id;

    // get called expr type, ensure it is a function
    const fn_ty = env.tw.get(head_expr.ty);
    if (fn_ty.* != .func) {
        const ty_text = try fn_ty.toString(ally, env.tw);
        defer ally.free(ty_text);

        const fmt = "expected function, found {s}";
        return try err(ally, head_expr.loc, fmt, .{ty_text});
    }

    // analyze tail with fn param expectations
    const takes = fn_ty.func.takes;
    if (takes.len != tail.len) {
        return wrongNArgsError(ally, expr.loc, takes.len, tail.len);
    }

    // infer params
    for (takes) |tid, i| {
        errdefer for (call[1 .. i + 1]) |id| env.del(id);
        const res = try analyzeExpr(env, scope, tail[i], tid);
        call[i + 1] = res.get() orelse return res;
    }

    // create TExpr
    const ty = fn_ty.func.returns;
    const id = try env.new(expr.loc, ty, .{
        .call = call,
    });
    return try coerce(env, id, outward);
}

/// given a ensure that it will properly coerce to its expectation
///
/// this will either:
/// a) do nothing (return the same expr)
/// b) make an implicit cast explicit
/// c) find that an expectation was violated and produce a nice error message
fn coerce(env: *Env, id: Id, outward: TypeId) SemaError!Result {
    const texpr = env.get(id);

    // TypeId comparison is fastest
    if (texpr.ty.eql(outward)) {
        return Result.ok(id);
    }

    // check for an allowed implicit cast
    const inner = env.tw.get(texpr.ty);
    const outer = env.tw.get(outward);

    const method = (try inner.coercesTo(env.ally, &env.tw, outer.*)) orelse {
        // no coercion :(
        return expectError(env.*, texpr.loc, outward, texpr.ty);
    };

    return switch (method) {
        .inbounds => in: {
            // nothing needs to be done
            break :in Result.ok(id);
        },
        .natural => nat: {
            // cast the expr
            const builtin_ty = try env.identify(.builtin);
            const cast = try env.new(
                texpr.loc,
                builtin_ty,
                .{ .builtin = .cast },
            );
            const final = try env.from(try TExpr.initCall(
                env.*,
                texpr.loc,
                outward,
                &.{ cast, id },
            ));

            break :nat Result.ok(final);
        },
        .array_ptr_to_slice => aptr: {
            // create a slice from the expr
            const builtin_ty = try env.identify(.builtin);
            const convert = try env.new(
                texpr.loc,
                builtin_ty,
                .{ .builtin = .array_ptr_to_slice },
            );
            const final = try env.from(try TExpr.initCall(
                env.*,
                texpr.loc,
                outward,
                &.{ convert, id },
            ));

            break :aptr Result.ok(final);
        },
    };
}

fn analyzeExpr(
    env: *Env,
    scope: Name,
    expr: SExpr,
    outward: TypeId,
) SemaError!Result {
    return switch (expr.data) {
        .call => try analyzeCall(env, scope, expr, outward),
        .symbol => try analyzeSymbol(env, scope, expr, outward),
        inline .number, .string => |data, tag| lit: {
            const ty = switch (comptime tag) {
                .number => try env.identify(Type{
                    .number = .{
                        .bits = data.bits,
                        .layout = data.data,
                    },
                }),
                // string literals are (Array N u8)
                .string => try env.identify(Type{
                    .array = Type.Array{
                        .size = data.str.len,
                        .of = try env.identify(Type{
                            .number = .{ .layout = .uint, .bits = 8 },
                        }),
                    },
                }),
                else => unreachable,
            };

            const typed_data = switch (comptime tag) {
                .number => TExpr.Data{ .number = data },
                .string => TExpr.Data{ .string = try data.clone(env.ally) },
                else => unreachable,
            };

            const id = try env.new(expr.loc, ty, typed_data);
            break :lit coerce(env, id, outward);
        },
    };
}

pub fn analyze(
    env: *Env,
    scope: Name,
    expr: SExpr,
    expects: TypeId,
) SemaError!Result {
    const res = try analyzeExpr(env, scope, expr, expects);
    const id = res.get() orelse return res;

    return switch (try postprocess(env, id)) {
        .ok => Result.ok(id),
        .err => |msg| Result.err(msg),
    };
}
