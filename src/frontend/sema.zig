const std = @import("std");
const builtin = @import("builtin");
const util = @import("../util/util.zig");
const fluent = @import("../fluent.zig");
const FlFile = @import("../file.zig");
const Scope = @import("../scope.zig");
const Expr = @import("expr.zig");

const Allocator = std.mem.Allocator;
const Context = FlFile.Context;
const FlType = fluent.FlType;
const FlValue = fluent.FlValue;

pub const Error = Allocator.Error
               || util.CompileFailure;

fn analyze_lambda(
    ctx: *Context,
    parent: *Scope,
    ast_ally: Allocator,
    expr: *Expr,
    expected: ?*const FlType,
) Error!void {
    const children = expr.children.?;
    _ = children;

    _ = ctx;
    _ = parent;
    _ = ast_ally;
    _ = expr;
    _ = expected;

    @panic("TODO");
}

/// used by analyze_r
fn check_infer_call(
    ctx: *Context,
    scope: *Scope,
    ast_ally: Allocator,
    expr: *Expr
) Error!FlType {
    const children = expr.children.?;
    if (children.len == 0) {
        try ctx.add_message(
            .err,
            "function call without function",
            expr.slice
        );
        return util.CompilationFailed;
    }

    // recur on function
    const fn_expr = &children[0];
    try analyze_r(ctx, scope, ast_ally, fn_expr, null);

    if (fn_expr.ltype != .function) {
        try ctx.add_message(
            .err,
            "attempted to call non-function",
            expr.slice
        );
        return util.CompilationFailed;
    }

    // check number of parameters
    const function = fn_expr.ltype.function;
    const params = children[1..];
    if (params.len != function.params.len) {
        const cmp_text =
            if (params.len > function.params.len) "many"
            else "few";
        const msg = try std.fmt.allocPrint(
            ctx.temp_allocator(),
            "too {s} parameters: expected {d}, found {d}",
            .{cmp_text, function.params.len, params.len}
        );
        try ctx.add_message(.err, msg, expr.slice);

        return util.CompilationFailed;
    }

    // type check parameters
    for (params) |*param, i| {
        try analyze_r(ctx, scope, ast_ally, param, &function.params[i]);
    }

    return try function.returns.clone(ast_ally);
}

/// used by analyze_r
fn check_infer_list(
    ctx: *Context,
    scope: *Scope,
    ast_ally: Allocator,
    expr: *Expr
) Error!FlType {
    const children = expr.children.?;
    if (children.len == 0) {
        return try FlType.init_list(ast_ally, &FlType{ .unknown = {} });
    }

    const fst = &children[0];
    try analyze_r(ctx, scope, ast_ally, fst, null);

    for (children[1..]) |*child| {
        // children should all be the same type
        try analyze_r(ctx, scope, ast_ally, child, &fst.ltype);
    }

    return try FlType.init_list(ast_ally, &fst.ltype);
}

/// either verifies that the types match, or if the type contains any unknown
/// values, attempts to fill them in. returns if match is valid.
fn expect_or_infer(
    ast_ally: Allocator,
    ltype: *FlType,
    expected: *const FlType
) Allocator.Error!bool {
    const tag = std.meta.activeTag(ltype.*);
    const exp_tag = std.meta.activeTag(expected.*);

    if (tag == .unknown) {
        // inferrable type
        ltype.* = try expected.clone(ast_ally);
        return true;
    } else if (tag != exp_tag) {
        // no match
        return false;
    }

    // recur on subtypes (if there are any)
    return switch (tag) {
        .function => blk: {
            const fun = ltype.function;
            const exp_fun = expected.function;

            if (fun.params.len != exp_fun.params.len) return false;

            for (fun.params) |*param, i| {
                const exp_param = &exp_fun.params[i];
                if (!try expect_or_infer(ast_ally, param, exp_param)) {
                    break :blk false;
                }
            }

            break :blk try expect_or_infer(
                ast_ally,
                fun.returns,
                exp_fun.returns
            );
        },
        .list => try expect_or_infer(
            ast_ally,
            ltype.list.subtype,
            expected.list.subtype
        ),
        else => true
    };
}

/// checks and infers types bidirectionally. I completely made up this algorithm
/// so I have no idea how it compares to algorithm W (hindley-milner) or
/// whatever. basically it first infers as much as it can from the bottom up,
/// and if holes are left in (unknown types), then it attempts to fill them in
/// with top down information (the expected type).
fn analyze_r(
    ctx: *Context,
    scope: *Scope,
    ast_ally: Allocator,
    expr: *Expr,
    expected: ?*const FlType,
) Error!void {
    // syntax requires special type inference rules
    // TODO I will eventually want a better organized implementation of syntax
    // rules, but I don't know exactly what I need to do with them yet
    const syntax_rules = std.ComptimeStringMap(@TypeOf(analyze_r), .{
        .{"lambda", analyze_lambda},
    });

    if (expr.etype == .call) blk: {
        const children = expr.children.?;
        if (children.len == 0) break :blk;

        const calling = children[0];
        if (calling.etype != .ident) break :blk;

        if (syntax_rules.get(calling.slice)) |infer_fn| {
            return infer_fn(ctx, scope, ast_ally, expr, expected);
        }
    }

    // infer this expr's type from the bottom up
    expr.ltype = switch (expr.etype) {
        .nil => FlType{ .nil = {} },
        .file => FlType{ .nil = {} },
        .int => FlType{ .int = {} },
        .float => FlType{ .float = {} },
        .string => FlType{ .string = {} },
        .ident => infer_ident: {
            if (scope.get(expr.slice)) |binding| {
                break :infer_ident try binding.ltype.clone(ast_ally);
            } else {
                try ctx.add_message(.err, "unknown identifier", expr.slice);
                return util.CompilationFailed;
            }
        },
        .call => try check_infer_call(ctx, scope, ast_ally, expr),
        .list => try check_infer_list(ctx, scope, ast_ally, expr),
    };

    // infer unknown types and validate known types
    if (expected) |exp| {
        if (!try expect_or_infer(ast_ally, &expr.ltype, exp)) {
            try ctx.add_message(
                .err,
                "expected <{}>, found <{}>",
                expr.slice
            );
            return util.CompilationFailed;
        }
    }
}

/// semantic analysis. performs type inference and type checking.
pub fn analyze(
    ctx: *Context,
    scope: *Scope,
    ast_ally: Allocator,
    ast: *Expr
) Error!void {
    try analyze_r(ctx, scope, ast_ally, ast, null);
}
