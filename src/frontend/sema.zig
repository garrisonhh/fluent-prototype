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
               || util.FmtError
               || util.CompileFailure;

// TODO replace this file with a homoiconic version

fn err_expected_type(
    ctx: *Context,
    expr: *const Expr,
    expected: *const FlType
) Error {
    const msg = try std.fmt.allocPrint(
        ctx.temp_allocator(),
        "expected <{}>, found <{}>",
        .{expr.ltype, expected}
    );

    try ctx.add_message(.err, msg, expr.slice);
    return util.CompilationFailed;
}

/// the is a type annotation: (the type expr)
fn analyze_the(
    ctx: *Context,
    scope: *Scope,
    ast_ally: Allocator,
    expr: *Expr,
    expected: ?*const FlType,
) Error!void {
    const children = expr.children.?;

    if (children.len != 3) {
        try ctx.add_message(
            .err,
            "`the` expects 2 arguments: a type and an expression",
            expr.slice
        );
        return util.CompilationFailed;
    }

    // TODO I CANNOT use dynamic like this. this is a hack to experiment with
    // lambdas until I think harder about the relationship between types and
    // functions and dynamic execution, etc. tbh I think I need to implement
    // quote/quasiquote and start compiling FlValues instead of Exprs to make
    // this work properly.
    const type_expr = &children[1];
    const ltype = blk: {
        const backend = @import("../backend.zig");

        try analyze_r(ctx, scope, ast_ally, type_expr, &FlType{ .ltype = {} });

        var block = backend.lower_ast(ctx, scope, type_expr)
                    catch return util.CompilationFailed;
        defer block.deinit(ctx.ally);

        var vm = backend.FlVm.init(ctx.ally);
        defer vm.deinit();

        try vm.execute_block(&block);

        break :blk try vm.stack.items[0].ltype.clone(ast_ally);
    };

    if (expected) |exp_ltype| {
        if (!ltype.eql(exp_ltype)) {
            return err_expected_type(ctx, type_expr, &ltype);
        }
    }

    // analyze child
    try analyze_r(ctx, scope, ast_ally, &children[2], &ltype);

    // forward type up the chain
    expr.ltype = try children[2].ltype.clone(ast_ally);
}

/// lambda looks like (lambda [p1 p2 ... p3] expr) where params are all unknown
/// identifiers. parameters are inferred from
fn analyze_lambda(
    ctx: *Context,
    parent: *Scope,
    ast_ally: Allocator,
    expr: *Expr,
    expected: ?*const FlType,
) Error!void {
    // verify expected type
    const exp_ltype = if (expected) |t| t else {
        try ctx.add_message(
            .err,
            "can't infer the type of this function without a hint",
            expr.slice
        );
        return util.CompilationFailed;
    };

    if (exp_ltype.* != .function) {
        return err_expected_type(ctx, expr, exp_ltype);
    }

    const exp_fun = exp_ltype.function;

    // verify lambda expr
    const children = expr.children.?;

    if (children.len != 3) {
        try ctx.add_message(
            .err,
            "lambda expects 2 arguments: parameters and an expression",
            expr.slice
        );
        return util.CompilationFailed;
    }

    const params_expr = children[1];
    if (params_expr.etype != .list) {
        try ctx.add_message(
            .err,
            "expected function parameters: [p1 p2 ... pN]",
            params_expr.slice
        );
        return util.CompilationFailed;
    }

    // scope lambda parameters
    var scope = Scope.init(ctx.ally, parent);
    defer scope.deinit();
    const scope_ally = scope.allocator();

    const params = params_expr.children.?;
    for (params) |param, i| {
        if (param.etype != .ident) {
            try ctx.add_message(.err, "expected identifier", param.slice);
            return util.CompilationFailed;
        } else if (scope.get(param.slice) != null) {
            try ctx.add_message(
                .err,
                "identifier already in scope",
                param.slice
            );
            return util.CompilationFailed;
        }

        // add to scope
        const ltype = try exp_fun.params[i].clone(scope_ally);
        try scope.bind_param(param.slice, ltype, i);
    }

    // expect and infer return type
    const return_expr = &children[2];
    try analyze_r(ctx, &scope, ast_ally, return_expr, exp_fun.returns);
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

fn expect_or_infer_r(
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
                if (!try expect_or_infer_r(ast_ally, param, exp_param)) {
                    break :blk false;
                }
            }

            break :blk try expect_or_infer_r(
                ast_ally,
                fun.returns,
                exp_fun.returns
            );
        },
        .list => try expect_or_infer_r(
            ast_ally,
            ltype.list.subtype,
            expected.list.subtype
        ),
        else => true
    };
}

/// either verifies that the types match, or if the type contains any unknown
/// values, attempts to fill them in. if invalid, returns an error
fn expect_or_infer(
    ctx: *Context,
    ast_ally: Allocator,
    expr: *Expr,
    expected: *const FlType
) Error!void {
    if (!try expect_or_infer_r(ast_ally, &expr.ltype, expected)) {
        return err_expected_type(ctx, expr, expected);
    }
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
    const RuleFn = @TypeOf(analyze_r);
    const syntax_rules = std.ComptimeStringMap(RuleFn, .{
        .{"the", analyze_the},
        .{"lambda", analyze_lambda},
    });

    if (expr.etype == .call) blk: {
        const children = expr.children.?;
        if (children.len == 0) break :blk;

        const calling = children[0];
        if (calling.etype != .ident) break :blk;

        if (syntax_rules.get(calling.slice)) |rule_fn| {
            return rule_fn(ctx, scope, ast_ally, expr, expected);
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
    if (expected) |exp_ltype| {
        try expect_or_infer(ctx, ast_ally, expr, exp_ltype);
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
