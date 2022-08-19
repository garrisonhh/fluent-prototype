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

pub const Error = Allocator.Error;

/// infers types from the bottom up, using scope as context
fn type_infer(
    ctx: *Context,
    scope: *Scope,
    ast_ally: Allocator,
    expr: *Expr
) Error!void {
    const msg_ally = ctx.temp_allocator();

    if (expr.children) |children| {
        for (children) |*child| try type_infer(ctx, scope, ast_ally, child);
    }

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
                return; // TODO error
            }
        },
        .call => infer_call: {
            const children = expr.children.?;
            if (children.len == 0) {
                try ctx.add_message(
                    .err,
                    "function call without function",
                    expr.slice
                );
                return; // TODO error
            }

            const fn_expr = children[0];
            if (fn_expr.ltype != .function) {
                try ctx.add_message(
                    .err,
                    "attempted to call non-function",
                    expr.slice
                );
                return; // TODO error
            }

            // type check parameters
            var bad_params = false;
            const function = fn_expr.ltype.function;
            const params = children[1..];

            var i: usize = 0;
            while (i < @minimum(function.params.len, params.len)) : (i += 1) {
                const expected = &function.params[i];
                const actual = &params[i].ltype;
                if (!actual.eql(expected)) {
                    bad_params = true;

                    const msg = try std.fmt.allocPrint(
                        msg_ally,
                        "wrong parameter type: expected {}, found {}",
                        .{expected, actual}
                    );
                    try ctx.add_message(.err, msg, params[i].slice);
                }
            }

            if (params.len != function.params.len) {
                bad_params = true;

                const cmp_text =
                    if (params.len > function.params.len) "many"
                    else "few";
                const msg = try std.fmt.allocPrint(
                    msg_ally,
                    "too {s} parameters: expected {d}, found {d}",
                    .{cmp_text, function.params.len, params.len}
                );
                try ctx.add_message(.err, msg, expr.slice);
            }

            if (bad_params) return; // TODO error

            break :infer_call try function.returns.clone(ast_ally);
        },
        .list => infer_list: {
            const children = expr.children.?;
            if (children.len == 0) {
                const unk = FlType{ .unknown = {} };
                break :infer_list try FlType.init_list(ast_ally, &unk);
            }

            const fst = children[0];
            for (children[1..]) |child| {
                if (!child.ltype.eql(&fst.ltype)) {
                    try ctx.add_message(
                        .err,
                        "list contains mismatched types:",
                        expr.slice
                    );

                    const fst_note = try std.fmt.allocPrint(
                        msg_ally,
                        "this is {}",
                        .{fst.ltype}
                    );
                    try ctx.add_message(.note, fst_note, fst.slice);

                    const child_note = try std.fmt.allocPrint(
                        msg_ally,
                        "but this is {}",
                        .{child.ltype}
                    );
                    try ctx.add_message(.note, child_note, child.slice);

                    return; // TODO error
                }
            }

            break :infer_list try FlType.init_list(ast_ally, &fst.ltype);
        }
    };
}

/// semantic analysis. performs type inference.
pub fn analyze(
    ctx: *Context,
    scope: *Scope,
    ast_ally: Allocator,
    ast: *Expr
) Error!void {
    return type_infer(ctx, scope, ast_ally, ast);
}