//! takes in parsed fluent code, converts everything into canonical form
//!
//! canonical fluent includes operators and syntax constructs which are destined
//! to be translated into `(<predicate> <parameters...>)` in the backend. it
//! does not include things like commas or fn decls, which must be desugared to
//! collection elements and lambdas with casts respectively.

const std = @import("std");
const stderr = std.io.getStdErr().writer();
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const com = @import("common");
const Message = com.Message;
const Project = com.Project;
const RawExpr = @import("raw_expr.zig");

const Result = Message.Result(RawExpr);
const CheckResult = Message.Result(void);

/// currently I am using this for compiler debugging only
fn verify(ally: Allocator, expr: RawExpr) Allocator.Error!CheckResult {
    switch (expr.form) {
        .parens, .comma, .kv, .stmt, .@"fn" => |tag| {
            return CheckResult.err(try Message.print(
                ally,
                .internal,
                expr.loc,
                "failed to desugar {s}",
                .{tag.name()},
            ));
        },
        else => {},
    }

    for (expr.exprs) |child| {
        const res = try verify(ally, child);
        if (res == .err) return res;
    }

    return CheckResult.ok({});
}

fn desugarChildrenOf(
    ally: Allocator,
    proj: Project,
    expr: RawExpr,
) Allocator.Error!Result {
    for (expr.exprs) |*child| {
        const res = try desugarExpr(ally, proj, child.*);
        child.* = res.get() orelse return res;
    }

    return Result.ok(expr);
}

fn desugarExpr(
    ally: Allocator,
    proj: Project,
    expr: RawExpr,
) Allocator.Error!Result {
    // do special desugaring rules
    switch (expr.form) {
        .kv => return desugarChildrenOf(ally, proj, RawExpr{
            .loc = expr.loc,
            .form = .tuple,
            .exprs = expr.exprs,
        }),
        .arrow => {
            const lhs = expr.exprs[0];

            // replace parameters with the address of the parameters
            const params = try ally.alloc(RawExpr, 1);
            params[0] = lhs;

            expr.exprs[0] = RawExpr{
                .form = .addr,
                .loc = lhs.loc,
                .exprs = params,
            };

            return desugarChildrenOf(ally, proj, expr);
        },
        // sequences get collected so they are more intuitive to work with
        inline .comma, .stmt => |tag| {
            const seq_form = switch (tag) {
                .comma => .comma,
                .stmt => .block,
                else => unreachable,
            };

            var coll = std.ArrayList(RawExpr).init(ally);
            defer coll.deinit();

            try coll.appendSlice(expr.exprs);
            ally.free(expr.exprs);

            while (true) {
                const rhs = coll.pop();

                if (rhs.form != tag) {
                    try coll.append(rhs);
                    break;
                }

                try coll.appendSlice(rhs.exprs);
                ally.free(rhs.exprs);
            }

            return desugarChildrenOf(ally, proj, RawExpr{
                .loc = expr.loc,
                .form = seq_form,
                .exprs = coll.toOwnedSlice(),
            });
        },
        .parens => {
            if (expr.exprs.len == 0) {
                return Result.ok(RawExpr{
                    .loc = expr.loc,
                    .form = .unit,
                });
            }

            std.debug.assert(expr.exprs.len == 1);

            const res = try desugarExpr(ally, proj, expr.exprs[0]);
            const inner = res.get() orelse return res;

            ally.free(expr.exprs);

            // parens enclose inner expr
            if (inner.form != .comma) {
                return Result.ok(inner);
            }

            // parens enclose sequence, construct a tuple
            const array_expr = RawExpr{
                .loc = expr.loc,
                .form = .coll,
                .exprs = inner.exprs,
            };

            const addr_exprs = try ally.alloc(RawExpr, 1);
            addr_exprs[0] = array_expr;

            const addr_expr = RawExpr{
                .loc = expr.loc,
                .form = .addr,
                .exprs = addr_exprs,
            };

            const tuple_exprs = try ally.alloc(RawExpr, 1);
            tuple_exprs[0] = addr_expr;

            return Result.ok(RawExpr{
                .loc = expr.loc,
                .form = .tuple,
                .exprs = tuple_exprs,
            });
        },
        .coll => {
            if (expr.exprs.len == 0) {
                return Result.ok(RawExpr{
                    .loc = expr.loc,
                    .form = .coll,
                });
            }

            const res = try desugarExpr(ally, proj, expr.exprs[0]);
            const inner = res.get() orelse return res;

            if (inner.form != .comma) {
                return Result.ok(expr);
            }

            ally.free(expr.exprs);

            return Result.ok(RawExpr{
                .loc = expr.loc,
                .form = .coll,
                .exprs = inner.exprs,
            });
        },
        else => return desugarChildrenOf(ally, proj, expr),
    }
}

pub fn desugar(
    ally: Allocator,
    proj: Project,
    ast: RawExpr,
) Allocator.Error!Result {
    const res = try desugarExpr(ally, proj, ast);

    if (builtin.mode == .Debug and res == .ok) {
        if ((try verify(ally, res.ok)).getErr()) |msg| {
            return Result.err(msg);
        }
    }

    return res;
}
