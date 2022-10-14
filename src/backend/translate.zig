//! 'translation' is the process of turning RawExprs from the frontend into
//! well-formed SExprs in the backend.

const std = @import("std");
const util = @import("util");
const frontend = @import("../frontend.zig");
const context = @import("../context.zig");
const SExpr = @import("sexpr.zig");

const Allocator = std.mem.Allocator;
const RawExpr = frontend.RawExpr;

pub const TranslateError =
    Allocator.Error
 || util.ParseNumberError;

fn translateGroup(ally: Allocator, expr: RawExpr) TranslateError!SExpr {
    const children = expr.children.?;
    const exprs = try ally.alloc(SExpr, children.len);

    for (children) |child, i| {
        exprs[i] = try translate(ally, child);
    }

    return SExpr.initCall(expr.loc, exprs);
}

fn translateCallTo(
    ally: Allocator,
    name: []const u8,
    expr: RawExpr
) TranslateError!SExpr {
    const children = expr.children.?;
    const exprs = try ally.alloc(SExpr, 1 + children.len);

    exprs[0] = try SExpr.initSymbol(ally, expr.loc, name);

    for (children) |child, i| {
        exprs[i + 1] = try translate(ally, child);
    }

    return SExpr.initCall(expr.loc, exprs);
}

// TODO produce fluent errors for bad numbers
fn translateNumber(ally: Allocator, expr: RawExpr) TranslateError!SExpr {
    const num = try util.parseNumber(ally, expr.loc.getSlice());
    defer num.deinit(ally);

    return switch (num.layout) {
        .int => SExpr.initInt(expr.loc, try num.to(i64)),
        .uint => SExpr.initUInt(expr.loc, try num.to(u64)),
        .float => SExpr.initFloat(expr.loc, try num.to(f64)),
    };
}

pub fn translate(ally: Allocator, expr: RawExpr) TranslateError!SExpr {
    return switch (expr.tag) {
        .file => try translateCallTo(ally, "do", expr),
        .group => try translateGroup(ally, expr),
        .number => try translateNumber(ally, expr),
        .string => try SExpr.initString(ally, expr.loc, expr.loc.getSlice()),
        .symbol => try SExpr.initSymbol(ally, expr.loc, expr.loc.getSlice()),
        .list => try translateCallTo(ally, "list", expr),
    };
}