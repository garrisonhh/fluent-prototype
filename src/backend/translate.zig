//! 'translation' is the process of turning RawExprs from the frontend into
//! well-formed SExprs in the backend.

const std = @import("std");
const Allocator = std.mem.Allocator;
const util = @import("util");
const Symbol = util.Symbol;
const frontend = @import("../frontend.zig");
const RawExpr = frontend.RawExpr;
const context = @import("../context.zig");
const Loc = context.Loc;
const SExpr = @import("sexpr.zig");
const canon = @import("canon.zig");
const Number = canon.Number;

pub const TranslateError =
    Allocator.Error
 || context.MessageError
 || context.FluentError;

fn translateGroup(ally: Allocator, expr: RawExpr) TranslateError!SExpr {
    const children = expr.children.?;
    const exprs = try ally.alloc(SExpr, children.len);

    for (children) |child, i| {
        exprs[i] = try translate(ally, child);
    }

    return SExpr.initCall(expr.loc, exprs);
}

/// translate a call with some symbols at the front
fn translateCall(
    ally: Allocator,
    initial: []const []const u8,
    expr: RawExpr
) TranslateError!SExpr {
    const children = expr.children.?;
    const exprs = try ally.alloc(SExpr, initial.len + children.len);
    errdefer ally.free(exprs);

    for (initial) |str, i| {
        errdefer for (exprs[0..i]) |e| e.deinit(ally);
        exprs[i] = try SExpr.initSymbol(ally, expr.loc, str);
    }

    const dst = exprs[initial.len..];
    for (children) |child, i| {
        errdefer for (dst[0..i]) |e| e.deinit(ally);
        dst[i] = try translate(ally, child);
    }

    return SExpr.initCall(expr.loc, exprs);
}

fn numberError(loc: Loc) TranslateError {
    const text = "malformed number literal";
    _ = try context.post(.err, loc, text, .{});
    return error.FluentError;
}

fn translateNumber(ally: Allocator, expr: RawExpr) TranslateError!SExpr {
    const num = util.parseNumber(ally, expr.loc.getSlice()) catch |e| {
        return switch (e) {
            error.BadNumber,
            error.WrongLayout,
            error.TooManyBits,
            error.NegativeToUnsigned => numberError(expr.loc),
            else => @errSetCast(TranslateError, e)
        };
    };
    defer num.deinit(ally);

    // validate bits
    if (num.bits) |bits| {
        const integral = num.layout.? != .float;
        const valid_bits: []const u8 =
            if (integral) &[_]u8{8, 16, 32, 64} else &[_]u8{32, 64};

        for (valid_bits) |valid| {
            if (bits == valid) break;
        } else {
            const msg = try context.post(
                .err,
                expr.loc,
                "invalid number width",
                .{}
            );

            if (integral) {
                _ = try msg.annotate(
                    null,
                    "fluent supports 8, 16, 32, or 64 bit integers",
                    .{}
                );
            } else {
                _ = try msg.annotate(
                    null,
                    "fluent supports 32 or 64 bit floats",
                    .{}
                );
            }

            return error.FluentError;
        }
    }

    const sexpr_num = Number.from(num) catch {
        return numberError(expr.loc);
    };

    return SExpr.initNumber(expr.loc, sexpr_num);
}

fn translateString(ally: Allocator, expr: RawExpr) TranslateError!SExpr {
    const slice = expr.loc.getSlice();
    const str = util.stringUnescape(ally, slice[1..slice.len - 1]) catch |e| {
        if (e == error.BadEscapeSeq) {
            _ = try context.post(
                .err, expr.loc, "string contains bad escape sequence", .{}
            );

            return error.FluentError;
        }
        return @errSetCast(TranslateError, e);
    };

    return SExpr.initOwnedString(expr.loc, str);
}

fn translateSymbol(ally: Allocator, expr: RawExpr) TranslateError!SExpr {
    const symbol = Symbol.init(try ally.dupe(u8, expr.loc.getSlice()));

    // regular symbol
    return SExpr{
        .data = .{ .symbol = symbol },
        .loc = expr.loc,
    };
}

pub fn translate(ally: Allocator, expr: RawExpr) TranslateError!SExpr {
    return switch (expr.tag) {
        .file => file: {
            const filename = expr.loc.file.getName();
            const initial = &[_][]const u8{"ns", filename};
            break :file try translateCall(ally, initial, expr);
        },
        .group => try translateGroup(ally, expr),
        .number => try translateNumber(ally, expr),
        .string => try translateString(ally, expr),
        .symbol => try translateSymbol(ally, expr),
        .list => try translateCall(ally, &[_][]const u8{"list"}, expr),
    };
}