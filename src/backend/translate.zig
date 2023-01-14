//! 'translation' is the process of turning RawExprs from the frontend into
//! well-formed SExprs in the backend.

const std = @import("std");
const Allocator = std.mem.Allocator;
const util = @import("util");
const Symbol = util.Symbol;
const Loc = util.Loc;
const Message = util.Message;
const Project = util.Project;
const frontend = @import("../frontend.zig");
const RawExpr = frontend.RawExpr;
const SExpr = @import("sexpr.zig");
const canon = @import("canon.zig");
const Number = canon.Number;

const Error = Allocator.Error;
const Result = Message.Result(SExpr);

fn translateGroup(ally: Allocator, proj: Project, expr: RawExpr) Error!Result {
    const children = expr.children.?;
    const exprs = try ally.alloc(SExpr, children.len);

    for (children) |child, i| {
        const res = try translate(ally, proj, child);
        exprs[i] = res.get() orelse return res;
    }

    return Result.ok(SExpr.initCall(expr.loc, exprs));
}

/// translate a call with some symbols at the front
fn translateCall(
    ally: Allocator,
    proj: Project,
    initial: []const []const u8,
    expr: RawExpr
) Error!Result {
    const children = expr.children.?;
    var exprs = std.ArrayList(SExpr).init(ally);
    errdefer {
        for (exprs.items) |sexpr| sexpr.deinit(ally);
        exprs.deinit();
    }

    for (initial) |str| {
        try exprs.append(try SExpr.initSymbol(ally, expr.loc, str));
    }

    for (children) |child| {
        const trans_res = try translate(ally, proj, child);
        const child_expr = trans_res.get() orelse return trans_res;
        try exprs.append(child_expr);
    }

    return Result.ok(SExpr.initCall(expr.loc, exprs.toOwnedSlice()));
}

fn numberError(ally: Allocator, loc: Loc) Allocator.Error!Result {
    return try Message.err(ally, SExpr, loc, "malformed number literal", .{});
}

fn translateNumber(ally: Allocator, proj: Project, expr: RawExpr) Error!Result {
    const slice = proj.getSlice(expr.loc);
    const num = util.parseNumber(ally, slice) catch |e| switch (e) {
        Error.OutOfMemory => return Error.OutOfMemory,
        else => return try numberError(ally, expr.loc)
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
            const text = "invalid number width";
            return try Message.err(ally, SExpr, expr.loc, text, .{});
        }
    }

    const sexpr_num = Number.from(num) catch {
        return try numberError(ally, expr.loc);
    };

    return Result.ok(SExpr.initNumber(expr.loc, sexpr_num));
}

fn translateString(ally: Allocator, proj: Project, expr: RawExpr) Error!Result {
    const slice = proj.getSlice(expr.loc);
    const trimmed = slice[1..slice.len - 1];
    const str = util.stringUnescape(ally, trimmed) catch |e| switch (e) {
        error.BadEscapeSeq => {
            const text = "string contains bad escape sequence";
            return try Message.err(ally, SExpr, expr.loc, text, .{});
        },
        Error.OutOfMemory => return Error.OutOfMemory,
    };

    return Result.ok(SExpr.initOwnedString(expr.loc, str));
}

fn translateSymbol(ally: Allocator, proj: Project, expr: RawExpr) Error!Result {
    const slice = proj.getSlice(expr.loc);
    const symbol = Symbol.init(try ally.dupe(u8, slice));

    // regular symbol
    return Result.ok(SExpr{
        .data = .{ .symbol = symbol },
        .loc = expr.loc,
    });
}

fn translateArray(ally: Allocator, proj: Project, expr: RawExpr) Error!Result {
    const children = expr.children.?;
    const exprs = try ally.alloc(SExpr, children.len);

    for (children) |child, i| {
        const res = try translate(ally, proj, child);
        exprs[i] = res.get() orelse return res;
    }

    return Result.ok(SExpr{
        .data = .{ .array = exprs },
        .loc = expr.loc,
    });
}

pub fn translate(
    ally: Allocator,
    proj: Project,
    expr: RawExpr
) Error!Result {
    return switch (expr.tag) {
        .file => file: {
            const filename = proj.getName(expr.loc.file);
            const initial = &[_][]const u8{"ns", filename};
            break :file try translateCall(ally, proj, initial, expr);
        },
        .group => try translateGroup(ally, proj, expr),
        .number => try translateNumber(ally, proj, expr),
        .string => try translateString(ally, proj, expr),
        .symbol => try translateSymbol(ally, proj, expr),
        .array => try translateArray(ally, proj, expr),
    };
}