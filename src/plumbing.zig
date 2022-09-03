//! plumbing is where the pipelines are. lol

const std = @import("std");
const builtin = @import("builtin");
const util = @import("util/util.zig");
const frontend = @import("frontend.zig");
const backend = @import("backend.zig");
const FlFile = @import("file.zig");

const stdout = std.io.getStdOut().writer();
const Allocator = std.mem.Allocator;
const Context = FlFile.Context;
const SExpr = backend.SExpr;

pub const ComprehensionError = frontend.Error || SExpr.TranslationError;

/// given an FlFile on a context, lexes, parses, and translates to a Fluent
/// representation
pub fn comprehend(ctx: *Context) ComprehensionError![]SExpr {
    // lex + parse
    var ast = try ctx.wrap_stage(frontend.parse(ctx));
    defer ast.deinit();

    // translate to fluent
    const exprs = try ctx.ally.alloc(SExpr, ast.exprs.len);
    for (ast.exprs) |expr, i| exprs[i] = try SExpr.from_expr(ctx, expr);

    return exprs;
}

/// comprehends without preexisting file
pub fn comprehend_text(
    ally: Allocator,
    name: []const u8,
    text: []const u8
) ComprehensionError![]SExpr {
    // create context
    var lfile = try FlFile.init(ally, name, text);
    defer lfile.deinit(ally);

    var ctx = lfile.context(ally);
    defer ctx.deinit();

    // compile
    return comprehend(&ctx);
}
