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

/// given an FlFile on a context, lexes, parses, and translates to a Fluent
/// representation
pub fn comprehend(ctx: *Context) !SExpr {
    // lex + parse
    var ast = try ctx.wrap_stage(frontend.parse(ctx, .expr));
    defer ast.deinit();

    // translate to fluent
    return try SExpr.from_expr(ctx, ast.root);
}

/// comprehends without preexisting file
pub fn comprehend_text(
    ally: Allocator,
    name: []const u8,
    text: []const u8
) !SExpr {
    // create context
    var lfile = try FlFile.init(ally, name, text);
    defer lfile.deinit(ally);

    var ctx = lfile.context(ally);
    defer ctx.deinit();

    // compile
    return try comprehend(&ctx);
}
