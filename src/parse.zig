const std = @import("std");
const lex = @import("lex.zig");
const sema = @import("sema.zig");
const util = @import("util/util.zig");
const FlFile = @import("file.zig");
const Expr = @import("fluent/expr.zig");
const FlType = @import("fluent/type.zig").FlType;

const stderr = std.io.getStdErr().writer();
const Allocator = std.mem.Allocator;
const TokenBuffer = lex.TokenBuffer;

const Error = sema.Error
           || Allocator.Error
           || util.FmtError
           || util.Error;

// generates ast starting at token index `from`
// writes final index to `out_to`
fn generate_ast_r(
    ctx: *FlFile.Context,
    tbuf: *const TokenBuffer,
    from: usize,
    out_to: *usize
) Error!Expr {
    const token = tbuf.tokens.get(from);

    out_to.* = from + 1; // for lists, this is modified again

    return switch (token.ttype) {
        .lparen, .lbracket => |ttype| gen_matched: {
            const terminator: lex.Token.Type = switch (ttype) {
                .lparen => .rparen,
                .lbracket => .rbracket,
                else => unreachable
            };
            const seq_type: Expr.Type = switch (ttype) {
                .lparen => .call,
                .lbracket => .list,
                else => unreachable
            };

            // generate asts recursively for children
            var children = std.ArrayList(Expr).init(ctx.ally);
            defer children.deinit();

            const ttypes = tbuf.tokens.items(.ttype);
            var i: usize = from + 1;
            while (ttypes[i] != terminator) {
                try children.append(try generate_ast_r(ctx, tbuf, i, &i));
            }

            out_to.* = i + 1;

            const token_views = tbuf.tokens.items(.view);
            const slice = try util.slice_from_bookends(
                token_views[from],
                token_views[i]
            );

            break :gen_matched Expr.init_sequence(
                seq_type,
                slice,
                children.toOwnedSlice()
            );
        },
        .ident => Expr.init_slice(.ident, token.view),
        .integer => Expr.init_slice(.int, token.view),
        .float => Expr.init_slice(.float, token.view),
        .string => Expr.init_slice(.string, token.view),
        .rbracket, .rparen => unreachable,
        else => @panic("TODO ???")
    };
}

fn err_empty_program(ctx: *FlFile.Context) Allocator.Error!void {
    try ctx.add_message(.err, "empty program?", ctx.lfile.text[0..0]);
}

fn generate_expr_ast(
    ctx: *FlFile.Context,
    tbuf: *const TokenBuffer
) Error!?Expr {
    if (tbuf.tokens.len == 0) {
        try err_empty_program(ctx);
        return null;
    }

    var index: usize = 0;
    const expr = try generate_ast_r(ctx, tbuf, index, &index);

    if (index != tbuf.tokens.len) {
        const views = tbuf.tokens.items(.view);
        const slice = try util.slice_from_bookends(
            views[index],
            views[tbuf.tokens.len - 1]
        );

        try ctx.add_message(
            .err,
            "too many tokenrrontnndexpression string.",
            slice
        );

        return null;
    }

    return expr;
}

/// returns a list of the expressions stored in ctx allocated onto the context
/// allocator
fn generate_file_ast(
    ctx: *FlFile.Context,
    tbuf: *const TokenBuffer
) Error!?Expr {
    if (tbuf.tokens.len == 0) {
        try err_empty_program(ctx);
        return null;
    }

    var children = std.ArrayList(Expr).init(ctx.ally);

    var index: usize = 0;
    while (index < tbuf.tokens.len) {
        try children.append(try generate_ast_r(ctx, tbuf, index, &index));
    }

    const token_views = tbuf.tokens.items(.view);
    const slice = try util.slice_from_bookends(
        token_views[0],
        token_views[token_views.len - 1]
    );

    return Expr.init_sequence(.file, slice, children.toOwnedSlice());
}

/// bottom-up type inference
fn infer_ast_types(ctx: *sema.Context, ast: *Expr) sema.Error!void {
    // infer on subexprs
    if (ast.children) |children| {
        for (children) |*child| try infer_ast_types(ctx, child);
    }

    // infer this expr
    if (try sema.type_check_and_infer(ctx, ast)) |ltype| {
        ast.ltype = ltype;
    }
}

/// intakes program as a string and outputs an ast
/// TODO allocating ast onto an arena allocator may make a lot of sense
pub fn parse(
    ally: Allocator,
    global: *const sema.TypeScope,
    lfile: *const FlFile,
    to: enum{expr, file}
) !?Expr {
    var ctx = FlFile.Context.init(ally, lfile);
    defer ctx.deinit();

    // lex
    var tbuf = try lex.lex(&ctx);
    defer tbuf.deinit(&ctx);

    try tbuf.validate(&ctx);

    if (ctx.err) {
        try ctx.print_messages();
        return null;
    }

    // generate and typing infer asttype_
    var ast = (try switch (to) {
        .expr => generate_expr_ast(&ctx, &tbuf),
        .file => generate_file_ast(&ctx, &tbuf),
    }) orelse return null;

    var sema_ctx = sema.Context.init(&ctx, global);

    try infer_ast_types(&sema_ctx, &ast);

    if (ctx.err) {
        try ctx.print_messages();
        return null;
    }

    return ast;
}