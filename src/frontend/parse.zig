const std = @import("std");
const lex = @import("lex.zig");
const fluent = @import("../fluent.zig");
const util = @import("../util/util.zig");
const Expr = @import("expr.zig");
const FlFile = @import("../util/file.zig");

const Context = FlFile.Context;
const Allocator = std.mem.Allocator;
const TokenBuffer = lex.TokenBuffer;
const stderr = std.io.getStdErr().writer();

const Error = Allocator.Error
           || util.FmtError
           || util.Error;

// generates ast starting at token index `from`
// writes final index to `out_to`
fn generate_ast_r(
    ctx: *FlFile.Context,
    ast: *Ast,
    tbuf: *const TokenBuffer,
    from: usize,
    out_to: *usize
) Error!Expr {
    const ast_ally = ast.allocator();
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
                try children.append(try generate_ast_r(ctx, ast, tbuf, i, &i));
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
                try ast_ally.dupe(Expr, children.items)
            );
        },
        .ident => Expr.init_slice(.ident, token.view),
        .integer => Expr.init_slice(.int, token.view),
        .float => Expr.init_slice(.float, token.view),
        .string => Expr.init_slice(.string, token.view),
        .character => @panic("TODO characters are currently unimplemented"),
        .atom => @panic("TODO atoms are currently unimplemented"),
        .rbracket, .rparen => unreachable,
    };
}

fn err_empty_program(ctx: *FlFile.Context) Allocator.Error!void {
    try ctx.add_message(.err, "empty program?", ctx.lfile.text[0..0]);
}

fn generate_expr_ast(
    ctx: *FlFile.Context,
    ast: *Ast,
    tbuf: *const TokenBuffer
) Error!?Expr {
    if (tbuf.tokens.len == 0) {
        try err_empty_program(ctx);
        return null;
    }

    var index: usize = 0;
    const expr = try generate_ast_r(ctx, ast, tbuf, index, &index);

    if (index != tbuf.tokens.len) {
        const views = tbuf.tokens.items(.view);
        const slice = try util.slice_from_bookends(
            views[index],
            views[tbuf.tokens.len - 1]
        );

        try ctx.add_message(
            .err,
            "too many tokens in expression string.",
            slice
        );

        return null;
    }

    return expr;
}

/// files are really just lists of expressions
fn generate_file_ast(
    ctx: *FlFile.Context,
    ast: *Ast,
    tbuf: *const TokenBuffer
) Error!?Expr {
    const ast_ally = ast.allocator();

    if (tbuf.tokens.len == 0) {
        try err_empty_program(ctx);
        return null;
    }

    var children = std.ArrayList(Expr).init(ctx.ally);
    defer children.deinit();

    var index: usize = 0;
    while (index < tbuf.tokens.len) {
        try children.append(try generate_ast_r(ctx, ast, tbuf, index, &index));
    }

    const token_views = tbuf.tokens.items(.view);
    const slice = try util.slice_from_bookends(
        token_views[0],
        token_views[token_views.len - 1]
    );

    return Expr.init_sequence(
        .file,
        slice,
        try ast_ally.dupe(Expr, children.items)
    );
}

/// the ast and the arena which backs it
pub const Ast = struct {
    const Self = @This();

    arena: std.heap.ArenaAllocator,
    root: Expr,

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
    }

    pub fn allocator(self: *Self) Allocator {
        return self.arena.allocator();
    }
};

/// intakes program as an lfile and outputs an ast
/// TODO in general factor out the pattern of using optionals, instead return
/// an error like 'StageFailed' or something
pub fn parse(
    ctx: *Context,
    to: enum{expr, file}
) !?Ast {
    // lex
    var tbuf = try lex.lex(ctx);
    defer tbuf.deinit(ctx);

    try tbuf.validate(ctx);

    if (ctx.err) {
        try ctx.print_messages();
        return null;
    }

    // generate ast
    var ast = Ast{
        .arena = std.heap.ArenaAllocator.init(ctx.ally),
        .root = undefined,
    };

    ast.root = (try switch (to) {
        // TODO remove ast from these and just hand them an allocator
        .expr => generate_expr_ast(ctx, &ast, &tbuf),
        .file => generate_file_ast(ctx, &ast, &tbuf),
    }) orelse return null;

    return ast;
}
