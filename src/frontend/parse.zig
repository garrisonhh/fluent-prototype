const std = @import("std");
const lex = @import("lex.zig");
const util = @import("../util/util.zig");
const Expr = @import("expr.zig");
const FlFile = @import("../file.zig");

const Context = FlFile.Context;
const Allocator = std.mem.Allocator;
const TokenBuffer = lex.TokenBuffer;
const stderr = std.io.getStdErr().writer();

pub const Error = Allocator.Error
           || util.FmtError
           || util.Error
           || util.CompileFailure
           || lex.Error;

/// generates ast starting at token index `from`
/// writes final index to `out_to`
fn generate_ast(
    ctx: *Context,
    ast_ally: Allocator,
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
                const child = try generate_ast(ctx, ast_ally, tbuf, i, &i);
                try children.append(child);
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
        .ident => Expr.init_slice(.symbol, token.view),
        .integer => Expr.init_slice(.int, token.view),
        .float => Expr.init_slice(.float, token.view),
        .string => Expr.init_slice(.string, token.view),
        .character => @panic("TODO characters are currently unimplemented"),
        .atom => @panic("TODO atoms are currently unimplemented"),
        .rbracket, .rparen => unreachable,
    };
}

/// intakes program as an lfile and outputs an ast allocated on ally
pub fn parse(ctx: *Context, ally: Allocator) Error![]Expr {
    // lex
    var tbuf = try lex.lex(ctx);
    defer tbuf.deinit(ctx);

    if (tbuf.tokens.len == 0) {
        try ctx.add_message(.err, "empty program?", ctx.lfile.text[0..0]);
        return util.CompilationFailed;
    }

    // generate ast
    var children = std.ArrayList(Expr).init(ally);

    var index: usize = 0;
    while (index < tbuf.tokens.len) {
        const child = try generate_ast(ctx, ally, &tbuf, index, &index);
        try children.append(child);
    }

    return children.toOwnedSlice();
}
