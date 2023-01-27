const std = @import("std");
const Allocator = std.mem.Allocator;
const stdout = std.io.getStdOut().writer();
const kz = @import("kritzler");
const util = @import("util");
const Loc = util.Loc;
const FileRef = util.FileRef;
const Project = util.Project;
const Message = util.Message;
const lex = @import("lex.zig");
const Token = lex.Token;
const Stream = @import("stream.zig").Stream;
const RawExpr = @import("raw_expr.zig");
const auto = @import("auto.zig");
const Form = auto.Form;
const FormExpr = auto.FormExpr;
const Syntax = auto.Syntax;

/// context for parsing
const Parser = struct {
    const Self = @This();

    ally: Allocator,
    strm: Stream(Token),
    proj: Project,
    file: FileRef,

    fn done(self: Self) bool {
        return self.strm.done();
    }

    fn reset(self: *Self, index: usize) void {
        self.strm.index = index;
    }

    fn peek(self: Self) ?Token {
        return self.strm.peek();
    }

    fn eat(self: *Self) void {
        self.strm.eat();
    }

    fn prev(self: Self) Token {
        return self.strm.prev();
    }
};

fn parseError(
    ally: Allocator,
    loc: Loc,
    comptime fmt: []const u8,
    args: anytype
) Allocator.Error!Result {
    return try Message.err(ally, RawExpr, loc, fmt, args);
}

fn streamError(
    p: *const Parser,
    comptime fmt: []const u8,
    args: anytype
) Allocator.Error!Result {
    const loc =
        if (p.peek()) |tok| tok.loc
        else if (p.strm.tokens.len == 0) Loc.of(p.file, 0, 0)
        else eof: {
            const final = p.strm.tokens[p.strm.index - 1].loc;
            break :eof Loc.of(p.file, final.stop, final.stop);
        };

    return parseError(p.ally, loc, fmt, args);
}

/// generic error at current parsing location
fn syntaxError(p: *const Parser) Allocator.Error!Result {
    return streamError(p, "syntax error", .{});
}

fn parseAtom(p: *Parser) Allocator.Error!?RawExpr {
    const tok = p.peek() orelse return null;
    return switch (tok.tag) {
        .word => null,
        inline .ident, .number, .string => |tag| lit: {
            p.eat();
            break :lit RawExpr{
                .form = comptime switch (tag) {
                    .ident => .symbol,
                    .number => .number,
                    .string => .string,
                    else => unreachable
                },
                .loc = tok.loc,
            };
        },
    };
}

/// parses syntax, doesn't reset parser on failure
fn parseNonLRSyntax(
    p: *Parser,
    syntax: Syntax,
    prec: usize,
) Allocator.Error!?RawExpr {
    std.debug.assert(!syntax.isLR());

    var exprs = std.ArrayList(RawExpr).init(p.ally);
    defer exprs.deinit();

    for (syntax.fexprs) |fexpr| {
        switch (fexpr) {
            .word => |word| {
                const tok = p.peek() orelse return null;
                const slice = tok.loc.slice(p.proj);

                if (!std.mem.eql(u8, slice, word)) {
                    return null;
                }

                p.eat();
            },
            .expr => |meta| {
                const expr = (try climb(p, meta.innerPrec(prec))) orelse {
                    return null;
                };
                try exprs.append(expr);
            },
        }
    }

    // success
    const start_loc = exprs.items[0].loc;
    const stop_loc = exprs.items[exprs.items.len - 1].loc;

    return RawExpr{
        .form = syntax.form,
        .loc = start_loc.span(stop_loc),
        .exprs = exprs.toOwnedSlice(),
    };
}

// same as parseSyntax but allows left-recursion
fn parseLRSyntax(
    p: *Parser,
    syntax: Syntax,
    prec: usize,
) Allocator.Error!?RawExpr {
    std.debug.assert(syntax.isLR());

    _ = p;
    _ = prec;

    // TODO lr syntax

    return null;
}

/// attempt to parse an expression at or above a certain precedence
fn climb(p: *Parser, prec: usize) Allocator.Error!?RawExpr {
    // atoms are always the highest precedence
    if (prec >= auto.SYNTAX.len) {
        return parseAtom(p);
    }

    // try to parse all of the syntax
    const start_index = p.strm.index;

    for (auto.SYNTAX[prec]) |rule| {
        const res =
            if (rule.isLR()) try parseLRSyntax(p, rule, prec)
            else try parseNonLRSyntax(p, rule, prec);

        if (res) |expr| return expr;

        p.reset(start_index);
    }

    // parsing at this precedence failed, attempt climbing
    return try climb(p, prec + 1);
}

pub const ParseType = enum { file, expr };
pub const Result = Message.Result(RawExpr);

/// parses a token string, providing errors
pub fn parse(
    ally: Allocator,
    proj: Project,
    file: FileRef,
    what: ParseType
) Allocator.Error!Result {
    for (auto.SYNTAX) |prec, i| {
        std.debug.print("[{} precedence]\n", .{i});
        for (prec) |syntax| {
            std.debug.print("{}\n", .{syntax});
        }
        std.debug.print("\n", .{});
    }

    // lex
    const lex_res = try lex.tokenize(ally, proj, file);
    const tokens = lex_res.get() orelse return lex_res.cast(RawExpr);
    defer ally.free(tokens);

    for (tokens) |token| {
        std.debug.print("[{s}] `{s}`\n", .{@tagName(token.tag), token.loc.slice(proj)});
    }

    // parse, respecting behavior based on file vs. expr parsing
    var parser = Parser{
        .ally = ally,
        .strm = Stream(Token).init(tokens, 0),
        .proj = proj,
        .file = file,
    };

    switch (what) {
        .expr => {
            if (parser.done()) {
                return Result.ok(RawExpr{
                    .loc = Loc.of(file, 0, 0),
                    .form = .unit,
                });
            }

            if (try climb(&parser, 0)) |expr| {
                return Result.ok(expr);
            }

            return syntaxError(&parser);
        },
        .file => {
            var exprs = std.ArrayList(RawExpr).init(ally);
            defer {
                for (exprs.items) |expr| expr.deinit(ally);
                exprs.deinit();
            }

            while (!parser.done()) {
                if (try climb(&parser, 0)) |expr| {
                    try exprs.append(expr);
                } else {
                    return syntaxError(&parser);
                }
            }

            // files need to be wrapped in a file expr
            const loc = switch (exprs.items.len) {
                0 => Loc.of(file, 0, 0),
                1 => exprs.items[0].loc,
                else => many: {
                    const first = exprs.items[0].loc;
                    const last = exprs.items[exprs.items.len - 1].loc;
                    break :many first.span(last);
                }
            };

            return Result.ok(RawExpr{
                .loc = loc,
                .form = .file,
                .exprs = exprs.toOwnedSlice(),
            });
        },
    }
}
