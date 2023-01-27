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
        .word => prefixed: {
            // try to parse any prefixed ops
            // TODO this is absurdly trivially optimizable with a hashmap
            const word = tok.loc.slice(p.proj);

            var i: usize = auto.SYNTAX.len;
            while (i > 0) : (i -= 1) {
                const prec = i - 1;
                const ruleset = auto.SYNTAX[prec];

                for (ruleset) |rule| {
                    if (rule.hasPrefix(word)) {
                        const res = try parsePrefixedSyntax(p, rule, prec);
                        if (res) |expr| {
                            return expr;
                        }
                    }
                }
            }

            // no rule matched
            break :prefixed null;
        },
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

fn parseFormExprs(
    p: *Parser,
    fexprs: []const FormExpr,
    prec: usize,
) Allocator.Error!?[]RawExpr {
    var exprs = std.ArrayList(RawExpr).init(p.ally);
    defer {
        for (exprs.items) |expr| expr.deinit(p.ally);
        exprs.deinit();
    }

    for (fexprs) |fexpr| {
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

    return exprs.toOwnedSlice();
}

/// parses syntax, doesn't reset parser on failure
fn parsePrefixedSyntax(
    p: *Parser,
    syntax: Syntax,
    prec: usize,
) Allocator.Error!?RawExpr {
    std.debug.assert(syntax.isPrefixed());

    const next = p.peek() orelse {
        return null;
    };
    const start_loc = next.loc;

    const exprs = (try parseFormExprs(p, syntax.fexprs, prec)) orelse {
        return null;
    };

    // success
    const stop_loc = p.prev().loc;

    return RawExpr{
        .form = syntax.form,
        .loc = start_loc.span(stop_loc),
        .exprs = exprs,
    };
}

/// parses syntax, doesn't reset parser on failure
/// allows left recursion
fn parseUnprefixedSyntax(
    p: *Parser,
    syntax: Syntax,
    prec: usize,
    leftmost: RawExpr,
) Allocator.Error!?RawExpr {
    std.debug.assert(!syntax.isPrefixed());

    var exprs = std.ArrayList(RawExpr).init(p.ally);
    defer {
        if (exprs.items.len > 1) {
            // leftmost expr is not managed by this function
            for (exprs.items[1..]) |expr| expr.deinit(p.ally);
        }
        exprs.deinit();
    }

    try exprs.append(leftmost);

    // parse this left expr
    const fexprs = syntax.fexprs[1..];
    const rule_exprs = (try parseFormExprs(p, fexprs, prec)) orelse {
        return null;
    };

    try exprs.appendSlice(rule_exprs);

    // success
    const loc = leftmost.loc.span(p.prev().loc);

    return RawExpr{
        .form = syntax.form,
        .loc = loc,
        .exprs = exprs.toOwnedSlice(),
    };
}

/// attempt to parse an expression at or above a certain precedence
fn climb(p: *Parser, prec: usize) Allocator.Error!?RawExpr {
    // atoms are always the highest precedence
    if (prec >= auto.SYNTAX.len) {
        return parseAtom(p);
    }

    // try to parse all of the syntax
    const start_index = p.strm.index;
    const leftmost = try climb(p, prec + 1);

    var expr = parse: for (auto.SYNTAX[prec]) |rule| {
        var res: ?RawExpr = null;

        if (leftmost) |left_expr| {
            if (!rule.isPrefixed()) {
                res = try parseUnprefixedSyntax(p, rule, prec, left_expr);
            }
        } else if (rule.isPrefixed()) {
            res = try parsePrefixedSyntax(p, rule, prec);
        }

        if (res) |got| {
            break :parse got;
        }

        p.reset(start_index);
    } else {
        // no rules found at this precedence, attempt climbing
        break :parse (try climb(p, prec + 1)) orelse {
            return null;
        };
    };

    // keep parsing LR rules until it's not possible anymore
    parse_lr: while (true) {
        const lr_start_index = p.strm.index;

        next_lr: for (auto.SYNTAX[prec]) |rule| {
            if (!rule.isLR()) continue;

            const res = try parseUnprefixedSyntax(p, rule, prec, expr);
            if (res) |got| {
                expr = got;
                break :next_lr;
            }

            p.reset(lr_start_index);
        } else {
            // no more rules worked, break the outer loop
            break :parse_lr;
        }
    }

    return expr;
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
                if (!parser.done()) {
                    return syntaxError(&parser);
                }

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
