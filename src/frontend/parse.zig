const std = @import("std");
const Allocator = std.mem.Allocator;
const stdout = std.io.getStdOut().writer();
const builtin = @import("builtin");
const kz = @import("kritzler");
const com = @import("common");
const Loc = com.Loc;
const FileRef = com.FileRef;
const Project = com.Project;
const Message = com.Message;
const lex = @import("lex.zig");
const Token = lex.Token;
const Stream = @import("stream.zig").Stream;
const RawExpr = @import("raw_expr.zig");
const auto = @import("auto.zig");
const Form = auto.Form;
const FormExpr = auto.FormExpr;
const Syntax = auto.Syntax;
const desugar = @import("desugar.zig").desugar;

const TopLevelIterator = struct {
    const Self = @This();

    tokens: []const Token,

    fn done(self: Self) bool {
        return self.tokens.len == 0;
    }

    fn next(self: *Self) ?[]const Token {
        // skip initial separators
        while (self.tokens.len > 0 and self.tokens[0].tag == .separator) {
            self.tokens = self.tokens[1..];
        }

        if (self.tokens.len == 0) {
            return null;
        }

        // get line
        for (self.tokens) |tok, i| {
            if (tok.tag == .separator) {
                defer self.tokens = self.tokens[i + 1 ..];
                return self.tokens[0..i];
            }
        }

        defer self.tokens = &.{};
        return self.tokens;
    }
};

/// context for parsing
const Parser = struct {
    const Self = @This();

    ally: Allocator,
    strm: Stream(Token),
    proj: Project,
    file: FileRef,
    max_parsed: usize = 0,

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
        self.max_parsed = @max(self.max_parsed, self.strm.index);
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
    args: anytype,
) Allocator.Error!Result {
    return try Message.err(ally, RawExpr, loc, fmt, args);
}

fn streamError(
    p: *const Parser,
    comptime fmt: []const u8,
    args: anytype,
) Allocator.Error!Result {
    // zig fmt: off
    const loc =
        if (p.peek()) |tok| tok.loc
        else if (p.strm.tokens.len == 0) Loc.of(p.file, 0, 0)
        else eof: {
            const final = p.strm.tokens[p.strm.index - 1].loc;
            break :eof Loc.of(p.file, final.stop, final.stop);
        };
    // zig fmt: on

    return parseError(p.ally, loc, fmt, args);
}

/// generic error at a location
fn syntaxErrorAt(ally: Allocator, loc: Loc) Allocator.Error!Result {
    return parseError(ally, loc, "syntax error", .{});
}

/// generic error at current parsing location
fn syntaxError(p: *const Parser) Allocator.Error!Result {
    const max_loc = p.strm.tokens[p.max_parsed].loc;
    return syntaxErrorAt(p.ally, Loc.of(p.file, max_loc.stop, max_loc.stop));
}

fn parseAtom(p: *Parser, out_prec: *usize) Allocator.Error!?RawExpr {
    out_prec.* = auto.MAX_PRECEDENCE;

    const tok = p.peek() orelse return null;
    return switch (tok.tag) {
        .separator => unreachable,
        .word => prefixed: {
            // try to parse any prefixed ops
            const word = tok.loc.slice(p.proj);

            for (TABLE.getPrefix(word)) |term| {
                const start_index = p.strm.index;
                const res = try parsePrefixedSyntax(p, term.rule.*, term.prec);
                if (res == null) {
                    p.reset(start_index);
                    continue;
                }

                // get next prec for this rule
                const fexprs = term.rule.fexprs;
                const final = fexprs[fexprs.len - 1];
                if (final == .expr) {
                    out_prec.* = final.expr.innerPrec(term.prec);
                }

                break :prefixed res;
            }

            break :prefixed null;
        },
        inline .ident, .number, .string => |tag| lit: {
            p.eat();
            break :lit RawExpr{
                .form = comptime switch (tag) {
                    .ident => .symbol,
                    .number => .number,
                    .string => .string,
                    else => unreachable,
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
                // accept word
                const tok = p.peek() orelse return null;
                const slice = tok.loc.slice(p.proj);

                if (!std.mem.eql(u8, slice, word)) {
                    return null;
                }

                p.eat();
            },
            .expr => |meta| switch (meta.flag) {
                inline .one, .multi => |tag| {
                    const inner_prec = meta.innerPrec(prec);
                    const fst = (try climb(p, inner_prec)) orelse {
                        return null;
                    };

                    try exprs.append(fst);

                    if (comptime tag == .multi) {
                        while (try climb(p, inner_prec)) |expr| {
                            try exprs.append(expr);
                        }
                    }
                },
                .opt => {
                    if (try climb(p, meta.innerPrec(prec))) |expr| {
                        try exprs.append(expr);
                    }
                },
                .any => {
                    const inner_prec = meta.innerPrec(prec);
                    while (try climb(p, inner_prec)) |expr| {
                        try exprs.append(expr);
                    }
                },
            },
        }
    }

    return exprs.toOwnedSlice();
}

/// resets on failure
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

/// resets to right after the left expr on failure
fn parseUnprefixed(
    p: *Parser,
    syntax: Syntax,
    prec: usize,
    left: RawExpr,
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

    // parse the leftmost expr
    if (syntax.fexprs[0].expr.flag != .one) {
        @panic("TODO parse unprefixed with flags on the left");
    }

    try exprs.append(left);

    // parse the rest of the exprs
    const fexprs = syntax.fexprs[1..];
    const rule_exprs = (try parseFormExprs(p, fexprs, prec)) orelse {
        return null;
    };
    defer p.ally.free(rule_exprs);

    try exprs.appendSlice(rule_exprs);

    // success
    const loc = left.loc.span(p.prev().loc);

    return RawExpr{
        .form = syntax.form,
        .loc = loc,
        .exprs = exprs.toOwnedSlice(),
    };
}

/// attempt to parse an expression at or above a certain precedence
fn climb(p: *Parser, min_prec: usize) Allocator.Error!?RawExpr {
    var max_prec: usize = 0;
    var expr = (try parseAtom(p, &max_prec)) orelse {
        return null;
    };

    // greedily parse as many rules at or above this precedence as possible
    loop: while (p.peek()) |next| {
        // try to parse infix rules
        if (next.tag == .word) {
            const word = next.loc.slice(p.proj);

            infix: for (TABLE.getInfix(word)) |term| {
                // rule must equal or supercede this rule's precedence. if I've
                // parsed a rule already, the next rule precedence must not
                // exceed the previous rules' precedence
                if (term.prec < min_prec or term.prec > max_prec) {
                    continue :infix;
                }

                const res = try parseUnprefixed(
                    p,
                    term.rule.*,
                    term.prec,
                    expr,
                );

                if (res) |got| {
                    expr = got;
                    max_prec = term.prec;
                    continue :loop;
                }
            }
        }

        // try to parse unfix rules
        unfix: for (TABLE.unfixed.slice()) |term| {
            if (term.prec < min_prec or term.prec > max_prec) {
                continue :unfix;
            }

            const res = try parseUnprefixed(
                p,
                term.rule.*,
                term.prec,
                expr,
            );

            if (res) |got| {
                expr = got;
                max_prec = term.prec;
                break :unfix;
            }
        } else {
            // no rules have been matched!
            break :loop;
        }
    }

    return expr;
}

var TABLE = auto.SyntaxTable{};

pub fn init(ally: Allocator) auto.SyntaxTable.PutError!void {
    var i = auto.SYNTAX.len;
    while (i > 0) : (i -= 1) {
        const prec = i - 1;
        const ruleset = auto.SYNTAX[prec];

        for (ruleset) |*rule| {
            try TABLE.put(ally, rule, prec);
        }
    }
}

pub fn deinit(ally: Allocator) void {
    TABLE.deinit(ally);
}

pub const ParseType = enum { file, expr };
pub const Result = Message.Result(RawExpr);

/// parses a token string, providing errors
pub fn parse(
    ally: Allocator,
    proj: Project,
    file: FileRef,
    what: ParseType,
) Allocator.Error!Result {
    // lex
    const lex_res = try lex.tokenize(ally, proj, file);
    const tokens = lex_res.get() orelse return lex_res.cast(RawExpr);
    defer ally.free(tokens);

    // parse, respecting behavior based on file vs. expr parsing
    var tl_iter = TopLevelIterator{
        .tokens = tokens,
    };

    const first_tokens: []const Token = tl_iter.next() orelse &.{};
    var parser = Parser{
        .ally = ally,
        .proj = proj,
        .file = file,
        .strm = Stream(Token).init(first_tokens, 0),
    };

    const expr = switch (what) {
        .expr => expr: {
            if (parser.done()) {
                return syntaxError(&parser);
            }

            const expr = (try climb(&parser, 0)) orelse {
                return syntaxError(&parser);
            };

            if (!parser.done()) {
                return syntaxError(&parser);
            }

            // exprs are single expressions
            if (tl_iter.next()) |next| {
                return syntaxErrorAt(ally, next[0].loc);
            }

            break :expr expr;
        },
        .file => file: {
            var exprs = std.ArrayList(RawExpr).init(ally);
            defer {
                for (exprs.items) |expr| expr.deinit(ally);
                exprs.deinit();
            }

            while (!parser.done()) {
                const expr = (try climb(&parser, 0)) orelse {
                    return syntaxError(&parser);
                };

                if (!parser.done()) {
                    return syntaxError(&parser);
                }

                try exprs.append(expr);

                // iterate toplevel
                if (tl_iter.next()) |next_tokens| {
                    parser.strm = Stream(Token).init(next_tokens, 0);
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
                },
            };

            break :file RawExpr{
                .loc = loc,
                .form = .file,
                .exprs = exprs.toOwnedSlice(),
            };
        },
    };

    // postprocess and return
    return try desugar(ally, proj, expr);
}
