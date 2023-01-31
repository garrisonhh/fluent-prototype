const std = @import("std");
const Allocator = std.mem.Allocator;
const stdout = std.io.getStdOut().writer();
const builtin = @import("builtin");
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
            const word = tok.loc.slice(p.proj);

            break :prefixed if (TABLE.prefixed.get(word)) |term| parse: {
                const start_index = p.strm.index;
                const res = try parsePrefixedSyntax(p, term.rule.*, term.prec);
                if (res == null) {
                    p.reset(start_index);
                }

                break :parse res;
            } else null;
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

                    if (tag == .multi) {
                        while (try climb(p, inner_prec)) |expr| {
                            try exprs.append(expr);
                        }
                    }
                },
                inline .opt, .any => |tag| {
                    const inner_prec = meta.innerPrec(prec);
                    const fst_res = try climb(p, inner_prec);

                    if (fst_res) |fst| {
                        try exprs.append(fst);
                    }

                    if (fst_res != null and tag == .any) {
                        while (try climb(p, inner_prec)) |expr| {
                            try exprs.append(expr);
                        }
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
    // atoms are always the highest precedence
    if (min_prec >= auto.MAX_PRECEDENCE) {
        return parseAtom(p);
    }

    // try to parse all of the syntax
    var expr = (try parseAtom(p)) orelse {
        return null;
    };

    // greedily parse as many rules at or above this precedence as possible
    var max_prec = auto.MAX_PRECEDENCE;
    loop: while (true) {
        const next = p.peek() orelse {
            break :loop;
        };

        if (next.tag == .word) {
            // try infix
            const word = next.loc.slice(p.proj);

            const term = TABLE.infixed.get(word) orelse {
                break :loop;
            };

            // rule must equal or supercede this rule's precedence. if I've
            // parsed a rule already, the next rule precedence must not exceed
            // the previous rules' precedence
            if (term.prec < min_prec or term.prec > max_prec) {
                break :loop;
            }

            const res = try parseUnprefixed(
                p,
                term.rule.*,
                term.prec,
                expr,
            );

            expr = res orelse {
                break :loop;
            };

            if (res) |got| {
                expr = got;
                max_prec = term.prec;
                continue;
            }

            break :loop;
        } else {
            // try unfix
            unfix: for (TABLE.unfixed.items) |term| {
                if (term.prec < min_prec or term.prec > max_prec) {
                    continue;
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
    }

    return expr;
}

/// NOTE remove when I no longer need to time parsing
fn timedClimb(p: *Parser) Allocator.Error!?RawExpr {
    const start = util.now();
    const res = try climb(p, 0);
    const stop = util.now();

    const msg: []const u8 = if (res != null) "succeeded" else "failed";
    std.debug.print("parsing {s} in {d:.6}ms\n", .{msg, stop - start});

    return res;
}

var TABLE = auto.SyntaxTable{};

pub fn init(ally: Allocator) auto.SyntaxTable.PutError!void {
    for (auto.SYNTAX) |ruleset, prec| {
        for (ruleset) |*rule| {
            try TABLE.put(ally, rule, prec);
        }
    }

    TABLE.dump();
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

            if (try timedClimb(&parser)) |expr| {
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
                if (try timedClimb(&parser)) |expr| {
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
