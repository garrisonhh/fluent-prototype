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
    p: *const Parser,
    loc: Loc,
    comptime fmt: []const u8,
    args: anytype
) Allocator.Error!Result {
    return try Message.err(p.ally, RawExpr, loc, fmt, args);
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

    return parseError(p, loc, fmt, args);
}

fn unexpectedEof(p: *const Parser) Allocator.Error!Result {
    return streamError(p, "unexpected EOF", .{});
}

fn expectKeyword(
    p: *Parser,
    kw: []const u8,
) Allocator.Error!Message.Result(Token) {
    const tok = p.peek() orelse {
        const err = try streamError(p, "expected `{s}`, found EOF", .{kw});
        return err.cast(Token);
    };

    if (tok.tag != .keyword or !std.mem.eql(u8, kw, tok.loc.slice(p.proj))) {
        const err = try streamError(p, "expected `{s}`", .{kw});
        return err.cast(Token);
    }

    p.eat();

    return Message.Result(Token).ok(tok);
}

fn expectSingleExpr(
    p: *Parser,
    power: usize,
    allowed: []const Form
) Allocator.Error!Result {
    if (allowed.len > 0) {
        const start_index = p.strm.index;
        const res = try climb(p, power);
        const expr = res.get() orelse return res;

        std.debug.print("checking `{s}` against allowed:", .{expr.form.name()});
        for (allowed) |form| std.debug.print(" `{s}`", .{form.name()});
        std.debug.print("\n", .{});

        // a form is provided, so check it
        const is_allowed = check: for (allowed) |form| {
            if (expr.form == form) {
                break :check true;
            }
        } else false;

        if (!is_allowed) {
            p.strm.index = start_index;
            const fmt = "expected {s}, found {s}";
            const exp = "TODO allowed forms";
            const found = expr.form.name();
            return try parseError(p, expr.loc, fmt, .{exp, found});
        }

        return Result.ok(expr);
    } else {
        return try climb(p, power);
    }
}

const ExpResult = Message.Result([]RawExpr);

/// expect a single formexpr
fn expectFormExpr(
    p: *Parser,
    power: usize,
    fexpr: FormExpr
) Allocator.Error!ExpResult {
    return switch (fexpr) {
        .keyword => |kw| switch (try expectKeyword(p, kw)) {
            .ok => ExpResult.ok(&.{}),
            .err => |msg| ExpResult.err(msg),
        },
        .expr => |meta| expr: {
            const allowed = meta.allowed.slice();

            break :expr switch (meta.flag) {
                .one => one: {
                    const res = try expectSingleExpr(p, power, allowed);
                    const expr = res.get() orelse return res.cast([]RawExpr);

                    const list = try p.ally.alloc(RawExpr, 1);
                    list[0] = expr;

                    break :one ExpResult.ok(list);
                },
                .maybe => switch (try expectSingleExpr(p, power, allowed)) {
                    .ok => |expr| one: {
                        const list = try p.ally.alloc(RawExpr, 1);
                        list[0] = expr;
                        break :one ExpResult.ok(list);
                    },
                    .err => |msg| none: {
                        msg.deinit(p.ally);
                        break :none ExpResult.ok(&.{});
                    }
                },
                .any, .multi => coll: {
                    var list = std.ArrayList(RawExpr).init(p.ally);
                    defer list.deinit();

                    while (true) {
                        const el_index = p.strm.index;
                        const el_res = try expectSingleExpr(p, power, allowed);
                        const el = switch (el_res) {
                            .ok => |expr| expr,
                            .err => |msg| {
                                if (meta.flag == .multi
                                and list.items.len == 0) {
                                    break :coll ExpResult.err(msg);
                                }

                                p.strm.index = el_index;
                                msg.deinit(p.ally);
                                break;
                            }
                        };

                        try list.append(el);
                    }

                    break :coll ExpResult.ok(list.toOwnedSlice());
                },
            };
        },
    };
}

/// parse syntax where power is >= a certain power
fn parseSyntax(p: *Parser, syntax: Syntax) Allocator.Error!Result {
    const start_index = p.strm.index;

    // parse each FormExpr in order
    var group = std.ArrayList(RawExpr).init(p.ally);
    defer group.deinit();

    const fst_loc = p.peek().?.loc;

    for (syntax.fexprs) |fexpr| {
        const res = try expectFormExpr(p, syntax.prec.power, fexpr);
        switch (res) {
            .err => |msg| {
                // early failure
                for (group.items) |parsed| parsed.deinit(p.ally);
                p.strm.index = start_index;
                return Result.err(msg);
            },
            .ok => |got| {
                defer p.ally.free(got);
                try group.appendSlice(got);
            },
        }
    }

    const last_loc = p.prev().loc;

    // make form expr
    const loc = fst_loc.span(last_loc);
    const exprs = group.toOwnedSlice();

    return Result.ok(RawExpr{
        .loc = loc,
        .form = syntax.form,
        .exprs = exprs,
    });
}

/// parens require special logic with unit, forced calls, etc. and I don't want
/// them to have their own ast form
fn parseParens(p: *Parser) Allocator.Error!Result {
    const start_index = p.strm.index;
    const tok = p.peek().?;
    p.eat();

    std.debug.assert(std.mem.eql(u8, tok.loc.slice(p.proj), "("));

    // check for unit
    switch (try expectKeyword(p, ")")) {
        .ok => |rparen| return Result.ok(RawExpr{
            .loc = tok.loc.span(rparen.loc),
            .form = .unit,
        }),
        .err => |msg| msg.deinit(p.ally),
    }

    // parse `( <> )` form
    const child_res = try climb(p, 0);
    const child = child_res.get() orelse {
        p.strm.index = start_index;
        return child_res;
    };

    const rparen_res = try expectKeyword(p, ")");
    const rparen = rparen_res.get() orelse {
        p.strm.index = start_index;
        return rparen_res.cast(RawExpr);
    };

    // construct child, forcing calls if necessary
    if (child.form == .symbol or child.form == .dot) {
        const loc = tok.loc.span(rparen.loc);
        return Result.ok(try RawExpr.init(p.ally, loc, .call, &.{child}));
    }

    return Result.ok(child);
}

fn parseAtom(p: *Parser) Allocator.Error!Result {
    const tok = p.peek() orelse return unexpectedEof(p);
    return switch (tok.tag) {
        .keyword => kw: {
            const slice = tok.loc.slice(p.proj);
            if (std.mem.eql(u8, slice, "(")) {
                break :kw parseParens(p);
            }

            const syntax = auto.PREFIXED_SYNTAX.get(slice) orelse {
                return streamError(p, "misplaced `{s}`", .{slice});
            };

            break :kw parseSyntax(p, syntax);
        },
        inline .number, .string, .symbol => |tag| lit: {
            p.eat();
            break :lit Result.ok(RawExpr{
                .loc = tok.loc,
                .form = @field(Form, @tagName(tag)),
            });
        },
    };
}

fn climb(p: *Parser, power: usize) Allocator.Error!Result {
    const CALL_POWER = auto.PRECEDENCES.CALL.power;
    const start_index = p.strm.index;

    // function calls require special behavior
    var group = std.ArrayList(RawExpr).init(p.ally);
    defer group.deinit();

    if (power >= CALL_POWER) {
        // parse atom
        const head_res = try parseAtom(p);
        const head = head_res.get() orelse return head_res;

        try group.append(head);
    } else {
        // parse call
        const head_res = try climb(p, CALL_POWER);
        const head = head_res.get() orelse return head_res;

        try group.append(head);

        while (true) {
            const param = switch (try climb(p, CALL_POWER)) {
                .ok => |got| got,
                .err => |msg| {
                    msg.deinit(p.ally);
                    break;
                }
            };
            try group.append(param);
        }
    }

    var expr = if (group.items.len == 1) group.items[0] else call: {
        const last = group.items[group.items.len - 1];
        const loc = group.items[0].loc.span(last.loc);
        break :call RawExpr{
            .loc = loc,
            .form = .call,
            .exprs = group.toOwnedSlice(),
        };
    };

    // parse  ops
    while (true) {
        const next = p.peek() orelse break;

        // check if this is a binary op where precedence.power >= power
        if (next.tag != .keyword) break;

        const keyword = next.loc.slice(p.proj);
        const syntax = auto.BINARY_SYNTAX.get(keyword) orelse break;

        if (syntax.prec.power < power) break;
        p.eat();

        // token is a binary operator, dispatch climb
        const climb_power = syntax.prec.power + @boolToInt(!syntax.prec.right);
        const rhs_res = try climb(p, climb_power);
        const rhs = rhs_res.get() orelse {
            p.strm.index = start_index;
            return rhs_res;
        };

        const loc = expr.loc.span(rhs.loc);
        expr = try RawExpr.init(p.ally, loc, syntax.form, &.{expr, rhs});
    }

    return Result.ok(expr);
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
    // lex
    const lex_res = try lex.tokenize(ally, proj, file);
    const tokens = lex_res.get() orelse return lex_res.cast(RawExpr);
    defer ally.free(tokens);

    // parse
    var parser = Parser{
        .ally = ally,
        .strm = Stream(Token).init(tokens, 0),
        .proj = proj,
        .file = file,
    };

    if (parser.done()) {
        // empty exprs are fine, empty files are not
        return switch (what) {
            .file => try streamError(&parser, "empty file", .{}),
            .expr => Result.ok(RawExpr{
                .loc = Loc.of(file, 0, 0),
                .form = .unit,
            }),
        };
    }

    // handle parse result
    const res = try climb(&parser, 0);

    // check for leftover input
    if (res == .ok and !parser.done()) {
        const fmt = "leftover tokens after parsing";
        return try streamError(&parser, fmt, .{});
    }

    if (what == .file and res == .ok) {
        // files need to be wrapped in a file expr
        const wrapped = try RawExpr.init(ally, res.ok.loc, .file, &.{res.ok});
        return Result.ok(wrapped);
    }

    return res;
}
