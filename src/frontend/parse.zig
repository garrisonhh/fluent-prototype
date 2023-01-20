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
    maybe_form: ?Form
) Allocator.Error!Result {
    if (maybe_form) |form| {
        const res = try climb(p, power);
        const expr = res.get() orelse return res;

        // if a form is provided, check it
        if (expr.data != .form) {
            const fmt = "expected {s}";
            const name = form.name();
            return try parseError(p, expr.loc, fmt, .{name});
        } else if (expr.data == .form) {
            const fmt = "expected {s}, found {s}";
            const exp = form.name();
            const found = expr.data.form.kind.name();
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
        .expr => |meta| switch (meta.flag) {
            .any, .multi => coll: {
                var list = std.ArrayList(RawExpr).init(p.ally);
                defer list.deinit();

                while (true) {
                    const el_res = try expectSingleExpr(p, power, meta.form);
                    const el = switch (el_res) {
                        .ok => |expr| expr,
                        .err => |msg| {
                            if (meta.flag == .multi and list.items.len == 0) {
                                break :coll ExpResult.err(msg);
                            }

                            msg.deinit(p.ally);
                            break;
                        }
                    };

                    try list.append(el);
                }

                break :coll ExpResult.ok(list.toOwnedSlice());
            },
            .one => one: {
                const sg_res = try expectSingleExpr(p, power, meta.form);
                const sg = sg_res.get() orelse return sg_res.cast([]RawExpr);

                const list = try p.ally.alloc(RawExpr, 1);
                list[0] = sg;

                break :one ExpResult.ok(list);
            },
        },
        .keyword => |kw| switch (try expectKeyword(p, kw)) {
            .ok => ExpResult.ok(&.{}),
            .err => |msg| ExpResult.err(msg),
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

    for (syntax.fexprs) |fexpr, i| {
        const res = try expectFormExpr(p, syntax.prec.power, fexpr);
        switch (res) {
            .err => |msg| {
                // early failure
                for (group.items[0..i]) |parsed| parsed.deinit(p.ally);
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

    return Result.ok(RawExpr.initOwnedForm(loc, syntax.form, exprs));
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
            .data = .unit,
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
    const force_call = child.data == .symbol
                    or child.data == .form and child.data.form.kind == .dot;

    if (force_call) {
        const loc = tok.loc.span(rparen.loc);
        return Result.ok(try RawExpr.initForm(p.ally, loc, .call, &.{child}));
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
        inline .number, .string => |tag| lit: {
            p.eat();
            break :lit Result.ok(RawExpr{
                .loc = tok.loc,
                .data = @unionInit(RawExpr.Data, @tagName(tag), {}),
            });
        },
        .symbol => sym: {
            const start_index = p.strm.index;
            p.eat();

            var expr = RawExpr{
                .loc = tok.loc,
                .data = .symbol
            };

            // parse `dot` (field access)
            while (true) {
                // check dot
                const dot_tok = p.peek() orelse break;
                if (dot_tok.tag != .keyword) break;

                const slice = dot_tok.loc.slice(p.proj);
                if (!std.mem.eql(u8, slice, ".")) break;

                p.eat();

                // check field
                const field_tok = p.peek() orelse {
                    p.strm.index = start_index;
                    break :sym streamError(p, "expected field, found EOF", .{});
                };

                if (field_tok.tag != .symbol) {
                    p.strm.index = start_index;
                    break :sym streamError(p, "expected field", .{});
                }

                p.eat();

                const field = RawExpr{
                    .loc = field_tok.loc,
                    .data = .symbol,
                };

                const loc = expr.loc.span(field.loc);
                expr = try RawExpr.initForm(p.ally, loc, .dot, &.{expr, field});
            }

            break :sym Result.ok(expr);
        },
    };
}

fn climb(p: *Parser, power: usize) Allocator.Error!Result {
    const start_index = p.strm.index;

    // parse head
    const head_res = try parseAtom(p);
    var expr = head_res.get() orelse return head_res;

    // parse any parameters passed to head
    var group = std.ArrayList(RawExpr).init(p.ally);
    defer group.deinit();

    try group.append(expr);

    while (true) {
        const param = switch (try parseAtom(p)) {
            .ok => |got| got,
            .err => |msg| {
                msg.deinit(p.ally);
                break;
            }
        };
        try group.append(param);
    }

    if (group.items.len > 1) {
        const last = group.items[group.items.len - 1];
        const loc = group.items[0].loc.span(last.loc);
        expr = RawExpr.initOwnedForm(loc, .call, group.toOwnedSlice());
    }

    // parse non-prefix ops
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
        expr = try RawExpr.initForm(p.ally, loc, syntax.form, &.{expr, rhs});
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
                .data = .unit,
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
        const expr = res.ok;
        const formed = try RawExpr.initForm(ally, expr.loc, .file, &.{expr});
        return Result.ok(formed);
    }

    return res;
}
