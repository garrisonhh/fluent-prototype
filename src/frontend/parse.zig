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

const ExpResult = Message.Result(?RawExpr);

/// expect a single formexpr
fn expectFormExpr(p: *Parser, fexpr: FormExpr) Allocator.Error!ExpResult {
    return switch (fexpr) {
        .expr => switch (try climb(p, 0)) {
            .ok => |expr| ExpResult.ok(expr),
            .err => |msg| ExpResult.err(msg),
        },
        .keyword => |kw| switch (try expectKeyword(p, kw)) {
            .ok => ExpResult.ok(null),
            .err => |msg| ExpResult.err(msg),
        },
    };
}

fn parseSyntax(p: *Parser, syntax: Syntax) Allocator.Error!Result {
    // parse each FormExpr in order
    var group = std.ArrayList(RawExpr).init(p.ally);
    defer group.deinit();

    for (syntax.fexprs) |fexpr, i| {
        const res = try expectFormExpr(p, fexpr);
        switch (res) {
            .err => |msg| {
                // early failure
                for (group.items[0..i]) |parsed| parsed.deinit(p.ally);
                return Result.err(msg);
            },
            .ok => |maybe| {
                // parsing was successful
                if (maybe) |got| try group.append(got);
            },
        }
    }

    // make form expr
    const first = group.items[0];
    const last = group.items[group.items.len - 1];

    return Result.ok(RawExpr{
        .loc = first.loc.span(last.loc),
        .data = .{
            .form = .{
                .kind = syntax.form,
                .exprs = group.toOwnedSlice(),
            }
        },
    });
}

fn parseAtom(p: *Parser) Allocator.Error!Result {
    const tok = p.peek() orelse return unexpectedEof(p);

    return switch (tok.tag) {
        .keyword => kw: {
            const slice = tok.loc.slice(p.proj);
            const syntax = auto.PREFIXED_SYNTAX.get(slice) orelse {
                break :kw streamError(p, "misplaced `{s}`", .{slice});
            };

            break :kw try parseSyntax(p, syntax);
        },
        inline .number, .string => |tag| lit: {
            p.eat();
            break :lit Result.ok(RawExpr{
                .loc = tok.loc,
                .data = @unionInit(RawExpr.Data, @tagName(tag), {}),
            });
        },
        .symbol => sym: {
            p.eat();
            break :sym Result.ok(RawExpr{
                .loc = tok.loc,
                .data = .symbol
            });
        },
    };
}

fn climb(p: *Parser, power: usize) Allocator.Error!Result {
    const head_res = try parseAtom(p);
    var expr = head_res.get() orelse return head_res;

    // parse any parameters passed to head
    var group = std.ArrayList(RawExpr).init(p.ally);
    defer group.deinit();

    try group.append(expr);

    while (true) {
        const param = (try parseAtom(p)).get() orelse break;
        try group.append(param);
    }

    if (group.items.len > 1) {
        const last = group.items[group.items.len - 1];
        const loc = group.items[0].loc.span(last.loc);

        expr = RawExpr{
            .loc = loc,
            .data = .{ .group = group.toOwnedSlice() },
        };
    }

    // parse binary ops
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
        const rhs = rhs_res.get() orelse return rhs_res;

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
        return switch (what) {
            .file => try streamError(&parser, "empty file", .{}),
            .expr => Result.ok(RawExpr{
                .loc = Loc.of(file, 0, 0),
                .data = .{
                    .form = .{
                        .kind = .unit,
                        .exprs = &.{},
                    }
                },
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
