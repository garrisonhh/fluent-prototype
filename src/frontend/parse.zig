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

fn unexpectedEof(p: *const Parser) Allocator.Error!Result {
    return streamError(p, "unexpected EOF", .{});
}

fn parseAtom(p: *Parser) Allocator.Error!Result {
    const tok = p.peek();
    return switch (tok.tag) {
        inline .symbol, .number, .string
            => |tag| RawExpr{
            .tag = @field(Form, @tagName(tag)),
            .loc = tok.loc,
        },
        .word => @panic("TODO error parseAtom of word"),
    };
}

fn parseExpr(p: *Parser) Allocator.Error!Result {
    _ = p;

    // TODO
}

fn parseWord(p: *Parser) Allocator.Error!Message.Result(void) {
    _ = p;

    // TODO
}

/// attempt to parse an expression at or above a certain precedence
fn climb(p: *Parser, prec: usize) Allocator.Error!Result {
    // atoms are always the highest precedence
    if (prec > auto.SYNTAX.len) {
        return parseAtom(p);
    }

    const start_index = p.strm.index;
    _ = start_index;

    // collect the syntax rules at this precedence
    const Valid = std.ArrayListUnmanaged(*const Syntax);
    const prec_rules = auto.SYNTAX[prec];

    var valid = try Valid.initCapacity(p.ally, prec_rules.len);
    defer valid.deinit(p.ally);

    for (prec_rules) |*rule| {
        valid.append(rule);
    }

    // attempt to parse each rule in parallel
    // TODO
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

            const res = try climb(&parser, 0);

            if (res == .ok and !parser.done()) {
                const fmt = "leftover tokens after parsing";
                return try streamError(&parser, fmt, .{});
            }

            return res;
        },
        .file => {
            var exprs = std.ArrayList(RawExpr).init(ally);
            defer {
                for (exprs.items) |expr| expr.deinit(ally);
                exprs.deinit();
            }

            while (!parser.done()) {
                switch (try climb(&parser, 0)) {
                    .ok => |expr| try exprs.append(expr),
                    .err => |msg| return Result.err(msg),
                }
            }

            const loc = switch (exprs.items.len) {
                0 => Loc.of(file, 0, 0),
                1 => exprs.items[0].loc,
                else => many: {
                    const first = exprs.items[0].loc;
                    const last = exprs.items[exprs.items.len - 1].loc;
                    break :many first.span(last);
                }
            };

            // files need to be wrapped in a file expr
            return Result.ok(RawExpr{
                .loc = loc,
                .form = .file,
                .exprs = exprs.toOwnedSlice(),
            });
        },
    }
}
