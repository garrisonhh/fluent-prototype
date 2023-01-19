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

const Precedence = union(enum) {
    const Self = @This();

    unary: struct {
        power: usize,
    },
    binary: struct {
        power: usize,
        right: bool = false,
    },

    const STATEMENT      = Self{ .binary = .{ .power = 0, .right = true } };
    const ASSIGNMENT     = Self{ .binary = .{ .power = 1 } };
    const COMPARISON     = Self{ .binary = .{ .power = 2 } };
    const ADDITIVE       = Self{ .binary = .{ .power = 3 } };
    const MULTIPLICATIVE = Self{ .binary = .{ .power = 4 } };
    const NEGATION       = Self{ .unary  = .{ .power = 5 } };
    const FIELD_ACCESS   = Self{ .binary = .{ .power = 6, .right = true } };
};

pub const Syntax = enum {
    const Self = @This();

    // special
    file,
    list,
    parens,

    // general ops
    dot,

    // math
    add,
    sub,
    mul,
    div,
    mod,

    // conditions
    eq,
    gt,
    lt,
    ge,
    le,

    // flow
    stmt,
    @"if",

    // if a syntax form has multiple symbols, only the first is included. the
    // rest are checked manually.
    const symbols = std.ComptimeStringMap(Self, .{
        .{".", .dot},
        .{"+", .add},
        .{"-", .sub},
        .{"*", .mul},
        .{"/", .div},
        .{"%", .mod},
        .{"==", .eq},
        .{">", .gt},
        .{"<", .lt},
        .{">=", .ge},
        .{"<=", .le},
        .{"(", .parens},
        .{"[", .list},
        .{";", .stmt},
        .{"if", .@"if"},
    });

    const precs = map: {
        var map = std.EnumArray(Self, Precedence).initUndefined();

        inline for (std.enums.values(Self)) |tag| {
            const prec: ?Precedence = switch (tag) {
                .dot => Precedence.FIELD_ACCESS,
                .add, .sub => Precedence.ADDITIVE,
                .mul, .div, .mod => Precedence.MULTIPLICATIVE,
                .eq, .gt, .lt, .ge, .le => Precedence.COMPARISON,
                .stmt => Precedence.STATEMENT,
                .file, .list, .parens, .@"if" => null,
            };

            if (prec) |got| {
                map.set(tag, got);
            }
        }

        break :map map;
    };

    fn ofStr(sym: []const u8) ?Self {
        return symbols.get(sym);
    }

    fn getPrec(self: Self) Precedence {
        std.debug.assert(self != .file);
        return precs.get(self);
    }
};

pub const RawExpr = struct {
    const Self = @This();

    pub const Form = struct {
        kind: Syntax,
        exprs: []RawExpr,
    };

    pub const Data = union(enum) {
        unit,
        number,
        string,
        symbol,
        group: []RawExpr,
        form: Form,
    };

    loc: Loc,
    data: Data,

    /// create a group expr by shallow cloning slice
    pub fn initGroup(
        ally: Allocator,
        loc: Loc,
        exprs: []const RawExpr
    ) Allocator.Error!Self {
        return Self{
            .loc = loc,
            .data = .{ .group = try ally.dupe(RawExpr, exprs) },
        };
    }

    /// create a form expr by shallow cloning slice
    pub fn initForm(
        ally: Allocator,
        loc: Loc,
        kind: Syntax,
        exprs: []const RawExpr
    ) Allocator.Error!Self {
        return Self{
            .loc = loc,
            .data = .{
                .form = .{
                    .kind = kind,
                    .exprs = try ally.dupe(Self, exprs),
                }
            },
        };
    }

    pub fn deinit(self: Self, ally: Allocator) void {
        switch (self.data) {
            .group => |group| {
                for (group) |child| child.deinit(ally);
                ally.free(group);
            },
            .form => |form| {
                for (form.exprs) |child| child.deinit(ally);
                ally.free(form.exprs);
            },
            else => {}
        }
    }

    pub fn render(
        self: Self,
        ctx: *kz.Context,
        proj: Project
    ) Allocator.Error!kz.Ref {
        const INDENT = 2;
        return switch (self.data) {
            .unit => try ctx.print(.{}, "()", .{}),
            .number, .string, .symbol => lit: {
                const color: kz.Color = switch (self.data) {
                    .number => .magenta,
                    .string => .green,
                    .symbol => .red,
                    else => unreachable
                };

                const slice = self.loc.slice(proj);
                break :lit try ctx.print(.{ .fg = color }, "{s}", .{slice});
            },
            .group => |exprs| group: {
                const faint = kz.Style{ .special = .faint };
                const head = try ctx.print(faint, "group", .{});

                var children = std.ArrayList(kz.Ref).init(ctx.ally);
                defer children.deinit();

                for (exprs) |expr| {
                    try children.append(try expr.render(ctx, proj));
                }

                break :group try ctx.unify(
                    head,
                    try ctx.stack(children.items, .bottom, .{}),
                    .{INDENT, 1},
                );
            },
            .form => |form| form: {
                const yellow = kz.Style{ .fg = .yellow };
                const name = @tagName(form.kind);
                const head = try ctx.print(yellow, "{s}", .{name});

                var children = std.ArrayList(kz.Ref).init(ctx.ally);
                defer children.deinit();

                for (form.exprs) |expr| {
                    try children.append(try expr.render(ctx, proj));
                }

                break :form try ctx.unify(
                    head,
                    try ctx.stack(children.items, .bottom, .{}),
                    .{INDENT, 1},
                );
            },
        };
    }
};

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

/// try to extract an operator from a token
fn parseOp(p: *const Parser, tok: Token) ?Syntax {
    return Syntax.ofStr(tok.loc.slice(p.proj));
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

/// used by parseForm to encode syntax
const FormExpr = union(enum) {
    const Self = @This();

    expr,
    keyword: []const u8,

    const ExpResult = Message.Result(?RawExpr);

    /// expect a single formexpr
    fn expect(self: Self, p: *Parser) Allocator.Error!ExpResult {
        return switch (self) {
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
};

fn countFormExprs(comptime str: []const u8) usize {
    comptime {
        var count: usize = 0;
        var iter = std.mem.tokenize(u8, str, " ");
        while (iter.next() != null) {
            count += 1;
        }

        return count;
    }
}

/// parses a formexpr at comptime.
/// this is a series of keywords and `<exprs>`
fn series(comptime str: []const u8) [countFormExprs(str)]FormExpr {
    comptime {
        var fexprs = std.BoundedArray(FormExpr, countFormExprs(str)){};

        var iter = std.mem.tokenize(u8, str, " ");
        while (iter.next()) |tok| {
            if (tok[0] == '<' and tok[tok.len - 1] == '>') {
                fexprs.appendAssumeCapacity(.expr);
            } else {
                fexprs.appendAssumeCapacity(.{ .keyword = tok });
            }
        }

        return fexprs.buffer;
    }
}

fn parseSeries(
    p: *Parser,
    kind: Syntax,
    fexprs: []const FormExpr
) Allocator.Error!Result {
    // parse each FormExpr in order
    var group = std.ArrayList(RawExpr).init(p.ally);
    defer group.deinit();

    for (fexprs) |fexpr, i| {
        const res = try fexpr.expect(p);
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
                .kind = kind,
                .exprs = group.toOwnedSlice(),
            }
        },
    });
}

fn parseForm(p: *Parser) Allocator.Error!Result {
    const tok = p.peek().?;
    const op = parseOp(p, tok) orelse {
        // keywords that don't have associated ops are either misplaced or a
        // part of another parse sequence
        const text = tok.loc.slice(p.proj);
        return streamError(p, "misplaced `{s}`", .{text});
    };

    // TODO I can definitely metaprogram this boilerplate somehow
    return switch (op) {
        // ( <expr> )
        .parens => parens: {
            // look for unit literals `()`
            switch (try expectKeyword(p, ")")) {
                .err => |msg| msg.deinit(p.ally),
                .ok => |rparen| {
                    break :parens Result.ok(RawExpr{
                        .loc = tok.loc.span(rparen.loc),
                        .data = .unit,
                    });
                },
            }

            // parens enclose sommething
            const res = try climb(p, 0);
            const expr = res.get() orelse return res;

            const rparen_res = try expectKeyword(p, ")");
            const rparen = rparen_res.get() orelse {
                break :parens rparen_res.cast(RawExpr);
            };

            // ensure that the enclosed expr is getting called (if that was the
            // intention of the parens)
            if (expr.data == .symbol or expr.data == .group) {
                const loc = tok.loc.span(rparen.loc);
                return Result.ok(try RawExpr.initGroup(p.ally, loc, &.{expr}));
            }

            break :parens Result.ok(expr);
        },
        .@"if" => try parseSeries(p, .@"if", &series("if <> then <> else <>")),
        else => unreachable
    };
}

fn parseAtom(p: *Parser) Allocator.Error!Result {
    const tok = p.peek() orelse return unexpectedEof(p);

    return switch (tok.tag) {
        .keyword => try parseForm(p),
        inline .number, .string => |tag| lit: {
            p.eat();
            break :lit Result.ok(RawExpr{
                .loc = tok.loc,
                .data = @unionInit(RawExpr.Data, @tagName(tag), {}),
            });
        },
        .symbol => sym: {
            // operators have special logic
            if (parseOp(p, tok)) |op| switch (op.getPrec()) {
                .unary => |un| {
                    p.eat();

                    const sub_res = try climb(p, un.power);
                    const sub = sub_res.get() orelse return sub_res;

                    const loc = tok.loc.span(sub.loc);
                    const expr = try RawExpr.initForm(p.ally, loc, op, &.{sub});
                    break :sym Result.ok(expr);
                },
                .binary => {
                    const fmt = "expected expr, found binary op";
                    break :sym try streamError(p, fmt, .{});
                },
            };

            // regular symbol
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
        const op = parseOp(p, next) orelse break;

        const prec = op.getPrec();
        if (prec != .binary or prec.binary.power < power) break;
        p.eat();

        // token is a binary operator, dispatch climb
        const climb_power = prec.binary.power + @boolToInt(!prec.binary.right);
        const rhs_res = try climb(p, climb_power);
        const rhs = rhs_res.get() orelse return rhs_res;

        const loc = expr.loc.span(rhs.loc);
        expr = try RawExpr.initForm(p.ally, loc, op, &.{expr, rhs});
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
