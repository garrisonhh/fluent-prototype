const std = @import("std");
const Allocator = std.mem.Allocator;
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

pub const Error = Allocator.Error;

const Parser = Stream(Token);
const Result = Message.Result(RawExpr);

/// a structured but still very raw textual representation of the AST
pub const RawExpr = struct {
    const Self = @This();

    pub const Tag = enum {
        file,
        group,

        // literal-ish
        number,
        string,
        symbol,

        // syntax
        array,
    };

    tag: Tag,
    loc: Loc,
    children: ?[]RawExpr,

    fn init(tag: Tag, loc: Loc, children: ?[]RawExpr) Self {
        return Self{
            .tag = tag,
            .loc = loc,
            .children = children,
        };
    }

    pub fn deinit(self: Self, ally: Allocator) void {
        if (self.children) |children| {
            for (children) |child| child.deinit(ally);
            ally.free(children);
        }
    }
};

fn unexpectedEof(proj: Project, parser: Parser) Allocator.Error!Result {
    const loc = parser.prev().?.loc;
    return try Message.err(proj.ally, RawExpr, loc, "unexpected EOF", .{});
}

fn skipWhitespace(parser: *Parser) void {
    while (parser.peek()) |tok| {
        if (tok.tag != .indent) break;
        parser.mustSkip(1);
    }
}

fn expectGroup(
    ally: Allocator,
    proj: Project,
    file: FileRef,
    parser: *Parser
) Error!Result {
    // collect
    var children = std.ArrayList(RawExpr).init(ally);

    const lparen = parser.next().?;
    std.debug.assert(lparen.tag == .lparen);

    skipWhitespace(parser);

    while (true) {
        const next = parser.peek() orelse {
            return try unexpectedEof(proj, parser.*);
        };

        if (next.tag == .rparen) break;

        // expect next element
        const res = try expectExpr(ally, proj, file, parser);
        const child = res.get() orelse return res;
        try children.append(child);

        skipWhitespace(parser);
    }

    const rparen = parser.next().?;
    std.debug.assert(rparen.tag == .rparen);

    // collect into a rawexpr
    const loc = lparen.loc.span(rparen.loc);
    return Result.ok(RawExpr.init(.group, loc, children.toOwnedSlice()));
}

/// helper for expectExpr
fn expectArray(
    ally: Allocator,
    proj: Project,
    file: FileRef,
    parser: *Parser
) Error!Result {
    // collect
    var children = std.ArrayList(RawExpr).init(ally);

    const lbracket = parser.next().?;
    std.debug.assert(lbracket.tag == .lbracket);

    skipWhitespace(parser);

    while (true) {
        const next = parser.peek() orelse {
            return try unexpectedEof(proj, parser.*);
        };

        if (next.tag == .rbracket) break;

        // expect next element
        const res = try expectExpr(ally, proj, file, parser);
        const child = res.get() orelse return res;
        try children.append(child);

        skipWhitespace(parser);
    }

    const rbracket = parser.next().?;
    std.debug.assert(rbracket.tag == .rbracket);

    // collect into a rawexpr
    const loc = lbracket.loc.span(rbracket.loc);
    return Result.ok(RawExpr.init(.array, loc, children.toOwnedSlice()));
}

/// expects a non-whitespace-aware expr
fn expectExpr(
    ally: Allocator,
    proj: Project,
    file: FileRef,
    parser: *Parser
) Error!Result {
    const fst = parser.peek() orelse {
        return unexpectedEof(proj, parser.*);
    };

    return switch (fst.tag) {
        inline .number, .string, .symbol => |tag| lit: {
            parser.mustSkip(1);

            const raw_tag = @field(RawExpr.Tag, @tagName(tag));
            break :lit Result.ok(RawExpr.init(raw_tag, fst.loc, null));
        },
        .lparen => try expectGroup(ally, proj, file, parser),
        .lbracket => try expectArray(ally, proj, file, parser),
        .rparen, .rbracket, =>
            try Message.err(ally, RawExpr, fst.loc, "expected expression", .{}),
        .indent => unreachable,
    };
}

fn unexpectedIndent(
    ally: Allocator,
    loc: Loc,
    expected: usize
) Allocator.Error!Result {
    const fmt = "expected indentation level of {}";
    return try Message.err(ally, RawExpr, loc, fmt, .{expected});
}

fn skipEmptyLines(parser: *Parser) void {
    // skip empty lines
    while (parser.peek()) |tok| {
        const zero_len_line = tok.tag == .indent and tok.loc.length() == 0;
        const empty_line = tok.tag == .indent and next_nl: {
            const next = parser.get(1) orelse break :next_nl true;
            break :next_nl next.tag == .indent;
        };

        if (!(zero_len_line or empty_line)) {
            break;
        }

        parser.mustSkip(1);
    }
}

/// whitespace awareness
fn expectIndented(
    ally: Allocator,
    proj: Project,
    file: FileRef,
    parser: *Parser,
    parent_level: usize
) Error!Result {
    var children = std.ArrayList(RawExpr).init(ally);
    defer children.deinit();

    // parse non-indented children
    while (parser.peek()) |tok| {
        if (tok.tag == .indent) break;
        const res = try expectExpr(ally, proj, file, parser);
        const child = res.get() orelse return res;
        try children.append(child);
    }

    // indented children
    if (parser.peek()) |next_expr| {
        const level = proj.getSlice(next_expr.loc).len;
        if (next_expr.tag == .indent and level > parent_level) {
            parser.mustSkip(1); // now we can skip this indent

            while (true) {
                // parse child
                const res = try expectIndented(ally, proj, file, parser, level);
                const child = res.get() orelse return res;
                try children.append(child);

                // verify next indent
                const next = parser.peek() orelse break;
                const len = next.loc.stop - next.loc.start;
                if (next.tag != .indent or len < level) {
                    break;
                } else if (len > level) {
                    return unexpectedIndent(ally, next.loc, level);
                }

                // found another indent, so continue
                parser.mustSkip(1);
            }
        }
    }

    // extrapolate to RawExpr
    return switch (children.items.len) {
        0 => unreachable,
        1 => Result.ok(children.items[0]),
        else => group: {
            const start = children.items[0].loc;
            const stop = children.items[children.items.len - 1].loc;
            const loc = start.span(stop);
            const expr = RawExpr.init(.group, loc, children.toOwnedSlice());
            break :group Result.ok(expr);
        }
    };
}

fn expectFile(
    ally: Allocator,
    proj: Project,
    file: FileRef,
    parser: *Parser
) Error!Result {
    var exprs = std.ArrayList(RawExpr).init(ally);

    // collect all the exprs in the file
    while (!parser.completed()) {
        const res = try expectIndented(ally, proj, file, parser, 0);
        const expr = res.get() orelse return res;
        try exprs.append(expr);

        skipEmptyLines(parser);
    }

    // return file as a RawExpr
    const loc = Loc.of(file, 0, proj.getText(file).len - 1);
    return Result.ok(RawExpr.init(.file, loc, exprs.toOwnedSlice()));
}

pub const ParseType = enum { file, expr };

/// parses a token string, providing errors
pub fn parse(
    ally: Allocator,
    proj: Project,
    file: FileRef,
    what: ParseType
) Error!Result {
    // lex
    const lex_res = try lex.tokenize(ally, proj, file);
    const tokens = lex_res.get() orelse return lex_res.cast(RawExpr);
    defer ally.free(tokens);

    // parse
    var parser = Parser.init(tokens, 0);
    skipEmptyLines(&parser);

    // check for empty input
    if (parser.completed()) {
        const loc = Loc.of(file, 0, 0);
        return try Message.err(ally, RawExpr, loc, "nothing to parse", .{});
    }

    return switch (what) {
        .file => try expectFile(ally, proj, file, &parser),
        .expr => expr: {
            // TODO check for tokens left over in token stream
            break :expr expectIndented(ally, proj, file, &parser, 0);
        }
    };
}
