const std = @import("std");
const builtin = @import("builtin");
const kz = @import("kritzler");
const context = @import("../context.zig");
const lex = @import("lex.zig");

const Allocator = std.mem.Allocator;
const FileHandle = context.FileHandle;
const Loc = context.Loc;
const Token = lex.Token;
const FluentError = context.FluentError;

const ParseError =
     Allocator.Error
  || context.MessageError
  || FluentError
  || error {
    ParserOutOfBounds
};

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
        list,
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

    pub fn render(self: Self, ally: Allocator) Allocator.Error!kz.Texture {
        const INDENT = 4;

        // render tag
        const tag_tex = try kz.Texture.print(
            ally,
            kz.Format{ .special = .faint },
            "{s} ",
            .{@tagName(self.tag)}
        );
        defer tag_tex.deinit(ally);

        // render data with tag
        const rendered = switch (self.tag) {
            .number, .string, .symbol => lit: {
                // fmt this lit
                const fmt = if (self.tag == .symbol) kz.Format{ .fg = .red }
                            else kz.Format{};

                const tex = try kz.Texture.from(ally, fmt, self.loc.getSlice());
                defer tex.deinit(ally);

                // slap to tag
                break :lit try tex.slap(ally, tag_tex, .left, .close);
            },
            .file, .group, .list => parent: {
                // stack child textures
                var textures = std.ArrayList(kz.Texture).init(ally);
                defer {
                    for (textures.items) |tex| tex.deinit(ally);
                    textures.deinit();
                }

                for (self.children.?) |child| {
                    try textures.append(try child.render(ally));
                }

                const children_tex = try kz.Texture.stack(
                    ally,
                    textures.items,
                    .bottom,
                    .close
                );
                defer children_tex.deinit(ally);

                // unify with tag
                break :parent
                    try tag_tex.unify(ally, children_tex, .{INDENT, 1});
            },
        };

        return rendered;
    }
};

/// state for parsing
const Parser = struct {
    const Self = @This();

    file: FileHandle,
    tokens: []const Token,
    index: usize,

    fn get(self: Self, at: usize) ?Token {
        const index = self.index + at;
        return if (index < self.tokens.len) self.tokens[index] else null;
    }

    fn peek(self: Self) ?Token {
        return self.get(0);
    }

    /// for when a token should exist by the syntax rules
    fn expectPeek(self: Self) ParseError!Token {
        return self.peek() orelse unexpectedEof(self.file);
    }

    /// for optimizing when you know it exists
    fn mustPeek(self: Self) Token {
        return self.peek() orelse switch (builtin.mode) {
            .Debug, .ReleaseSafe => {
                @panic("parser called mustPeek in an invalid state.");
            },
            else => unreachable
        };
    }

    fn next(self: *Self) ?Token {
        const token = self.peek() orelse return null;
        self.index += 1;
        return token;
    }

    /// for when a token should exist by the syntax rules
    fn expectNext(self: *Self) ParseError!Token {
        return self.next() orelse unexpectedEof(self.file);
    }

    fn skip(self: *Self, steps: usize) ParseError!void {
        if (self.index + steps <= self.tokens.len) {
            self.index += steps;
        } else {
            return error.ParserOutOfBounds;
        }
    }

    fn mustSkip(self: *Self, steps: usize) void {
        return self.skip(steps) catch switch (builtin.mode) {
            .Debug, .ReleaseSafe => {
                @panic("parser called mustSkip in an invalid state.");
            },
            else => unreachable
        };
    }

    fn skipWhitespace(self: *Self) void {
        while (self.peek()) |tok|{
            if (tok.tag != .indent) break;
            self.mustSkip(1);
        }
    }
};

fn unexpectedEof(file: FileHandle) ParseError {
    _ = try context.post(.err, Loc.init(file, 0, 0, 0), "unexpected EOF.", .{});
    return error.FluentError;
}

/// helper for expectExpr
fn collectParens(ally: Allocator, parser: *Parser) ParseError![]RawExpr {
    var children = std.ArrayList(RawExpr).init(ally);

    // expect child exprs until rparen is reached
    while (true) {
        const begin_index = parser.index;

        switch ((try parser.expectPeek()).tag) {
            .rparen => {
                parser.mustSkip(1);
                break;
            },
            // expectExpr does not care about whitespace
            .indent => parser.mustSkip(1),
            else => try children.append(try expectExpr(ally, parser))
        }

        if (builtin.mode == .Debug) {
            // parser index must iterate or this loop will be infinite
            if (parser.index == begin_index) unreachable;
        }
    }

    return children.toOwnedSlice();
}

/// helper for expectExpr
fn collectList(ally: Allocator, parser: *Parser) ParseError![]RawExpr {
    // collect
    var children = std.ArrayList(RawExpr).init(ally);

    while (true) {
        parser.skipWhitespace();

        // rbracket or next element
        if ((try parser.expectPeek()).tag == .rbracket) {
            parser.mustSkip(1);
            break;
        } else {
            try children.append(try expectExpr(ally, parser));
        }

        // comma or rbracket
        parser.skipWhitespace();

        const tok = try parser.expectNext();
        switch (tok.tag) {
            .comma => {},
            .rbracket => break,
            else => {
                const msg = "expected comma or right bracket.";
                _ = try context.post(.err, tok.loc, msg, .{});
                return error.FluentError;
            }
        }
    }

    return children.toOwnedSlice();
}

/// expects a non-whitespace-aware expr
fn expectExpr(ally: Allocator, parser: *Parser) ParseError!RawExpr {
    const fst = try parser.expectNext();
    return switch (fst.tag) {
        .number => RawExpr.init(.number, fst.loc, null),
        .string => RawExpr.init(.string, fst.loc, null),
        .symbol => RawExpr.init(.symbol, fst.loc, null),
        .lparen => RawExpr.init(
            .group,
            fst.loc.beginning(),
            try collectParens(ally, parser)
        ),
        .lbracket => RawExpr.init(
            .list,
            fst.loc.beginning(),
            try collectList(ally, parser)
        ),
        .rparen, .rbracket, .comma => {
            _ = try context.post(.err, fst.loc, "expected expression.", .{});
            return error.FluentError;
        },
        .indent => unreachable,
        else => @panic("HELP ME")
    };
}

fn unexpectedIndent(loc: Loc, expected: usize) ParseError {
    const fmt = "expected indentation level of {}.";
    _ = try context.post(.err, loc, fmt, .{expected});
    return error.FluentError;
}

/// whitespace awareness
fn expectIndentedExpr(
    ally: Allocator,
    parser: *Parser,
    parent_level: usize
) ParseError!RawExpr {
    var children = std.ArrayList(RawExpr).init(ally);
    defer children.deinit();

    // parse non-indented children
    while (parser.peek()) |tok| {
        if (tok.tag == .indent) break;
        try children.append(try expectExpr(ally, parser));
    }

    // indented children
    if (parser.peek()) |next_expr| {
        const level = next_expr.slice.len;
        if (next_expr.tag == .indent and level > parent_level) {
            parser.mustSkip(1); // now we can skip this indent

            while (true) {
                // parse child
                const child = try expectIndentedExpr(ally, parser, level);
                try children.append(child);

                // verify next indent
                const next = parser.peek() orelse break;
                if (next.tag != .indent or next.slice.len < level) {
                    break;
                } else if (next.slice.len > level) {
                    return unexpectedIndent(next.loc, level);
                }

                // found another indent, so continue
                parser.mustSkip(1);
            }
        }
    }

    // extrapolate to RawExpr
    return switch (children.items.len) {
        0 => unreachable,
        1 => children.items[0],
        else => group: {
            const loc = children.items[0].loc.beginning();
            break :group RawExpr.init(.group, loc, children.toOwnedSlice());
        }
    };
}

fn expectFile(ally: Allocator, parser: *Parser) ParseError!RawExpr {
    var exprs = std.ArrayList(RawExpr).init(ally);

    // collect all the exprs in the file
    while (parser.peek()) |fst| {
        if (fst.tag == .indent) {
            if (fst.slice.len != 0) {
                return unexpectedIndent(fst.loc, 0);
            }

            parser.mustSkip(1);
        }

        try exprs.append(try expectIndentedExpr(ally, parser, 0));
    }

    // return file as a RawExpr
    return RawExpr.init(
        .file,
        Loc.init(parser.file, 0, 0, 0),
        exprs.toOwnedSlice()
    );
}

pub const ParseType = enum { file, expr };

/// parses a token string, providing errors
pub fn parse(
    ally: Allocator,
    file: FileHandle,
    what: ParseType
) ParseError!RawExpr {
    // lex
    const tokens = try lex.tokenize(ally, file);
    defer ally.free(tokens);

    // parse
    var parser = Parser{
        .file = file,
        .tokens = tokens,
        .index = 0,
    };

    return switch (what) {
        .file => try expectFile(ally, &parser),
        .expr => expr: {
            if (parser.mustPeek().tag == .indent) {
                parser.mustSkip(1);
            }

            // TODO check for tokens left over in token stream
            break :expr try expectIndentedExpr(ally, &parser, 0);
        }
    };
}
