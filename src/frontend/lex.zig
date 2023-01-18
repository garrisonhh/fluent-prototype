const std = @import("std");
const Allocator = std.mem.Allocator;
const util = @import("util");
const Loc = util.Loc;
const Message = util.Message;
const FileRef = util.FileRef;
const Project = util.Project;
const kz = @import("kritzler");
const Stream = @import("stream.zig").Stream;

pub const Token = struct {
    const Self = @This();

    pub const Tag = enum {
        number,
        string,
        symbol,
        keyword,
    };

    tag: Tag,
    loc: Loc,

    fn of(tag: Tag, loc: Loc) Self {
        return Self{
            .tag = tag,
            .loc = loc,
        };
    }
};

const KEYWORDS = kw: {
    const list = [_][]const u8{
        // parens
        "(", ")",
        // lists
        "[", "]",
        // if
        "if", "then", "else"
    };

    // manipulate so it's consumable by ComptimeStringMap
    const KV = struct {
        @"0": []const u8,
        @"1": void = {},
    };
    var kvs: [list.len]KV = undefined;
    for (list) |keyword, i| {
        kvs[i] = KV{ .@"0" = keyword };
    }

    break :kw std.ComptimeStringMap(void, kvs);
};

const CharClass = enum {
    lexical,
    digit,
    separator,
    whitespace,
};

const Lexer = Stream(u8);

const Error = Allocator.Error || error { InvalidCharacter };

fn classify(ch: u8) Error!CharClass {
    return switch (ch) {
        '0'...'9' => .digit,
        ' ', '\n' => .whitespace,
        '.', ';' => .separator,
        else => if (std.ascii.isPrint(ch)) .lexical else Error.InvalidCharacter
    };
}

fn isLexOrNum(ch: u8) Error!bool {
    return switch (try classify(ch)) {
        .digit, .lexical => true,
        else => false
    };
}

fn here(lexer: Lexer, file: FileRef, len: usize) Loc {
    return Loc.of(file, lexer.index, lexer.index + len);
}

fn lengthen(loc: Loc, len: usize) Loc {
    return Loc.of(loc.file, loc.start, loc.start + len);
}

fn lexNumber(lexer: *Lexer, loc: Loc) Error!Token {
    const start = lexer.index;

    // integral section
    while (lexer.peek()) |ch| : (lexer.eat()) {
        if (!try isLexOrNum(ch)) break;
    }

    // test for int
    if (lexer.peek() != @as(u8, '.')) {
        return Token.of(.number, lengthen(loc, lexer.index - start));
    }

    lexer.eat();

    // float section
    while (lexer.peek()) |ch| : (lexer.eat()) {
        if (!try isLexOrNum(ch)) break;
    }

    return Token.of(.number, lengthen(loc, lexer.index - start));
}

fn tokenOfIdent(loc: Loc, slice: []const u8) Token {
    const tag: Token.Tag =
        if (KEYWORDS.get(slice) != null) .keyword else .symbol;

    return Token.of(tag, loc);
}

fn lexIdent(lexer: *Lexer, loc: Loc) Error!Token {
    const start = lexer.index;

    while (lexer.peek()) |ch| : (lexer.eat()) {
        if (!try isLexOrNum(ch)) break;
    }

    const full_loc = lengthen(loc, lexer.index - start);
    const slice = lexer.tokens[start..lexer.index];
    return tokenOfIdent(full_loc, slice);
}

fn lex(
    lexer: *Lexer,
    tokens: *std.ArrayList(Token),
    file: FileRef,
) Error!void {
    while (lexer.peek()) |ch| {
        const loc = here(lexer.*, file, 1);
        switch (try classify(ch)) {
            .whitespace => lexer.eat(),
            .digit => try tokens.append(try lexNumber(lexer, loc)),
            .lexical => try tokens.append(try lexIdent(lexer, loc)),
            .separator => {
                lexer.eat();
                try tokens.append(tokenOfIdent(loc, &[_]u8{ch}));
            },
        }
    }
}

const Result = Message.Result([]Token);

/// returns array of tokens allocated on ally
pub fn tokenize(
    ally: Allocator,
    proj: Project,
    file: FileRef,
) Allocator.Error!Result {
    var lexer = Lexer.init(proj.getText(file), 0);
    var tokens = std.ArrayList(Token).init(ally);

    lex(&lexer, &tokens, file) catch |e| switch (e) {
        Error.OutOfMemory => return Error.OutOfMemory,
        Error.InvalidCharacter => {
            const loc = here(lexer, file, 0);
            const fmt = "invalid character";
            return try Message.err(ally, []Token, loc, fmt, .{});
        }
    };

    // std.debug.print("[tokens]\n", .{});
    // for (tokens.items) |token| {
        // const slice = token.loc.slice(proj);
        // std.debug.print("{s} `{s}`\n", .{@tagName(token.tag), slice});
    // }

    return Result.ok(tokens.toOwnedSlice());
}
