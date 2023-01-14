const std = @import("std");
const Allocator = std.mem.Allocator;
const util = @import("util");
const Loc = util.Loc;
const Message = util.Message;
const FileRef = util.FileRef;
const Project = util.Project;
const Stream = @import("stream.zig").Stream;

pub const Token = struct {
    const Self = @This();

    pub const Tag = enum {
        indent,

        // literal-ish
        number,
        string,
        symbol,

        // reserved symbols
        lparen,
        rparen,
        lbracket,
        rbracket,
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

const CharClass = enum {
    lexical,
    digit,
    lparen,
    rparen,
    lbracket,
    rbracket,
    space,
    newline,
};

const Lexer = Stream(u8);

const Error = Allocator.Error || error { InvalidCharacter };

fn classify(ch: u8) Error!CharClass {
    return switch (ch) {
        '0'...'9' => .digit,
        ' ' => .space,
        '(' => .lparen,
        ')' => .rparen,
        '[' => .lbracket,
        ']' => .rbracket,
        '\n' => .newline,
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

fn lexNumber(lexer: *Lexer, start: Loc) Error!Token {
    var len: usize = 0;

    // integral section
    while (lexer.peek()) |ch| {
        if (!try isLexOrNum(ch)) break;

        lexer.mustSkip(1);
        len += 1;
    }

    // test for int
    if (lexer.peek() != @as(u8, '.')) {
        return Token.of(.number, lengthen(start, len));
    }

    lexer.mustSkip(1);
    len += 1;

    // float section
    while (lexer.peek()) |ch| {
        if (!try isLexOrNum(ch)) break;

        lexer.mustSkip(1);
        len += 1;
    }

    return Token.of(.number, lengthen(start, len));
}

fn lexIdent(lexer: *Lexer, start: Loc) Error!Token {
    var len: usize = 0;
    while (lexer.peek()) |ch| {
        if (!try isLexOrNum(ch)) break;

        lexer.mustSkip(1);
        len += 1;
    }

    return Token.of(.symbol, lengthen(start, len));
}

fn lexLine(
    lexer: *Lexer,
    tokens: *std.ArrayList(Token),
    file: FileRef,
) Error!void {
    // indentation
    const start = here(lexer.*, file, 0);
    var indent: usize = 0;
    while (lexer.peek()) |ch| {
        if (ch != ' ') break;

        lexer.mustSkip(1);
        indent += 1;
    }

    try tokens.append(Token.of(.indent, lengthen(start, indent)));

    // other tokens
    while (lexer.peek()) |ch| {
        const loc = here(lexer.*, file, 1);
        switch (try classify(ch)) {
            .newline => {
                lexer.mustSkip(1);
                break;
            },
            .space => lexer.mustSkip(1),
            .digit => try tokens.append(try lexNumber(lexer, loc)),
            .lexical => try tokens.append(try lexIdent(lexer, loc)),
            inline .lparen, .rparen, .lbracket, .rbracket => |tag| {
                defer lexer.mustSkip(1);
                const tok = Token.of(@field(Token.Tag, @tagName(tag)), loc);
                try tokens.append(tok);
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

    while (!lexer.completed()) {
        lexLine(&lexer, &tokens, file) catch |e| switch (e) {
            Error.OutOfMemory => return Error.OutOfMemory,
            Error.InvalidCharacter => {
                const loc = here(lexer, file, 0);
                const fmt = "invalid character";
                return try Message.err(ally, []Token, loc, fmt, .{});
            }
        };
    }

    // TODO remove
    for (tokens.items) |token| {
        const stdout = std.io.getStdOut().writer();
        stdout.print("[{s}]\n", .{@tagName(token.tag)}) catch unreachable;
        @import("kritzler").display(ally, proj, token.loc, stdout) catch unreachable;
        stdout.writeByte('\n') catch unreachable;
    }

    return Result.ok(tokens.toOwnedSlice());
}
