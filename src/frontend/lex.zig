const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");
const Loc = com.Loc;
const Message = com.Message;
const FileRef = com.FileRef;
const Project = com.Project;
const kz = @import("kritzler");
const Stream = @import("stream.zig").Stream;
const auto = @import("auto.zig");

pub const Token = struct {
    const Self = @This();

    pub const Tag = enum {
        separator,
        number,
        string,
        ident,
        word,
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
    comment,
    whitespace,
    newline,
};

const Lexer = Stream(u8);

const Error = Allocator.Error || error{InvalidCharacter};

fn classify(ch: u8) error{InvalidCharacter}!CharClass {
    return switch (ch) {
        '0'...'9' => .digit,
        ' ' => .whitespace,
        '\n' => .newline,
        '#' => .comment,
        // essentially alphabetical characters and punctuation
        'a'...'z',
        'A'...'Z',
        '!',
        '"',
        '$'...'/',
        ':'...'@',
        '['...'`',
        '{'...'~',
        => .lexical,
        else => Error.InvalidCharacter,
    };
}

fn here(lexer: Lexer, file: FileRef, len: usize) Loc {
    return Loc.of(file, lexer.index, lexer.index + len);
}

fn lengthen(loc: Loc, len: usize) Loc {
    return Loc.of(loc.file, loc.start, loc.start + len);
}

/// characters allowed as part of a number
fn isNumeric(ch: u8) bool {
    return switch (ch) {
        '0'...'9', 'a'...'z', 'A'...'Z', '_' => true,
        else => false,
    };
}

fn lexNumber(lexer: *Lexer, loc: Loc) Token {
    const start = lexer.index;

    // integral section
    while (lexer.peek()) |ch| : (lexer.eat()) {
        if (!isNumeric(ch)) break;
    }

    // test for int
    if (lexer.peek() != @as(u8, '.')) {
        return Token.of(.number, lengthen(loc, lexer.index - start));
    }

    lexer.eat();

    // float section
    while (lexer.peek()) |ch| : (lexer.eat()) {
        if (!isNumeric(ch)) break;
    }

    return Token.of(.number, lengthen(loc, lexer.index - start));
}

fn tokenOfLexical(lexer: *const Lexer, loc: Loc) Error!Token {
    const str = lexer.tokens[loc.start..loc.stop];
    std.debug.assert(str.len > 0);

    // zig fmt: off
    const tag: Token.Tag =
        if ((try classify(str[0])) == .digit) .number
        else if (auto.SYMBOLS.has(str) or auto.KEYWORDS.has(str)) .word
        else .ident;
    // zig fmt: on

    return Token.of(tag, loc);
}

fn lexWords(
    lexer: *Lexer,
    tokens: *std.ArrayList(Token),
    loc: Loc,
) Error!void {
    const start = lexer.index;

    while (lexer.peek()) |ch| {
        switch (try classify(ch)) {
            .digit, .lexical => lexer.eat(),
            else => break,
        }
    }

    // split up token into symbols and keywords
    const slice = lexer.tokens[start..lexer.index];

    var i: usize = 0;
    var word_start: usize = 0;
    while (i < slice.len) {
        // find the longest symbol possible
        const found: ?usize = sym: {
            var j: usize = @min(auto.MAX_SYMBOL_LEN, slice.len - i);
            while (j > 0) : (j -= 1) {
                if (auto.SYMBOLS.has(slice[i .. i + j])) {
                    break :sym j;
                }
            }

            break :sym null;
        };

        if (found) |sym_len| {
            const word_len = i - word_start;
            if (word_len > 0) {
                const word_loc = Loc{
                    .file = loc.file,
                    .start = start + word_start,
                    .stop = start + i,
                };
                try tokens.append(try tokenOfLexical(lexer, word_loc));
            }

            const sym_loc = Loc{
                .file = loc.file,
                .start = start + i,
                .stop = start + i + sym_len,
            };
            try tokens.append(try tokenOfLexical(lexer, sym_loc));

            i += sym_len;
            word_start = i;
        } else {
            i += 1;
        }
    }

    if (start + word_start < lexer.index) {
        const word_loc = Loc{
            .file = loc.file,
            .start = start + word_start,
            .stop = lexer.index,
        };
        try tokens.append(try tokenOfLexical(lexer, word_loc));
    }
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
            .digit, .lexical => try lexWords(lexer, tokens, loc),
            .newline => {
                // insert a separator on a newline followed by an unindented
                // character
                lexer.eat();
                if (lexer.peek()) |next_ch| {
                    switch (try classify(next_ch)) {
                        .whitespace, .newline => {},
                        else => try tokens.append(Token.of(.separator, loc)),
                    }
                }
            },
            .comment => {
                // ignore comments
                lexer.eat();

                while (lexer.peek()) |next| {
                    lexer.eat();
                    if (next == '\n') break;
                }
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
        },
    };

    return Result.ok(tokens.toOwnedSlice());
}
