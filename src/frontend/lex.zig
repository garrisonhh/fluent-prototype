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

    // minus sign
    if (lexer.peek() == @as(u8, '-')) {
        lexer.eat();
    }

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

fn tryLexSymbol(lexer: *Lexer, loc: Loc) Error!?Token {
    var len = auto.MAX_SYMBOL_LEN;
    return while (len > 0) : (len -= 1) {
        const slice = lexer.npeek(len) orelse continue;
        if (auto.SYMBOLS.has(slice)) {
            lexer.neat(len);
            break Token.of(.word, lengthen(loc, len));
        }
    } else null;
}

/// makes a token of a word, checking whether this is a keyword
fn lexicalToken(lexer: *const Lexer, loc: Loc) Token {
    const text = lexer.tokens[loc.start..loc.stop];
    const tag: Token.Tag = if (auto.KEYWORDS.has(text)) .word else .ident;
    return Token.of(tag, loc);
}

fn lexWords(
    lexer: *Lexer,
    tokens: *std.ArrayList(Token),
    loc: Loc,
) Error!void {
    // special case for '-'
    if (lexer.peek() == @as(u8, '-')) {
        if (lexer.get(1)) |ch| {
            if (try classify(ch) == .digit) {
                try tokens.append(lexNumber(lexer, loc));
                return;
            }
        }
    }

    // check for symbols
    if (try tryLexSymbol(lexer, loc)) |token| {
        try tokens.append(token);
        return;
    }

    // parse a word
    var length: usize = 0;
    while (lexer.peek()) |ch| {
        switch (try classify(ch)) {
            .digit, .lexical => {},
            else => break,
        }

        lexer.eat();
        length += 1;

        const sym_index = loc.start + length;
        const sym_loc = Loc.of(loc.file, sym_index, sym_index);

        if (try tryLexSymbol(lexer, sym_loc)) |sym| {
            // hit a symbol, stop parsing
            try tokens.append(lexicalToken(lexer, lengthen(loc, length)));
            try tokens.append(sym);
            return;
        }
    }

    try tokens.append(lexicalToken(lexer, lengthen(loc, length)));
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
            .digit => try tokens.append(lexNumber(lexer, loc)),
            .lexical => try lexWords(lexer, tokens, loc),
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
            return Result.err(try Message.print(
                ally,
                .@"error",
                here(lexer, file, 0),
                "invalid character",
                .{},
            ));
        },
    };

    return Result.ok(tokens.toOwnedSlice());
}
