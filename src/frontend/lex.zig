const std = @import("std");
const Allocator = std.mem.Allocator;
const util = @import("util");
const Loc = util.Loc;
const Message = util.Message;
const FileRef = util.FileRef;
const Project = util.Project;
const kz = @import("kritzler");
const Stream = @import("stream.zig").Stream;
const auto = @import("auto.zig");

pub const Token = struct {
    const Self = @This();

    pub const Tag = enum {
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
};

const Lexer = Stream(u8);

const Error = Allocator.Error || error { InvalidCharacter };

fn classify(ch: u8) Error!CharClass {
    return switch (ch) {
        '0'...'9' => .digit,
        ' ', '\n' => .whitespace,
        '#' => .comment,
        else => if (std.ascii.isPrint(ch)) .lexical else Error.InvalidCharacter
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

fn tokenOfLexical(lexer: *const Lexer, loc: Loc) Token {
    const str = lexer.tokens[loc.start..loc.stop];
    const tag: Token.Tag =
        if (auto.SYMBOLS.has(str) or auto.KEYWORDS.has(str)) .word
        else .ident;

    return Token.of(tag, loc);
}

fn lexLexical(
    lexer: *Lexer,
    tokens: *std.ArrayList(Token),
    loc: Loc
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
                if (auto.SYMBOLS.has(slice[i..i + j])) {
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
                try tokens.append(tokenOfLexical(lexer, word_loc));
            }

            const sym_loc = Loc{
                .file = loc.file,
                .start = start + i,
                .stop = start + i + sym_len,
            };
            try tokens.append(tokenOfLexical(lexer, sym_loc));

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
        try tokens.append(tokenOfLexical(lexer, word_loc));
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
            .digit => try tokens.append(lexNumber(lexer, loc)),
            .lexical => try lexLexical(lexer, tokens, loc),
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
        }
    };

    return Result.ok(tokens.toOwnedSlice());
}
