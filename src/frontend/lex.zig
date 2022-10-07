const std = @import("std");
const context = @import("../context.zig");

const Allocator = std.mem.Allocator;
const Loc = context.Loc;
const FileHandle = context.FileHandle;

const CharClass = enum {
    const Self = @This();

    space,

    lparen,
    rparen,
    lbracket,
    rbracket,
    comma,
    colon,
    dquote,

    digit,
    lexical,

    const Error = error { BadChar };

    fn of(ch: u8) Error!Self {
        return switch (ch) {
            ' ' => .space,

            '(' => .lparen,
            ')' => .rparen,
            '[' => .lbracket,
            ']' => .rbracket,
            ',' => .comma,
            ':' => .colon,
            '"' => .dquote,

            '0'...'9' => .digit,

            else => if (std.ascii.isPrint(ch)) Self.lexical else error.BadChar
        };
    }

    /// test if this tag is one of a set of tags. though this accepts an array
    /// it is O(1)
    fn in(self: Self, comptime tags: []const Self) bool {
        // comptime generate set of tags
        const tag_set = comptime gen_set: {
            var set = std.EnumSet(Self){};
            for (tags) |tag| set.insert(tag);

            break :gen_set set;
        };

        return tag_set.contains(self);
    }
};

pub const Token = struct {
    const Self = @This();

    const Tag = enum {
        line_start,
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
        comma,
        colon,
    };

    loc: Loc,
    tag: Tag,
    slice: []const u8, // unowned

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        _ = fmt;
        _ = options;

        try writer.print(
            "{}: {s} '{s}'",
            .{self.loc, @tagName(self.tag), self.slice}
        );
    }
};

/// bounds checked char indexing
fn at(text: []const u8, index: usize) ?u8 {
    return if (index < text.len) text[index] else null;
}

pub const TokenizeError =
     Allocator.Error
  || context.FluentError;

/// returns array of tokens allocated on ally
///
/// may generate a FluentError
pub fn tokenize(ally: Allocator, handle: FileHandle) TokenizeError![]Token {
    var tokens = std.ArrayList(Token).init(ally);

    for (context.getLines(handle)) |commented_line, line_idx| {
        var i: usize = 0;

        // remove comments
        const comm_start = std.mem.indexOf(u8, commented_line, "//");
        const line = if (comm_start) |index| commented_line[0..index]
                     else commented_line;

        i = 0;

        // indentation awareness
        try tokens.append(Token{
            .loc = Loc.init(handle, line_idx, i, 0),
            .tag = .line_start,
            .slice = line[i..i]
        });

        while (at(line, i) == @as(u8, ' ')) : (i += 1) {
            try tokens.append(Token{
                .loc = Loc.init(handle, line_idx, i, 1),
                .tag = .indent,
                .slice = line[i..i + 1]
            });
        }

        // other tokenization
        while (at(line, i)) |ch| {
            switch (CharClass.of(ch) catch {
                // TODO syntax error
                return error.FluentError;
            }) {
                .space => i += 1,

                // number
                .digit, .lexical => |start_class| {
                    const start = i;
                    i += 1;

                    while (at(line, i)) |sch| : (i += 1) {
                        const class = CharClass.of(sch) catch {
                            // TODO bad number/symbol
                            return error.FluentError;
                        };
                        if (!class.in(&.{ .lexical, .digit })) break;
                    }

                    try tokens.append(Token{
                        .loc = Loc.init(handle, line_idx, start, i - start),
                        .tag = if (start_class == .digit) .number else .symbol,
                        .slice = line[start..i]
                    });
                },

                // strings
                .dquote => @panic("TODO tokenize strings"),

                // single char symbols
                .lparen, .rparen, .lbracket, .rbracket, .comma, .colon,
                    => |class| {
                    try tokens.append(Token{
                        .loc = Loc.init(handle, line_idx, i, 1),
                        .tag = switch (class) {
                            .lparen => .lparen,
                            .rparen => .rparen,
                            .lbracket => .lbracket,
                            .rbracket => .rbracket,
                            .comma => .comma,
                            .colon => .colon,
                            else => unreachable
                        },
                        .slice = line[i..i + 1]
                    });
                    i += 1;
                },
            }
        }
    }

    return tokens.toOwnedSlice();
}
