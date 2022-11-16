const std = @import("std");
const Allocator = std.mem.Allocator;
const context = @import("../context.zig");
const Loc = context.Loc;
const FileHandle = context.FileHandle;
const FluentError = context.FluentError;

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

    fn of(ch: u8, loc: Loc) (context.MessageError || FluentError)!Self {
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

            else => if (std.ascii.isPrint(ch)) Self.lexical else err: {
                _ = try context.post(.err, loc, "invalid character", .{});
                break :err error.FluentError;
            },
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
 || context.MessageError
 || FluentError;

/// returns array of tokens allocated on ally
///
/// may generate a FluentError
pub fn tokenize(ally: Allocator, file: FileHandle) TokenizeError![]Token {
    var tokens = std.ArrayList(Token).init(ally);

    for (file.getLines()) |commented_line, line_idx| {
        var i: usize = 0;

        // remove comments
        const comm_start = std.mem.indexOf(u8, commented_line, "//");
        const line = if (comm_start) |index| commented_line[0..index]
                     else commented_line;

        i = 0;

        // indentation awareness
        var indent_len: usize = 0;
        while (at(line, i) == @as(u8, ' ')) : (i += 1) {
            indent_len += 1;
        }

        if (i < line.len) {
            try tokens.append(Token{
                .loc = Loc.init(file, line_idx, 0, indent_len),
                .tag = .indent,
                .slice = line[0..indent_len]
            });
        }

        // other tokenization
        while (at(line, i)) |ch| {
            const ch_loc = Loc.init(file, line_idx, i, 0);

            switch (try CharClass.of(ch, ch_loc)) {
                .space => i += 1,

                // number or symbol
                .digit, .lexical => |start_class| {
                    // find char extent
                    const start = i;
                    i += 1;

                    while (at(line, i)) |sch| : (i += 1) {
                        const class = try CharClass.of(sch, ch_loc);
                        if (!class.in(&.{ .lexical, .digit })) break;
                    }

                    // detect tag
                    var tag: Token.Tag = .symbol;
                    if (start_class == .digit) {
                        tag = .number;
                    } else if (at(line, start).? == '-') maybe_neg: {
                        const next_ch = at(line, start + 1) orelse {
                            break :maybe_neg;
                        };

                        if (try CharClass.of(next_ch, ch_loc) == .digit) {
                            tag = .number;
                        }
                    }

                    try tokens.append(Token{
                        .loc = Loc.init(file, line_idx, start, i - start),
                        .tag = tag,
                        .slice = line[start..i]
                    });
                },

                // strings
                .dquote => {
                    const start = i;
                    i += 1;

                    var last = ch;
                    while (at(line, i)) |sch| : (i += 1) {
                        if (sch == '"' and last != '\\') {
                            i += 1;
                            break;
                        }
                        last = sch;
                    } else {
                        _ = try context.post(
                            .err,
                            ch_loc,
                            "unterminated string",
                            .{}
                        );
                        return error.FluentError;
                    }

                    try tokens.append(Token{
                        .loc = Loc.init(file, line_idx, start, i - start),
                        .tag = .string,
                        .slice = line[start..i]
                    });
                },

                // single char symbols
                .lparen, .rparen, .lbracket, .rbracket, .comma, .colon,
                    => |class| {
                    try tokens.append(Token{
                        .loc = Loc.init(file, line_idx, i, 1),
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
