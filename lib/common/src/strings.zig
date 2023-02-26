//! string escaping and unescaping
//!
//! supported escape characters:
//! \e -> escape character
//! \n -> line feed
//! \r -> carriage return
//! \t -> horizontal tab
//! \v -> vertical tab
//! \\ -> backquote
//! \' -> squote
//! \" -> dquote
//! \xhh -> hex byte

const std = @import("std");
const Allocator = std.mem.Allocator;

/// parses escape sequences and returns an owned string
pub fn stringUnescape(
    ally: Allocator,
    raw: []const u8,
) (Allocator.Error || error{BadEscapeSeq})![]u8 {
    var buf = try std.ArrayList(u8).initCapacity(ally, raw.len);

    var i: usize = 0;
    while (i < raw.len) : (i += 1) {
        const ch = raw[i];
        const processed = if (ch != '\\') ch else ch: {
            // get next character
            if (i == raw.len) return error.BadEscapeSeq;

            i += 1;
            const next = raw[i];

            // parse escape
            break :ch switch (next) {
                'e' => 0x1b,
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                'v' => 0x0b,
                'x' => hex: {
                    // parse next 2 characters as a byte
                    var val: u8 = 0;
                    if (i + 3 > raw.len) return error.BadEscapeSeq;

                    for (raw[i + 1 .. i + 3]) |hexch| {
                        val = val * 0x10 + switch (hexch) {
                            'a'...'f' => 0xa + hexch - 'a',
                            'A'...'F' => 0xa + hexch - 'A',
                            '0'...'9' => hexch - '0',
                            else => return error.BadEscapeSeq,
                        };
                    }

                    i += 2;
                    break :hex val;
                },
                else => |escaped| escaped,
            };
        };

        try buf.append(processed);
    }

    return buf.toOwnedSlice();
}

/// takes a string containing control characters and escapes them, returns an
/// owned string
pub fn stringEscape(ally: Allocator, str: []const u8) Allocator.Error![]u8 {
    var buf = try std.ArrayList(u8).initCapacity(ally, str.len);

    for (str) |ch| {
        switch (ch) {
            '\\' => try buf.appendSlice("\\\\"),
            '\'' => try buf.appendSlice("\\\'"),
            '\"' => try buf.appendSlice("\\\""),
            else => {
                if (ch >= 0x20) {
                    try buf.append(ch);
                } else {
                    try buf.append('\\');
                    const next = switch (ch) {
                        0x1b => 'e',
                        '\n' => 'n',
                        '\r' => 'r',
                        '\t' => 't',
                        0x0b => 'v',
                        else => hex: {
                            try buf.append('x');
                            try buf.append(ch / 0x10);
                            break :hex ch % 0x10;
                        },
                    };
                    try buf.append(next);
                }
            },
        }
    }

    return buf.toOwnedSlice();
}

test "unescape string" {
    const ally = std.testing.allocator;

    const esc =
        \\\e\n\r\t\v\\\'\"\x30\x31
    ;
    const unesc = "\x1b\n\r\t\x0b\\\'\"\x30\x31";

    const unescaped = try stringUnescape(ally, esc);
    defer ally.free(unescaped);

    try std.testing.expectEqualStrings(unesc, unescaped);
}

test "escape string" {
    const ally = std.testing.allocator;

    const esc =
        \\\e\n\r\t\v\\\'\"01
    ;
    const unesc = "\x1b\n\r\t\x0b\\\'\"\x30\x31";

    const escaped = try stringEscape(ally, unesc);
    defer ally.free(escaped);

    try std.testing.expectEqualStrings(esc, escaped);
}
