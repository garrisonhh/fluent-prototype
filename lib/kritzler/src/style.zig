//! formatting for kritzler

const std = @import("std");
const builtin = @import("builtin");

/// ANSI SGR foreground colors
pub const Color = enum(u8) {
    const Self = @This();

    black = 30,
    red = 31,
    green = 32,
    yellow = 33,
    blue = 34,
    magenta = 35,
    cyan = 36,
    light_gray = 37,

    default = 39,

    dark_gray = 90,
    bright_red = 91,
    bright_green = 92,
    bright_yellow = 93,
    bright_blue = 94,
    bright_magenta = 95,
    bright_cyan = 96,
    white = 97,

    pub fn fgCode(self: Self) u8 {
        return @enumToInt(self);
    }

    pub fn bgCode(self: Self) u8 {
        return @enumToInt(self) + 10;
    }
};

pub const Style = struct {
    const Self = @This();

    const Special = enum(u8) {
        reset = 0,
        bold = 1,
        faint = 2,
        italic = 3,
        underline = 4,
        blink = 5,
        fast_blink = 6,

        crossed_out = 9,
    };

    pub const RESET = Self{ .special = .reset };

    special: Special = .reset,
    fg: Color = .default,
    bg: Color = .default,

    pub fn eql(self: Self, other: Self) bool {
        return self.special == other.special and self.fg == other.fg
           and self.bg == other.bg;
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        _ = fmt;
        _ = options;

        try writer.print("\x1b[{}m\x1b[{};{}m", .{
            @enumToInt(self.special),
            self.fg.fgCode(),
            self.bg.bgCode(),
        });
    }
};
