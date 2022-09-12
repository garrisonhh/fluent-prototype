//! parsing fluent literals.

const std = @import("std");

const Allocator = std.mem.Allocator;

pub const ParseNumberError = Allocator.Error
                          || error { BadNumber, BadCastType };

pub const Number = struct {
    const Self = @This();

    pub const Layout = enum {int, uint, float};

    neg: bool,
    radix: u8,
    // digits before and after the dot
    pre: []u8,
    post: []u8,
    layout: Layout,
    bits: u8,

    pub fn deinit(self: Self, ally: Allocator) void {
        ally.free(self.pre);
        ally.free(self.post);
    }

    pub fn to(self: Self, comptime T: type) ParseNumberError!T {
        switch (@typeInfo(T)) {
            .Int => |int| {
                // verify type is equivalent
                if (int.bits != self.bits
                 or self.layout == .int and int.signedness != .signed
                 or self.layout == .uint and int.signedness != .unsigned) {
                    return ParseNumberError.BadCastType;
                }

                // cast
                const t_radix = @intCast(T, self.radix);
                var num: T = 0;
                for (self.pre) |digit| {
                    num = num * t_radix + @intCast(T, digit);
                }

                if (self.neg) {
                    if (int.signedness == .unsigned) {
                        return ParseNumberError.BadCastType;
                    }

                    num = -num;
                }

                return num;
            },
            .Float => |float| {
                _ = float;
                @panic("TODO");
            },
            else => return ParseNumberError.BadCastType
        }
    }

    fn digit_to_char(digit: u8) u8 {
        return if (digit < 10) digit + '0' else digit + 'a' - 10;
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        _ = fmt;
        _ = options;

        if (self.neg) try writer.writeByte('-');

        for (self.pre) |ch| try writer.writeByte(digit_to_char(ch));

        if (self.post.len > 0) {
            try writer.writeByte('.');
            for (self.post) |ch| try writer.writeByte(digit_to_char(ch));
        }

        const suffix: u8 = switch (self.layout) {
            .int => 'i',
            .uint => 'u',
            .float => 'f',
        };

        try writer.print("{c}{}{{{}}}", .{suffix, self.bits, self.radix});
    }
};

const Slicerator = struct {
    const Self = @This();

    slice: []const u8,

    fn get(self: Self, index: usize) ?u8 {
        return if (index < self.slice.len) self.slice[index] else null;
    }

    fn peek(self: Self) ?u8 {
        return if (self.slice.len > 0) self.slice[0] else null;
    }

    /// consumes next char
    fn next(self: *Self) ?u8 {
        if (self.slice.len > 0) {
            const ch = self.slice[0];
            self.slice = self.slice[1..];

            return ch;
        } else {
            return null;
        }
    }

    /// consume some number of chars, assumes bounds are checked
    fn eat(self: *Self, n: usize) void {
        self.slice = self.slice[n..];
    }
};

pub fn eat_digits(
    ally: Allocator,
    sl: *Slicerator,
    radix: u8
) ParseNumberError![]u8 {
    var buf = std.ArrayList(u8).init(ally);
    while (sl.peek()) |ch| {
        const digit = switch (ch) {
            '0'...'9' => ch - '0',
            'a'...'f' => ch - 'a' + 10,
            'A'...'F' => ch - 'A' + 10,
            '_' => {
                // skip underscores
                sl.eat(1);
                continue;
            },
            else => break
        };

        if (digit > radix) return ParseNumberError.BadNumber;

        sl.eat(1);
        try buf.append(digit);
    }

    return buf.toOwnedSlice();
}

/// parses a number output by the lexer
pub fn parse_number(ally: Allocator, str: []const u8) ParseNumberError!Number {
    const BadNumber = ParseNumberError.BadNumber;
    var sl = Slicerator{ .slice = str };

    // negation
    var neg = false;
    if (sl.peek() == @intCast(u8, '-')) {
        sl.eat(1);
        neg = true;
    }

    // radix
    var radix: u8 = 10;
    if (sl.get(0) == @intCast(u8, '0')) {
        if (sl.get(1)) |r| {
            const found: u8 = switch (r) {
                'b' => 2,
                'o' => 8,
                'x' => 16,
                else => 0
            };

            if (found != 0) {
                sl.eat(2);
                radix = found;
            }
        }
    }

    // pre
    const pre = try eat_digits(ally, &sl, radix);
    if (pre.len == 0) return BadNumber;

    // post
    const post = if (sl.peek() == @intCast(u8, '.')) post: {
        sl.eat(1);
        break :post try eat_digits(ally, &sl, radix);
    } else try ally.alloc(u8, 0);

    // layout
    var layout: Number.Layout = if (post.len > 0) .float else .int;
    if (sl.peek()) |ch| layout: {
        layout = switch (ch) {
            'i' => .int,
            'u' => .uint,
            'f' => .float,
            else => break :layout
        };
        sl.eat(1);
    }

    // bits
    var bits: u8 = 64;
    if (sl.peek() != null) {
        bits = std.fmt.parseInt(u8, sl.slice, 10) catch return BadNumber;
    }

    return Number{
        .neg = neg,
        .radix = radix,
        .pre = pre,
        .post = post,
        .layout = layout,
        .bits = bits,
    };
}