const std = @import("std");

pub fn Stream(comptime T: type) type {
    return struct {
        const Self = @This();

        tokens: []const T,
        index: usize,

        pub fn init(tokens: []const T, index: usize) Self {
            return Self{
                .tokens = tokens,
                .index = index,
            };
        }

        pub fn done(self: Self) bool {
            return self.index == self.tokens.len;
        }

        pub fn get(self: Self, at: usize) ?T {
            const index = self.index + at;
            return if (index < self.tokens.len) self.tokens[index] else null;
        }

        pub fn peek(self: Self) ?T {
            return self.get(0);
        }

        pub fn eat(self: *Self) void {
            std.debug.assert(self.index < self.tokens.len);
            self.index += 1;
        }

        pub fn prev(self: Self) T {
            std.debug.assert(self.index > 0);
            return self.tokens[self.index - 1];
        }
    };
}
