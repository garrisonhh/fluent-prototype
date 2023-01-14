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

        pub fn completed(self: Self) bool {
            return self.index == self.tokens.len;
        }

        pub fn get(self: Self, at: usize) ?T {
            const index = self.index + at;
            return if (index < self.tokens.len) self.tokens[index] else null;
        }

        pub fn peek(self: Self) ?T {
            return self.get(0);
        }

        pub fn prev(self: Self) ?T {
            return if (self.index > 0) self.tokens[self.index] else null;
        }

        pub fn next(self: *Self) ?T {
            const token = self.peek() orelse return null;
            self.index += 1;
            return token;
        }

        pub fn skip(self: *Self, steps: usize) error { OutOfBounds }!void {
            if (self.index + steps <= self.tokens.len) {
                self.index += steps;
            } else {
                return error.OutOfBounds;
            }
        }

        pub fn mustSkip(self: *Self, steps: usize) void {
            return self.skip(steps) catch unreachable;
        }
    };
}