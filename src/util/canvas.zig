const std = @import("std");
const Allocator = std.mem.Allocator;

/// ansi color codes
pub const ConsoleColor = struct {
    pub const Format = enum(u32) {
        bold = 1,
        underline = 4,
        blink = 5,
        crossed_out = 9,
        normal = 22,
    };

    pub const Color = enum(u32) {
        // fg ansi code is +30 from these numbers, bg is +40
        black = 0,
        red = 1,
        green = 2,
        yellow = 3,
        blue = 4,
        magenta = 5,
        cyan = 6,
        white = 7,
        default = 9,
    };

    fg: Color = .default,
    bg: Color = .default,
    fmt: Format = .normal,

    pub fn eql(self: ConsoleColor, other: ConsoleColor) bool {
        return self.fg == other.fg
           and self.bg == other.bg
           and self.fmt == other.fmt;
    }

    pub fn format(
        self: ConsoleColor,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        _ = fmt;
        _ = options;

        // ansi color code
        const code = "\x1b[{d}m";
        try writer.print(code ** 4, .{
            0, // reset
            @enumToInt(self.fmt),
            30 + @enumToInt(self.fg),
            40 + @enumToInt(self.bg)
        });
    }
};

pub const Vec2 = struct {x: i32, y: i32};
pub const Rect = struct {x: i32, y: i32, w: i32, h: i32};

pub const TextBox = struct {
    rect: Rect,
    text: []const u8,
    color: ConsoleColor,

    pub fn init(pos: Vec2, text: []const u8, color: ConsoleColor) TextBox {
        const dims = TextBox.detect_text_dims(text);
        return TextBox.init_with_dims(pos, dims, text, color);
    }

    fn init_with_dims(
        pos: Vec2,
        dims: Vec2,
        text: []const u8,
        color: ConsoleColor
    ) TextBox {
        return TextBox{
            .rect = .{ .x = pos.x, .y = pos.y, .w = dims.x, .h = dims.y},
            .text = text,
            .color = color
        };
    }

    /// creates an ascii box like this:
    /// +--------------------+
    /// | previous text here |
    /// +--------------------+
    /// new text is allocated on the allocator.
    pub fn init_ascii_box(
        allocator: Allocator,
        rect: Rect,
        color: ConsoleColor
    ) !TextBox {
        if (rect.w < 2 or rect.h < 2) return error.BoxSizeTooSmall;

        var text = std.ArrayList(u8).init(allocator);
        const inside_width = @intCast(usize, rect.w) - 2;

        try text.append('.');
        try text.appendNTimes('-', inside_width);
        try text.appendSlice(".\n");

        var i: usize = 0;
        while (i < rect.h - 2) : (i += 1) {
            try text.append('|');
            try text.appendNTimes(' ', inside_width);
            try text.appendSlice("|\n");
        }

        try text.append('\'');
        try text.appendNTimes('-', inside_width);
        try text.append('\'');

        return TextBox{
            .rect = rect,
            .text = text.toOwnedSlice(),
            .color = color
        };
    }

    /// given multiline text, returns its dimensions
    fn detect_text_dims(text: []const u8) Vec2 {
        var longest: usize = 0;
        var lines: usize = 1;
        var current: usize = 0;
        for (text) |ch| {
            if (ch == '\n') {
                longest = std.math.max(longest, current);
                current = 0;
                lines += 1;
            }
            current += 1;
        }

        return Vec2{
            .x = @intCast(i32, std.math.max(longest, current)),
            .y = @intCast(i32, lines)
        };
    }
};

/// used to queue up messages (and other textboxes) for nice output
pub const TextCanvas = struct {
    boxes: std.MultiArrayList(TextBox),
    backing_allocator: Allocator,
    arena: std.heap.ArenaAllocator, // prefer using this as allocator

    pub fn init(allocator: Allocator) TextCanvas {
        var arena = std.heap.ArenaAllocator.init(allocator);

        return TextCanvas{
            .boxes = std.MultiArrayList(TextBox){},
            .backing_allocator = allocator,
            .arena = arena,
        };
    }

    pub fn deinit(self: *TextCanvas) void {
        self.boxes.deinit(self.backing_allocator);
        self.arena.deinit();
    }

    pub fn add_box(self: *TextCanvas, box: TextBox) Allocator.Error!void {
        try self.boxes.append(self.backing_allocator, box);
    }

    /// use for allocating text and stuff for text boxes
    pub fn temp_allocator(self: *TextCanvas) Allocator {
        return self.arena.allocator();
    }

    /// print outputs and 'consumes' textboxes
    pub fn print(self: *TextCanvas, writer: anytype) !void {
        const allocator = self.arena.allocator();

        // detect bounds of all textboxes
        const rects = self.boxes.items(.rect);
        var bounds = rects[0];

        for (rects[1..]) |rect| {
            if (rect.x < bounds.x) {
                bounds.w += bounds.x - rect.x;
                bounds.x = rect.x;
            }
            if (rect.y < bounds.y) {
                bounds.h += bounds.y - rect.y;
                bounds.y = rect.y;
            }

            const rect_maxx = rect.x + rect.w;
            const bounds_maxx = bounds.x + bounds.w;
            if (rect_maxx > bounds_maxx) {
                bounds.w += rect_maxx - bounds_maxx;
            }

            const rect_maxy = rect.y + rect.h;
            const bounds_maxy = bounds.y + bounds.h;
            if (rect_maxy > bounds_maxy) {
                bounds.h += rect_maxy - bounds_maxy;
            }
        }

        // print boxes onto 2d array
        const bounded_area = @intCast(usize, bounds.w * bounds.h);
        var text = try allocator.alloc(u8, bounded_area);
        var colors = try allocator.alloc(ConsoleColor, bounded_area);

        std.mem.set(u8, text, ' ');
        std.mem.set(ConsoleColor, colors, ConsoleColor{});

        // extrude text and colors
        var i: usize = 0;
        while (i < self.boxes.len) : (i += 1) {
            const box = self.boxes.get(i);

            var pos = Vec2{ .x = box.rect.x, .y = box.rect.y };
            for (box.text) |ch| {
                // find indices into 2d array
                const rel_pos = Vec2{
                    .x = pos.x - bounds.x,
                    .y = pos.y - bounds.y
                };
                const index = @intCast(
                    usize,
                    rel_pos.y * bounds.w + rel_pos.x
                );

                // write appropriate text, considering newlines
                if (ch == '\n') {
                    pos.x = box.rect.x;
                    pos.y += 1;
                } else {
                    text[index] = ch;
                    colors[index] = box.color;

                    pos.x += 1;
                }
            }
        }

        // print (endlich)
        var row: usize = 0;
        while (row < bounds.h) : (row += 1) {
            var cur_color = ConsoleColor{};

            const row_start = row * @intCast(usize, bounds.w);
            var col: usize = 0;
            while (col < bounds.w) : (col += 1) {
                const index = row_start + col;

                // print the color whenever it changes
                if (!colors[index].eql(cur_color)) {
                    cur_color = colors[index];
                    try writer.print("{}", .{cur_color});
                }

                // print the current character
                try writer.writeByte(text[index]);
            }

            // clear screen
            try writer.print("{}\n", .{ConsoleColor{}});
        }

        // clear textboxes
        try self.boxes.resize(self.backing_allocator, 0);
    }
};
