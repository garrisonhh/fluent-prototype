const std = @import("std");
const canvas = @import("util/canvas.zig");

const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();
const Allocator = std.mem.Allocator;
const TextBox = canvas.TextBox;
const ConsoleColor = canvas.ConsoleColor;

/// the FlFile abstraction allow for contextual errors.
const Self = @This();

const WriteError = std.fs.File.WriteError;
pub const Error = Allocator.Error
               || WriteError
               || error {
                   InvalidLocation,
               };

/// Message is a message relating to code in a lfile.
pub const Message = struct {
    const FlFile = Self;

    pub const Level = enum {
        debug,
        warning,
        err,
        note,

        fn color(self: Level) ConsoleColor {
            return switch (self) {
                .debug => ConsoleColor{ .fg = .green },
                .warning => ConsoleColor{ .fg = .yellow },
                .err => ConsoleColor{ .fg = .red  },
                .note => ConsoleColor{ .fg = .cyan },
            };
        }
    };

    lfile: *const FlFile,
    level: Level,
    msg: []const u8,
    slice: []const u8, // slice of lfile to be displayed with message
};

pub const Location = struct {
    const FlFile = Self;

    // these are stored zero-indexed for lispfileming convenience, and displayed
    // with 1 added
    filename: []const u8,
    line: usize,
    char: usize,

    pub fn of_slice(lfile: *const FlFile, slice: []const u8) Error!Location {
        // verify slice is within lfile
        const text = lfile.text;
        const text_pos = @ptrToInt(text.ptr);
        const slice_pos = @ptrToInt(slice.ptr);

        const within_text = slice_pos >= text_pos
                        and slice_pos - text_pos + slice.len <= text.len;

        if (!within_text) return Error.InvalidLocation;

        // count lines and get char
        var self = Location{
            .filename = lfile.name,
            .line = 0,
            .char = 0
        };

        for (text[0..slice_pos - text_pos]) |ch| {
            if (ch == '\n') {
                self.line += 1;
                self.char = 0;
            } else {
                self.char += 1;
            }
        }

        return self;
    }

    pub fn format(
        self: Location,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) WriteError!void {
        _ = fmt;
        _ = options;

        try std.fmt.format(writer, "{s}:{d}:{d}", .{
            self.filename,
            self.line,
            self.char,
        });
    }
};

/// File.Context can be used to collect messages and stop compilation to report
/// them throughout the various stages of compilation
pub const Context = struct {
    const FlFile = Self;

    ally: Allocator,
    arena: std.heap.ArenaAllocator,
    lfile: *const FlFile,
    messages: std.ArrayList(Message),
    // when an error is added to the context, this flag is set
    err: bool,

    pub fn init(ally: Allocator, lfile: *const FlFile) Context {
        return Context{
            .ally = ally,
            .arena = std.heap.ArenaAllocator.init(ally),
            .lfile = lfile,
            .messages = std.ArrayList(FlFile.Message).init(ally),
            .err = false,
        };
    }

    pub fn deinit(self: *Context) void {
        self.messages.deinit();
        self.arena.deinit();
    }

    pub fn temp_allocator(self: *Context) Allocator {
        return self.arena.allocator();
    }

    pub fn add_message(
        self: *Context,
        level: Message.Level,
        msg: []const u8,
        slice: []const u8
    ) Allocator.Error!void {
        if (level == .err) self.err = true;

        const message = self.lfile.new_message(level, msg, slice);
        try self.messages.append(message);
    }

    pub fn print_messages(self: *Context) !void {
        try FlFile.print_messages(self.ally, self.messages.items, stderr);
        self.messages.clearAndFree();
    }
};

name: []const u8,
text: []const u8,
lines: []const usize, // indices of the start of each line

pub fn init(ally: Allocator, name: []const u8, text: []const u8) !Self {
    // find lines
    var lines = std.ArrayList(usize).init(ally);

    var i: usize = 0;
    while (i < text.len) : (i += 1) {
        if (i == 0 or text[i - 1] == '\n') try lines.append(i);
    }
    try lines.append(text.len);

    return Self{
        .name = name,
        .text = text,
        .lines = lines.toOwnedSlice()
    };
}

pub fn deinit(self: *Self, ally: Allocator) void {
    ally.free(self.lines);
}

pub fn new_message(
    self: *const Self,
    level: Message.Level,
    msg: []const u8,
    slice: []const u8
) Message {
    return Message{
        .lfile = self,
        .level = level,
        .msg = msg,
        .slice = slice
    };
}

// used for collecting message batches for printing
const MessageQueue = struct {
    const FlFile = Self;
    const LocatedMessage = struct {
        level: Message.Level,
        msg: []const u8,
        loc: Location,
        len: usize,
        num_lines: usize,
    };

    lfile: *const FlFile,
    messages: std.ArrayList(LocatedMessage),

    fn init(lfile: *const FlFile, ally: Allocator) MessageQueue {
        return MessageQueue{
            .lfile = lfile,
            .messages = std.ArrayList(LocatedMessage).init(ally),
        };
    }

    fn deinit(self: *MessageQueue) void {
        self.messages.deinit();
    }

    fn add_message(self: *MessageQueue, message: *const Message) !void {
        try self.messages.append(LocatedMessage{
            .level = message.level,
            .msg = message.msg,
            .loc = try Location.of_slice(self.lfile, message.slice),
            .num_lines = std.mem.count(u8, message.slice, "\n") + 1,
            .len = message.slice.len
        });
    }
};

/// prints a line number to the canvas
fn print_line_num(tc: *canvas.TextCanvas, y: i32, line_num: usize) !void {
    const textbox_text = try std.fmt.allocPrint(
        tc.temp_allocator(),
        " {d} | ",
        .{line_num}
    );

    try tc.add_box(TextBox.init(
        .{ .x = -@intCast(i32, textbox_text.len), .y = y },
        textbox_text,
        ConsoleColor{ .fmt = .bold }
    ));
}

/// given a number of messages, print them in a nicely organized and readable
/// format for the user.
pub fn print_messages(
    ally: Allocator,
    messages: []const Message,
    writer: anytype
) Error!void {
    // collect messages by lfile, expanded into an easily textboxable format
    // (keyed by lfile name)
    var queues = std.StringArrayHashMap(*MessageQueue).init(ally);
    defer {
        for (queues.values()) |queue| {
            queue.deinit();
            ally.destroy(queue);
        }

        queues.deinit();
    }

    for (messages) |message| {
        // retrieve queue or make a new one
        var msg_queue =
            if (queues.get(message.lfile.name)) |found| found
            else make_new: {
                const new_queue = try ally.create(MessageQueue);
                new_queue.* = MessageQueue.init(message.lfile, ally);
                try queues.put(message.lfile.name, new_queue);

                break :make_new new_queue;
            };

        try msg_queue.add_message(&message);
    }

    // draw and print output for each message queue
    var tc = canvas.TextCanvas.init(ally);
    defer tc.deinit();
    const temp_allocator = tc.temp_allocator();

    for (queues.values()) |queue| {
        const lfile = queue.lfile;

        // print the lfile name
        try tc.add_box(TextBox.init(
            .{ .x = 0, .y = 0 },
            try std.fmt.allocPrint(temp_allocator, "in {s}:", .{lfile.name}),
            ConsoleColor{ .fmt = .bold }
        ));
        try tc.print(writer);

        for (queue.messages.items) |msg, msg_index| {
            // ellipses between code blocks
            if (msg_index > 0) try writer.writeByte('\n');

            // print the message's lines
            const start_line = msg.loc.line;
            const stop_line = msg.loc.line + msg.num_lines;

            const start_index = lfile.lines[start_line];
            // - 1 removes trailing '\n'
            const stop_index = lfile.lines[stop_line] - 1;

            try tc.add_box(TextBox.init(
                .{ .x = 0, .y = 0 },
                lfile.text[start_index..stop_index],
                ConsoleColor{}
            ));

            // line numbers
            var line = start_line;
            while (line < stop_line) : (line += 1) {
                const y = @intCast(i32, line - start_line);
                try print_line_num(&tc, y, line + 1);
            }

            // the message and starting arrow
            const level_color = msg.level.color();
            const char_x = @intCast(i32, msg.loc.char);

            var msg_text_box = TextBox.init(
                .{ .x = char_x, .y = -1 },
                msg.msg,
                level_color
            );
            msg_text_box.rect.y -= msg_text_box.rect.h;

            try tc.add_box(msg_text_box);
            try tc.add_box(TextBox.init(
                .{ .x = char_x, .y = -1 },
                "v",
                level_color
            ));

            // the end of span arrow
            if (msg.len > 1) {
                const stop_char = start_index + msg.loc.char
                                + msg.len - lfile.lines[stop_line - 1] - 1;

                try tc.add_box(TextBox.init(
                    .{
                        .x = @intCast(i32, stop_char),
                        .y = @intCast(i32, msg.num_lines)
                    },
                    "^",
                    level_color
                ));
            }

            try tc.print(writer);
        }
    }
}