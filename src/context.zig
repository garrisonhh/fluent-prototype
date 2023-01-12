//! this provides global access to and storage for source files, and implements
//! contextual messaging for source code.
//!
//! files are stored and accessed through a globally accessible data structure.
//! this allows file locations to be stored in a compact format.

const std = @import("std");
const builtin = @import("builtin");
const kz = @import("kritzler");
const util = @import("util");

const Allocator = std.mem.Allocator;

const stderr = std.io.getStdErr().writer();

/// this error is used to signify that the compiler should fail and the message
/// tree should be displayed
pub const FluentError = error { FluentError };

/// all fields owned by FileStorage
const File = struct {
    name: []const u8,
    text: []const u8,
    lines: [][]const u8,
};

/// index into this.files
pub const FileHandle = struct {
    const Self = @This();
    const Index = u32;

    index: Index,

    pub fn getName(self: Self) []const u8 {
        return this.files.items[self.index].name;
    }

    pub fn getSource(self: Self) []const u8 {
        return this.files.items[self.index].text;
    }

    pub fn getLines(self: Self) []const []const u8 {
        return this.files.items[self.index].lines;
    }
};

/// used to reference into source files
pub const Loc = struct {
    const Self = @This();
    const CharIndex = u32;

    comptime {
        std.debug.assert(@sizeOf(Self) <= 16);
    }

    /// stored zero-indexed
    line: u32,
    /// character span relative to the line
    /// stored zero-indexed
    span: struct {
        start: CharIndex,
        len: CharIndex,
    },
    file: FileHandle,

    pub fn init(
        file: FileHandle,
        line: usize,
        char_start: usize,
        char_len: usize,
    ) Self {
        std.debug.assert(char_start + char_len <= file.getLines()[line].len);

        return Self{
            .file = file,
            .line = @intCast(u32, line),
            .span = .{
                .start = @intCast(u32, char_start),
                .len = @intCast(u32, char_len),
            }
        };
    }

    /// returns a loc that represents the beginning of this loc
    pub fn beginning(self: Self) Self {
        return Self{
            .line = self.line,
            .span = .{ .start = self.span.start, .len = 0 },
            .file = self.file
        };
    }

    pub fn getSlice(self: Self) []const u8 {
        const start = self.span.start;
        return self.file.getLines()[self.line][start..start + self.span.len];
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        _ = fmt;
        _ = options;

        const path = this.files.items[self.file.index].name;
        // this should never fail since the path is verified on loadSource()
        const rel_path = std.fs.path.relative(this.ally, ".", path)
                         catch unreachable;
        defer this.ally.free(rel_path);

        try writer.print(
            "{s}:{}:",
            .{rel_path, self.line + 1}
        );

        if (self.span.len > 0) {
            try writer.print(
                "{}-{}",
                .{self.span.start + 1, self.span.start + self.span.len}
            );
        } else {
            try writer.print(
                "{}",
                .{self.span.start + 1}
            );
        }
    }
};

const GlobalContext = struct {
    const Self = @This();

    ally: Allocator,
    // arena is for stuff that is only written to once in the whole program,
    // which is *most* of the file and message stuff
    arena: std.heap.ArenaAllocator,

    files: std.ArrayListUnmanaged(File),
    messages: MessageTree,

    fn tmp_ally(self: *Self) Allocator {
        return self.arena.allocator();
    }
};

var this: GlobalContext = undefined;

pub fn init(ally: Allocator) Allocator.Error!void {
    this = GlobalContext{
        .ally = ally,
        .arena = std.heap.ArenaAllocator.init(ally),
        .files = .{},
        .messages = .{},
    };

    _ = try addSource(
        "nowhere",
        \\TODO the fluent context system doesn't know what to do with
        \\values that were produced by bytecode execution (yet, at least)
        \\so in the meantime, they point to this file.
    );
}

pub fn deinit() void {
    this.files.deinit(this.ally);
    this.messages.deinit();
    this.arena.deinit();
}

/// expects `this` to own both params
fn addSource(name: []const u8, text: []const u8) Allocator.Error!FileHandle {
    const ally = this.tmp_ally();

    // create handle
    const handle = FileHandle{
        .index = @intCast(FileHandle.Index, this.files.items.len)
    };

    // find lines, verify char + line limits
    var lines = std.ArrayList([]const u8).init(this.ally);
    defer lines.deinit();

    var line_start: usize = 0;
    for (text) |ch, i| {
        if (ch == '\n') {
            const line = text[line_start..i];

            try lines.append(line);
            line_start = i + 1;
        }
    }

    // test for terminating line without '\n'
    if (line_start < text.len) {
        try lines.append(text[line_start..]);
    }

    // store file and return
    try this.files.append(this.ally, File{
        .name = name,
        .text = text,
        .lines = try ally.dupe([]const u8, lines.items),
    });

    return handle;
}

pub fn addExternalSource(
    name: []const u8,
    text: []const u8
) Allocator.Error!FileHandle {
    const ally = this.tmp_ally();
    return addSource(
        try ally.dupe(u8, name),
        try ally.dupe(u8, text),
    );
}

pub const LoadSourceError =
     Allocator.Error
  || std.os.RealPathError
  || std.os.ReadError
  || std.fs.File.OpenError;

pub fn loadSource(path: []const u8) LoadSourceError!FileHandle {
    const ally = this.tmp_ally();

    // get the path and file
    const abs_path = try std.fs.cwd().realpathAlloc(ally, path);
    const fs_file = try std.fs.openFileAbsolute(abs_path, .{});
    defer fs_file.close();

    // read and add to storage
    const max_bytes = std.math.maxInt(Loc.CharIndex);
    const text = try fs_file.readToEndAlloc(ally, max_bytes);

    return try addSource(path, text);
}

pub const Message = struct {
    const Self = @This();

    const Level = enum {
        note,

        err,
        warn,
        log,
        debug,

        fn getStyle(self: @This()) kz.Style {
            return switch (self) {
                .note => .{ .fg = .cyan },
                .err => .{ .fg = .red },
                .warn => .{ .fg = .magenta },
                .log => .{},
                .debug => .{ .fg = .green },
            };
        }
    };

    level: Level,
    loc: ?Loc,
    text: []const u8,
    children: std.ArrayListUnmanaged(Self),

    fn init(
        level: Level,
        loc: ?Loc,
        comptime fmt: []const u8,
        args: anytype
    ) MessageError!Self {
        return Self{
            .level = level,
            .loc = loc,
            .text = try std.fmt.allocPrint(this.ally, fmt, args),
            .children = .{}
        };
    }

    fn deinit(self: *Self) void {
        this.ally.free(self.text);
        for (self.children.items) |*child| child.deinit();
        self.children.deinit(this.ally);
    }

    /// use this to create cascading messages
    pub fn annotate(
        self: *Self,
        loc: ?Loc,
        comptime fmt: []const u8,
        args: anytype
    ) MessageError!*Self {
        const msg_ptr = try self.children.addOne(this.ally);
        msg_ptr.* = try Message.init(.note, loc, fmt, args);

        return msg_ptr;
    }

    fn render(self: Self, ctx: *kz.Context) Allocator.Error!kz.Ref {
        const INDENT = 4;

        // message header
        const level_tag = @tagName(self.level);
        const head_refs = [_]kz.Ref{
            try ctx.print(.{}, "[", .{}),
            try ctx.print(self.level.getStyle(), "{s}", .{level_tag}),
            try ctx.print(.{}, "] ", .{}),
            try ctx.print(.{}, "{s}", .{self.text}),
        };
        const head = try ctx.stack(&head_refs, .right, .{});

        // render with contextual location
        // TODO

        // append annotations
        var notes = try ctx.stub();
        for (self.children.items) |child| {
            notes = try ctx.slap(notes, try child.render(ctx), .bottom, .{});
        }

        // put it all together
        const final = try ctx.unify(head, notes, .{INDENT, 1});
        return final;
    }
};

/// container for all messages in the global context
const MessageTree = struct {
    const Self = @This();

    map: std.AutoHashMapUnmanaged(
        FileHandle,
        std.ArrayListUnmanaged(Message)
    ) = .{},

    fn deinit(self: *Self) void {
        var list_iter = self.map.valueIterator();
        while (list_iter.next()) |list| {
            for (list.items) |*msg| msg.deinit();
            list.deinit(this.ally);
        }

        self.map.deinit(this.ally);
    }

    fn allocMessage(
        self: *Self,
        file: FileHandle
    ) Allocator.Error!*Message {
        const res = try self.map.getOrPut(this.ally, file);
        if (!res.found_existing) {
            res.value_ptr.* = std.ArrayListUnmanaged(Message){};
        }

        return try res.value_ptr.addOne(this.ally);
    }

    fn render(self: Self, ctx: *kz.Context) Allocator.Error!kz.Ref {
        var texs = std.ArrayList(kz.Ref).init(this.ally);
        defer texs.deinit();

        var lists = self.map.valueIterator();
        while (lists.next()) |list| {
            for (list.items) |msg| {
                try texs.append(try msg.render(ctx));
            }
        }

        return try ctx.stack(texs.items, .bottom, .{ .space = 1 });
    }

    fn clear(self: *Self) void {
        var lists = self.map.valueIterator();
        while (lists.next()) |list| {
            for (list.items) |*msg| msg.deinit();
            list.shrinkAndFree(this.ally, 0);
        }

        self.map.clearRetainingCapacity();
    }
};

pub const MessageError =
    Allocator.Error
 || std.fmt.AllocPrintError
 || @TypeOf(stderr).Error;

/// generates a new message which may have sub-messages
pub fn post(
    level: Message.Level,
    loc: ?Loc,
    comptime fmt: []const u8,
    args: anytype
) MessageError!*Message {
    const file: FileHandle = if (loc) |l| l.file else FileHandle{ .index = 0 };
    const msg_ptr = try this.messages.allocMessage(file);
    msg_ptr.* = try Message.init(level, loc, fmt, args);

    return msg_ptr;
}

pub fn flushMessages() MessageError!void {
    defer this.messages.clear();

    var ctx = kz.Context.init(this.ally);
    defer ctx.deinit();

    try ctx.write(try this.messages.render(&ctx), stderr);
}