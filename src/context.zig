//! this provides access to and storage for source files, and implements
//! contextual messaging for language source code.
//!
//! files are stored and accessed through a globally accessible data structure.
//! this allows file locations to be stored in a compact format.

const std = @import("std");
const builtin = @import("builtin");
const kz = @import("kritzler");
const util = @import("util/util.zig");

const Allocator = std.mem.Allocator;

const stderr = std.io.getStdErr().writer();

/// this error is used to signify that the compiler should fail and the message
/// tree should be displayed
pub const FluentError = error { FluentError };

/// given an error union from a function which contains FluentError, this
/// flushes messages if FluentError is returned.
pub fn wrap(value: anytype) t: {
    const info = @typeInfo(@TypeOf(value)).ErrorUnion;
    const flush_info = @typeInfo(@TypeOf(flushMessages)).Fn;
    const flush_err = @typeInfo(flush_info.return_type.?).ErrorUnion.error_set;
    break :t (info.error_set || flush_err)!info.payload;
} {
    _ = value catch |e| if (e == error.FluentError) {
        try flushMessages();
    };

    return try value;
}

/// all fields owned by FileStorage
const File = struct {
    name: []const u8,
    text: []const u8,
    lines: [][]const u8,
};

/// index into this.files
pub const FileHandle = struct {
    const Index = u32;
    const MAX_FILES = std.math.maxInt(Index);

    index: Index,
};

/// used to reference into source files
pub const Loc = struct {
    const Self = @This();

    comptime {
        std.debug.assert(@sizeOf(Self) <= 16);
    }

    /// stored zero-indexed
    line: u32,
    /// character span relative to the line
    /// stored zero-indexed
    span: struct {
        start: u32,
        len: u32,
    },
    file: FileHandle,

    pub fn init(
        file: FileHandle,
        line: usize,
        char_start: usize,
        char_len: usize,
    ) Self {
        return Self{
            .file = file,
            .line = @intCast(u32, line),
            .span = .{
                .start = @intCast(u32, char_start),
                .len = @intCast(u32, char_len),
            }
        };
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

pub fn init(ally: Allocator) void {
    this = GlobalContext{
        .ally = ally,
        .arena = std.heap.ArenaAllocator.init(ally),
        .files = .{},
        .messages = .{},
    };
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
    const text = try fs_file.readToEndAlloc(ally, std.math.maxInt(u64));

    return try addSource(abs_path, text);
}

pub fn getSource(handle: FileHandle) []const u8 {
    return this.files.items[handle.index].text;
}

pub fn getLines(handle: FileHandle) []const []const u8 {
    return this.files.items[handle.index].lines;
}

pub const Message = struct {
    const Self = @This();

    const Level = enum {
        note,

        err,
        warn,
        log,
        debug,

        const meta = util.EnumTable(@This(), kz.Format).init(.{
            .{.note,  kz.Format{                }},
            .{.err,   kz.Format{ .fg = .red     }},
            .{.warn,  kz.Format{ .fg = .magenta }},
            .{.log,   kz.Format{ .fg = .cyan    }},
            .{.debug, kz.Format{ .fg = .green   }},
        });

        fn getFormat(self: @This()) kz.Format {
            return @This().meta.get(self);
        }
    };

    level: Level,
    loc: ?Loc,
    text: []const u8,
    children: std.ArrayListUnmanaged(Self),

    fn init(level: Level, loc: ?Loc, text: []const u8) Self {
        return Self{
            .level = level,
            .loc = loc,
            .text = text,
            .children = .{}
        };
    }

    fn deinit(self: *Self) void {
        self.children.deinit(this.ally);
    }

    /// use this to create cascading messages
    pub fn annotate(
        self: *Self,
        loc: ?Loc,
        text: []const u8
    ) Allocator.Error!*Self {
        try self.children.append(this.ally, Message.init(.note, loc, text));
    }

    fn render(self: Self, ally: Allocator) Allocator.Error!kz.Texture {
        // message text
        const text_tex = try kz.Texture.from(ally, kz.Format{}, self.text);
        defer text_tex.deinit(ally);

        // text + level label
        const labeled_tex = if (self.level != .note) msg: {
            const tag = @tagName(self.level);
            var tag_tex = try kz.Texture.init(ally, .{tag.len + 3, 1});
            defer tag_tex.deinit(ally);

            tag_tex.set(.{0, 0}, kz.Format{}, '[');
            tag_tex.write(.{1, 0}, self.level.getFormat(), tag);
            tag_tex.write(.{tag.len + 1, 0}, kz.Format{}, "] ");

            break :msg try text_tex.slap(ally, &tag_tex, .left, .center);
        } else try text_tex.clone(ally);
        defer labeled_tex.deinit(ally);

        // contextual location
        const located_tex = if (self.loc) |loc| locate: {
            const faint_fmt = kz.Format{ .special = .faint };

            // `loc` header
            const header_tex = try kz.Texture.print(
                ally,
                faint_fmt,
                "{}:",
                .{loc}
            );
            defer header_tex.deinit(ally);

            // line with line number
            const line_num_tex = try kz.Texture.print(
                ally,
                faint_fmt,
                "{} | ",
                .{loc.line + 1}
            );
            defer line_num_tex.deinit(ally);

            const line = getLines(loc.file)[loc.line];
            const line_tex = try kz.Texture.from(ally, kz.Format{}, line);
            defer line_tex.deinit(ally);

            const numbered_tex =
                try line_tex.slap(ally, &line_num_tex, .left, .close);
            defer numbered_tex.deinit(ally);

            // span highlighting (arrow or underline)
            const hl_fmt = self.level.getFormat();
            const hl_to_x =
                @intCast(isize, line_num_tex.size[0] + loc.span.start);

            const highlighted_tex = if (loc.span.len > 0) underline: {
                var ul_tex = try kz.Texture.init(ally, .{loc.span.len, 1});
                defer ul_tex.deinit(ally);

                ul_tex.fill(hl_fmt, '~');

                break :underline
                    try numbered_tex.unify(ally, &ul_tex, .{hl_to_x, 1});
            } else arrow: {
                const arrow_tex = try kz.Texture.from(ally, hl_fmt, "v");
                defer arrow_tex.deinit(ally);

                break :arrow
                    try numbered_tex.unify(ally, &arrow_tex, .{hl_to_x, -1});
            };
            defer highlighted_tex.deinit(ally);

            // slap it together!
            break :locate
                try highlighted_tex.slap(ally, &header_tex, .top, .close);
        } else try labeled_tex.clone(ally);
        defer located_tex.deinit(ally);

        // TODO notes

        return try labeled_tex.slap(ally, &located_tex, .bottom, .close);
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

    fn allocMessage(self: *Self, file: FileHandle) Allocator.Error!*Message {
        const res = try self.map.getOrPut(this.ally, file);

        if (!res.found_existing) {
            res.value_ptr.* = std.ArrayListUnmanaged(Message){};
        }

        return try res.value_ptr.addOne(this.ally);
    }

    fn render(self: Self, ally: Allocator) Allocator.Error!kz.Texture {
        var tex = try kz.Texture.init(ally, .{0, 0});

        var lists = self.map.valueIterator();
        while (lists.next()) |list| {
            for (list.items) |msg| {
                const msg_tex = try msg.render(ally);
                defer msg_tex.deinit(ally);

                // append msg to message list
                const new_tex = try tex.slap(
                    ally,
                    &msg_tex,
                    .bottom,
                    .close
                );

                tex.deinit(ally);
                tex = new_tex;
            }
        }

        return tex;
    }

    fn clear(self: *Self) void {
        var lists = self.map.valueIterator();
        while (lists.next()) |list| list.shrinkAndFree(this.ally, 0);

        self.map.clearRetainingCapacity();
    }
};

/// generates a new message which may have sub-messages
pub fn postMessage(
    level: Message.Level,
    loc: Loc,
    text: []const u8
) Allocator.Error!*Message {
    const msg_ptr = try this.messages.allocMessage(loc.file);
    msg_ptr.* = Message.init(level, loc, text);

    return msg_ptr;
}

pub fn flushMessages() (Allocator.Error || @TypeOf(stderr).Error)!void {
    const tmp_ally = this.arena.allocator();

    const tex = try this.messages.render(tmp_ally);
    defer tex.deinit(tmp_ally);
    defer this.messages.clear();

    try tex.display(stderr);

}