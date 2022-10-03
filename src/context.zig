//! this provides access to and storage for source files, and implements
//! contextual messaging for language source code.
//!
//! files are stored and accessed through a globally accessible data structure.
//! this allows file locations to be stored in a compact format.

const std = @import("std");
const builtin = @import("builtin");
const kz = @import("kritzler");

const Allocator = std.mem.Allocator;

/// all fields owned by FileStorage
const File = struct {
    name: []const u8,
    text: []const u8,
    lines: [][]const u8,
};

/// index into this.files
pub const FileHandle = struct {
    const Index = u16;
    const MAX_FILES = std.math.maxInt(Index);

    index: Index,
};

/// used to reference into source files
pub const Loc = struct {
    const Self = @This();

    comptime {
        std.debug.assert(@sizeOf(@This()) == 8);
    }

    const LineIndex = u32;
    const CharIndex = u16;

    const MAX_LINES = std.math.maxInt(LineIndex);
    const MAX_LINE_LENGTH = std.math.maxInt(CharIndex);

    line: LineIndex,
    char: CharIndex,
    file: FileHandle,

    pub fn init(file: FileHandle, line: usize, char: usize) Self {
        return Self{
            .file = file,
            .line = @intCast(LineIndex, line),
            .char = @intCast(CharIndex, char),
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
        const rel_path = std.fs.path.relative(this.ally, ".", path)
                         catch unreachable;
        defer this.ally.free(rel_path);

        try writer.print(
            "{s}:{}:{}",
            .{rel_path, self.line + 1, self.char + 1}
        );
    }
};

const FileStorage = struct {
    const Self = @This();
    const Files = std.ArrayListUnmanaged(File);

    ally: Allocator,
    text_arena: std.heap.ArenaAllocator,
    files: Files,

    fn text_ally(self: *Self) Allocator {
        return self.text_arena.allocator();
    }
};

var this: FileStorage = undefined;

pub fn init(ally: Allocator) void {
    this = FileStorage{
        .ally = ally,
        .text_arena = std.heap.ArenaAllocator.init(ally),
        .files = .{},
    };
}

pub fn deinit() void {
    this.text_arena.deinit();
    this.files.deinit(this.ally);
}

pub const AddSourceError = error {
    TooManyFiles,
    TooManyLines,
    LineTooLong,
} || Allocator.Error;

/// expects `this` to own both params
fn addSource(name: []const u8, text: []const u8) AddSourceError!FileHandle {
    const ally = this.text_ally();

    // create handle
    if (this.files.items.len > FileHandle.MAX_FILES) {
        return error.TooManyFiles;
    }

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

            if (line.len > Loc.MAX_LINE_LENGTH) {
                return error.LineTooLong;
            }

            try lines.append(line);
            line_start = i + 1;
        }
    }

    // test for terminating line without '\n'
    if (line_start < text.len) {
        try lines.append(text[line_start..]);
    }

    if (lines.items.len > Loc.MAX_LINES) {
        return error.TooManyLines;
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
) AddSourceError!FileHandle {
    const ally = this.text_ally();

    return try addSource(
        try ally.dupe(u8, name),
        try ally.dupe(u8, text),
    );
}

pub const LoadSourceError =
     AddSourceError
  || std.os.RealPathError
  || std.os.ReadError
  || std.fs.File.OpenError;

pub fn loadSource(path: []const u8) LoadSourceError!FileHandle {
    const ally = this.text_ally();

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

/// TODO remove; debugging
pub fn displayFile(handle: FileHandle) !void {
    const file = this.files.items[handle.index];

    var canvas = kz.Canvas.init(this.ally);
    defer canvas.deinit();

    try canvas.scribble(
        .{0, -1},
        kz.Color{ .fg = .cyan },
        "file \"{s}\":",
        .{file.name}
    );

    for (file.lines) |line, i| {
        const txt = try canvas.print("{} | ", .{i + 1});
        const y = @intCast(isize, i);

        try canvas.scribble(
            .{-@intCast(isize, txt.len), y},
            kz.Color{ .fmt = .bold },
            "{s}",
            .{txt}
        );

        try canvas.scribble(.{0, y}, kz.Color{}, "{s}", .{line});
    }

    try canvas.flush(std.io.getStdOut().writer());
}

/// this error is used to signify that stack traces should be printed and the
/// compiler should fail
pub const FluentError = error { FluentError };

// TODO pub fn addError {}
