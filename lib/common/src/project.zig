//! implementing storage and references into source code, as well as
//! contextual messaging which may reference source code.

const std = @import("std");
const fs = std.fs;
const Allocator = std.mem.Allocator;
const stderr = std.io.getStdErr().writer();
const kz = @import("kritzler");
const ResultMonad = @import("result.zig").Result;

pub const FileRef = struct {
    const Self = @This();

    index: usize,

    fn of(index: usize) Self {
        return Self{ .index = index };
    }
};

pub const Loc = struct {
    const Self = @This();

    file: FileRef,
    start: usize,
    stop: usize,

    pub fn of(file: FileRef, start: usize, stop: usize) Self {
        std.debug.assert(start <= stop);
        return Self{
            .file = file,
            .start = start,
            .stop = stop,
        };
    }

    /// location spanning both locs
    pub fn span(start: Self, stop: Self) Self {
        std.debug.assert(start.file.index == stop.file.index);
        return Loc.of(start.file, start.start, stop.stop);
    }

    pub fn length(self: Self) usize {
        return self.stop - self.start;
    }

    pub fn slice(self: Self, proj: Project) []const u8 {
        return proj.get(self.file).text[self.start..self.stop];
    }

    pub fn render(
        self: Self,
        ctx: *kz.Context,
        proj: Project,
    ) Allocator.Error!kz.Ref {
        const file = proj.get(self.file);
        const name = file.name;
        const text = file.text;

        const faint = kz.Style{ .special = .faint };
        const red = kz.Style{ .fg = .red };

        // find necessary char + line numbers
        var blk_start: usize = 0;
        var blk_stop = self.stop;
        var line_start: usize = 0;

        for (text[0..self.start]) |ch, i| {
            if (ch == '\n') {
                blk_start = i + 1;
                line_start += 1;
            }
        }

        const line_count = std.mem.count(u8, text[self.start..self.stop], "\n");
        const line_stop = line_start + line_count;

        for (text[blk_stop..]) |ch| {
            if (ch == '\n') break;
            blk_stop += 1;
        }

        var char_stop_line = blk_stop;
        while (char_stop_line > 0 and text[char_stop_line - 1] != '\n') {
            char_stop_line -= 1;
        }

        const char_start = self.start - blk_start;
        const char_stop = self.stop - char_stop_line;

        // render the header
        var header = try ctx.print(
            faint,
            "{s}:{}:{}",
            .{ name, line_start, self.start - blk_start },
        );

        if (self.stop != self.start) {
            const ext =
                if (line_start != line_stop)
                try ctx.print(faint, "-{}:{}", .{ line_stop, char_stop })
            else
                try ctx.print(faint, "-{}", .{char_stop});

            header = try ctx.slap(header, ext, .right, .{});
        }

        // render block text and arrows
        var source = try ctx.print(.{}, "{s}", .{text[blk_start..blk_stop]});

        const down_arrow = try ctx.print(red, "v", .{});
        const up_arrow = try ctx.print(red, "^", .{});

        source = try ctx.unify(
            source,
            down_arrow,
            .{ @intCast(isize, char_start), -1 },
        );

        if (self.stop != self.start) {
            const char_stop_display = if (char_stop > 0) char_stop - 1 else 0;

            source = try ctx.unify(
                source,
                up_arrow,
                .{
                    @intCast(isize, char_stop_display),
                    @intCast(isize, ctx.getSize(source)[1]),
                },
            );
        }

        // slap it and return
        return try ctx.slap(header, source, .bottom, .{});
    }
};

/// project is the central storage for files
///
/// struct itself can be passed around by value
pub const Project = struct {
    const Self = @This();

    const File = struct {
        name: []u8,
        text: []u8,
    };

    comptime {
        // I want this struct to remain smallish
        if (@sizeOf(Self) > 32) {
            @compileLog(@sizeOf(Self)); // project struct is too large
        }
    }

    cwd: fs.Dir,
    files: std.ArrayListUnmanaged(File) = .{},

    pub fn init() Self {
        return Self{
            .cwd = fs.cwd(),
        };
    }

    pub fn deinit(self: *Self, ally: Allocator) void {
        for (self.files.items) |file| {
            ally.free(file.name);
            ally.free(file.text);
        }
        self.files.deinit(ally);
    }

    fn get(self: Self, ref: FileRef) File {
        return self.files.items[ref.index];
    }

    pub fn getName(self: Self, ref: FileRef) []const u8 {
        return self.get(ref).name;
    }

    pub fn getText(self: Self, ref: FileRef) []const u8 {
        return self.get(ref).text;
    }

    pub fn getSlice(self: Self, loc: Loc) []const u8 {
        const text = self.getText(loc.file);
        return text[loc.start..loc.stop];
    }

    pub fn register(
        self: *Self,
        ally: Allocator,
        name: []const u8,
        text: []const u8,
    ) Allocator.Error!FileRef {
        const ref = FileRef.of(self.files.items.len);
        try self.files.append(ally, File{
            .name = try ally.dupe(u8, name),
            .text = try ally.dupe(u8, text),
        });

        return ref;
    }

    // zig fmt: off
    pub const LoadError =
        Allocator.Error
     || fs.File.OpenError
     || std.os.ReadError
     || std.os.GetCwdError;
    // zig fmt: on

    /// load file from disk
    pub fn load(
        self: *Self,
        ally: Allocator,
        path: []const u8,
    ) LoadError!FileRef {
        // load file text
        const file = try self.cwd.openFile(path, .{ .mode = .read_only });
        defer file.close();

        const size = (try file.stat()).size;
        const text = try file.readToEndAlloc(ally, size);

        // normalize relative path
        const name = try fs.path.relative(ally, ".", path);
        defer ally.free(name);

        // register file
        return self.register(ally, name, text);
    }
};

/// messages are how the fluent compiler generates output for the user
pub const Message = struct {
    const Self = @This();

    pub fn Result(comptime T: type) type {
        return ResultMonad(T, Self);
    }

    pub const Tag = enum {
        log,
        @"error",
        internal,

        fn render(tag: Tag, ctx: *kz.Context) Allocator.Error!kz.Ref {
            const sty: kz.Style = switch (tag) {
                .log => .{ .fg = .cyan },
                .@"error" => .{ .fg = .red },
                .internal => .{ .fg = .green },
            };

            return try ctx.stack(&.{
                try ctx.print(.{}, "[", .{}),
                try ctx.print(sty, "{s}", .{@tagName(tag)}),
                try ctx.print(.{}, "]", .{}),
            }, .right, .{});
        }
    };

    tag: Tag,
    loc: ?Loc,
    text: []u8,

    pub fn print(
        ally: Allocator,
        tag: Tag,
        loc: ?Loc,
        comptime fmt: []const u8,
        args: anytype,
    ) Allocator.Error!Self {
        return Self{
            .tag = tag,
            .loc = loc,
            .text = try std.fmt.allocPrint(ally, fmt, args),
        };
    }

    pub fn deinit(self: Self, ally: Allocator) void {
        ally.free(self.text);
    }

    pub fn render(
        self: Self,
        ctx: *kz.Context,
        proj: Project,
    ) Allocator.Error!kz.Ref {
        var ref = try ctx.slap(
            try self.tag.render(ctx),
            try ctx.print(.{}, "{s}", .{self.text}),
            .right,
            .{ .space = 1 },
        );

        if (self.loc) |loc| {
            const source = try loc.render(ctx, proj);
            ref = try ctx.slap(ref, source, .bottom, .{});
        }

        return ref;
    }
};
