//! context manages memory for kritzler chunks, which allows kritzler users
//! to generally be able to treat chunks as primitives.
//!
//! the only weirdness that this creates is the possibility of use-after-free
//! bugs, since Refs follow move semantics. luckily, this is checkable at
//! runtime at least, and manual chunk cloning is pretty intuitive. kritzler
//! also encourages a pure style that will make these bugs rare.

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const style = @import("style.zig");
const Color = style.Color;
const Style = style.Style;
const types = @import("types.zig");
const Pos = types.Pos;
const Offset = types.Offset;
const Rect = types.Rect;

const Self = @This();

/// the kritzler primitive
const Chunk = struct {
    gen: u32,
    styles: []Style,
    text: []u8,
    size: Pos,

    fn deinit(self: @This(), ally: Allocator) void {
        ally.free(self.styles);
        ally.free(self.text);
    }
};

pub const Ref = packed struct(u64) { gen: u32, index: u32 };

ally: Allocator,
chunks: std.ArrayListUnmanaged(Chunk) = .{},
reusable: std.ArrayListUnmanaged(u32) = .{},

pub fn init(ally: Allocator) Self {
    return Self{
        .ally = ally,
    };
}

pub fn deinit(self: *Self) void {
    for (self.chunks.items) |chunk| chunk.deinit(self.ally);
    self.chunks.deinit(self.ally);
    self.reusable.deinit(self.ally);
}

pub fn numActiveRefs(self: Self) usize {
    return self.chunks.items.len - self.reusable.items.len;
}

fn assertCurrent(self: *Self, ref: Ref) void {
    if (builtin.mode == .Debug) {
        if (self.chunks.items[ref.index].gen != ref.gen) {
            std.debug.panic(
                "mismatched chunk generation. remember to clone your chunks.",
                .{}
            );
        }
    }
}

/// remember that addChunk can invalidate this pointer
fn get(self: *Self, ref: Ref) *Chunk {
    self.assertCurrent(ref);
    return &self.chunks.items[ref.index];
}

/// frees up a chunk for reuse
pub fn drop(self: *Self, ref: Ref) void {
    self.get(ref).gen += 1;
    // capacity is assured in new()
    self.reusable.appendAssumeCapacity(ref.index);
}

// creating chunks =============================================================

/// creates a chunk, returns pointer for convenience
fn new(self: *Self, size: Pos) Allocator.Error!Ref {
    var index: u32 = undefined;
    var chunk: *Chunk = undefined;

    // get chunk + index
    if (self.reusable.items.len > 0) {
        // reuse old slot
        index = self.reusable.pop();
        chunk = &self.chunks.items[index];
        chunk.deinit(self.ally);
    } else {
        // create new slot
        index = @intCast(u32, self.chunks.items.len);
        chunk = try self.chunks.addOne(self.ally);
        chunk.gen = 0;
        try self.reusable.ensureTotalCapacity(self.ally, self.chunks.items.len);
    }

    chunk.size = size;

    // allocate chunk memory
    const mem_size = size[0] * size[1];
    chunk.styles = try self.ally.alloc(Style, mem_size);
    chunk.text = try self.ally.alloc(u8, mem_size);

    return Ref{
        .gen = chunk.gen,
        .index = index,
    };
}

pub fn block(self: *Self, size: Pos, sty: Style, ch: u8) Allocator.Error!Ref {
    const ref = try self.new(size);
    const chunk = self.get(ref);

    std.mem.set(Style, chunk.styles, sty);
    std.mem.set(u8, chunk.text, ch);

    return ref;
}

/// create a new, blank chunk of a certain size
pub fn blank(self: *Self, size: Pos) Allocator.Error!Ref {
    const ref = try self.new(size);
    const chunk = self.get(ref);

    std.mem.set(Style, chunk.styles, .{});
    std.mem.set(u8, chunk.text, ' ');

    return ref;
}

/// create a chunk without a size (useful in some situations)
pub fn stub(self: *Self) Allocator.Error!Ref {
    return self.new(.{0, 0});
}

/// create a copy of another chunk
pub fn clone(self: *Self, ref: Ref) Allocator.Error!Ref {
    const old = self.get(ref).*;
    const new_ref = try self.new(old.size);
    const chunk = self.get(new_ref);

    std.mem.copy(Style, chunk.styles, old.styles);
    std.mem.copy(u8, chunk.text, old.text);

    return new_ref;
}

pub const PrintError = Allocator.Error || std.fmt.AllocPrintError;

/// print to a new chunk
pub fn print(
    self: *Self,
    sty: Style,
    comptime fmt: []const u8,
    args: anytype
) PrintError!Ref {
    // get naive printed text
    const text = try std.fmt.allocPrint(self.ally, fmt, args);
    defer self.ally.free(text);

    // extract lines
    var lines = std.ArrayList([]const u8).init(self.ally);
    defer lines.deinit();

    var line_iter = std.mem.split(u8, text, "\n");
    while (line_iter.next()) |line| try lines.append(line);

    // remove trailing newlines
    while (lines.items.len > 0) {
        const last = lines.items[lines.items.len - 1];
        if (last.len > 0) break;
        _ = lines.pop();
    }

    // find size of chunk
    var size = Pos{ 0, lines.items.len };
    for (lines.items) |line| size[0] = @max(size[0], line.len);

    // write style + text to chunk, filling the gaps with spaces
    const ref = try self.new(size);
    const chunk = self.get(ref);

    std.mem.set(Style, chunk.styles, sty);

    var i: usize = 0;
    for (lines.items) |line| {
        const next = i + size[0];
        const dest = chunk.text[i..next];
        i = next;

        std.mem.copy(u8, dest, line);
        std.mem.set(u8, dest[line.len..], ' ');
    }

    return ref;
}

// getters =====================================================================

pub fn getSize(self: *Self, ref: Ref) Pos {
    return self.get(ref).size;
}

// chunk operations ============================================================

/// write src over dst at a position. this expects everything to already be
/// bounds-checked.
fn blit(dst: *Chunk, src: *const Chunk, pos: Pos) void {
    var y: usize = 0;
    while (y < src.size[1]) : (y += 1) {
        // get src slices
        const start = y * src.size[0];
        const end = start + src.size[0];
        const line_styles = src.styles[start..end];
        const line = src.text[start..end];

        // find dst index and memcpy
        const cursor = (pos[1] + y) * dst.size[0] + pos[0];
        std.mem.copy(Style, dst.styles[cursor..], line_styles);
        std.mem.copy(u8, dst.text[cursor..], line);
    }
}

/// draw chunk `b` over chunk `a` at offset `to`
/// drops both chunks
pub fn unify(
    self: *Self,
    a: Ref,
    b: Ref,
    to: Offset
) Allocator.Error!Ref {
    // find size of new chunk
    const target = Rect.init(to, self.getSize(b));
    const unified = target.unionWith(Rect.init(.{0, 0}, self.getSize(a)));

    // create new chunk and blit
    const ref = try self.blank(unified.size);
    const chunk = self.get(ref);

    blit(chunk, self.get(a), types.toPos(-unified.offset));
    blit(chunk, self.get(b), types.toPos(to - unified.offset));
    self.drop(a);
    self.drop(b);

    return ref;
}

pub const SlapAlign = enum { close, center, far };
pub const SlapDirection = enum {
    left,
    right,
    top,
    bottom,
    fn flip(self: @This()) @This() {
        return switch (self) {
            .left => .right,
            .right => .left,
            .top => .bottom,
            .bottom => .top,
        };
    }
};

fn slapOffset(size: Pos, dir: SlapDirection, aln: SlapAlign) Offset {
    const ssize = types.toOffset(size);

    // find side vertices
    const a: Offset = switch (dir) {
        .left, .top => .{0, 0},
        .right => .{ssize[0], 0},
        .bottom => .{0, ssize[1]},
    };
    const b: Offset = switch (dir) {
        .left => .{0, ssize[1]},
        .top => .{ssize[0], 0},
        .right, .bottom => ssize,
    };

    // interpolate with alignment
    return a + switch (aln) {
        .close => Offset{ 0, 0 },
        .center => (b - a) / Offset{ 2, 2 },
        .far => b - a,
    };
}

fn slapSpacing(dir: SlapDirection, space: usize) Offset {
    const ispace = @intCast(isize, space);
    return switch (dir) {
        .left => .{-ispace, 0},
        .right => .{ispace, 0},
        .top => .{0, -ispace},
        .bottom => .{0, ispace},
    };
}

pub const SlapOpt = struct {
    aln: SlapAlign = .close,
    space: usize = 0,
};

/// instead of using numbers, slap lets you declare qualitatively where to
/// put b in relation to a.
///
/// drops both chunks
pub fn slap(
    self: *Self,
    a: Ref,
    b: Ref,
    dir: SlapDirection,
    opt: SlapOpt
) Allocator.Error!Ref {
    const dst_offset = slapOffset(self.getSize(a), dir, opt.aln);
    const src_offset = slapOffset(self.getSize(b), dir.flip(), opt.aln);
    const spacing = slapSpacing(dir, opt.space);
    const final = dst_offset - src_offset + spacing;

    return try self.unify(a, b, final);
}

/// reduce a series of slaps
///
/// all refs are dropped
pub fn stack(
    self: *Self,
    refs: []const Ref,
    dir: SlapDirection,
    opt: SlapOpt
) Allocator.Error!Ref {
    return switch (refs.len) {
        0 => self.stub(),
        1 => refs[0],
        else => stack: {
            var stacked = refs[0];
            for (refs[1..]) |ref| {
                stacked = try self.slap(stacked, ref, dir, opt);
            }

            break :stack stacked;
        }
    };
}

// displaying chunks ===========================================================

const WriteBuffer = struct {
    style: Style = .{},
    buf: [256]u8 = undefined,
    filled: usize = 0,

    fn putchar(
        self: *@This(),
        sty: Style,
        ch: u8,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        std.debug.assert(ch != '\n');

        if (!sty.eql(self.style)) {
            if (self.filled > 0) try self.flush(writer);
            self.style = sty;
        }

        self.buf[self.filled] = ch;
        self.filled += 1;

        if (self.filled == self.buf.len) {
            try self.flush(writer);
        }
    }

    fn newline(self: *@This(), writer: anytype) @TypeOf(writer).Error!void {
        try writer.print(
            "{}{s}{}\n",
            .{ self.style, self.buf[0..self.filled], &Style{} }
        );

        self.style = .{};
        self.filled = 0;
    }

    fn flush(self: *@This(), writer: anytype) @TypeOf(writer).Error!void {
        try writer.print("{}{s}", .{ self.style, self.buf[0..self.filled] });
        self.style = .{};
        self.filled = 0;
    }
};

/// write a chunk to a writer and drop the ref
pub fn write(
    self: *Self,
    ref: Ref,
    writer: anytype
) @TypeOf(writer).Error!void {
    const chunk = self.get(ref);
    defer self.drop(ref);

    var buf = WriteBuffer{};
    var y: usize = 0;
    while (y < chunk.size[1]) : (y += 1) {
        const start = y * chunk.size[0];
        const end = start + chunk.size[0];
        const line_styles = chunk.styles[start..end];
        const line = chunk.text[start..end];

        for (line) |ch, i| {
            try buf.putchar(line_styles[i], ch, writer);
        }

        try buf.newline(writer);
    }
}
