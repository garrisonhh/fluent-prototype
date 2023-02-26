//! value is the canonical low-level representation of a type-erased value. this
//! is what SSA IR constants use, and the bytecode VM follows its layout rules
//! in code generation

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");

const Self = @This();

pub const Alignment = 8;
pub const Slice = []align(Alignment) u8;

buf: Slice,

pub fn of(buf: Slice) Self {
    return Self{ .buf = buf };
}

pub fn alloc(
    ally: Allocator,
    nbytes: usize,
) Allocator.Error!Self {
    return Self{ .buf = try ally.alignedAlloc(u8, Alignment, nbytes) };
}

/// allocates and dupes data to aligned ptr
pub fn init(ally: Allocator, buf: []const u8) Allocator.Error!Self {
    const self = try Self.alloc(ally, buf.len);
    std.mem.copy(u8, self.buf, buf);

    return self;
}

pub fn deinit(self: Self, ally: Allocator) void {
    ally.free(self.buf);
}

/// bitcast to the type desired
/// tolerates different widths, assuming no (non-zero) data is lost
pub fn as(self: Self, comptime T: type) T {
    // bitcast
    var t: T = undefined;
    const view = std.mem.asBytes(&t);
    const len = @min(view.len, self.buf.len);

    if (builtin.mode == .Debug) {
        for (self.buf[len..]) |byte| {
            if (byte != 0) {
                std.debug.panic(
                    "attempted bad Value bitcast:\nfrom {d}\n to {}\n",
                    .{ self.buf, T },
                );
            }
        }
    }

    std.mem.set(u8, view, 0);
    std.mem.copy(u8, view, self.buf[0..len]);

    return t;
}
