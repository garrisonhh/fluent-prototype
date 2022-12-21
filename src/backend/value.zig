//! value is the canonical low-level representation of a type-erased value. this
//! is what SSA IR constants use, and the bytecode VM follows its layout rules
//! in code generation.

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const util = @import("util");
const Env = @import("env.zig");
const types = @import("types.zig");
const TypeId = types.TypeId;
const Type = types.Type;
const TExpr = @import("texpr.zig");

const Self = @This();

// NOTE potential optimization here would be to store only the raw array ptr
ptr: []align(16) u8,

/// allocates and dupes data to aligned ptr
pub fn init(ally: Allocator, data: []const u8) Allocator.Error!Self {
    const self = try initEmpty(ally, data.len);
    std.mem.copy(u8, self.ptr, data);

    return self;
}

pub fn initEmpty(ally: Allocator, size: usize) Allocator.Error!Self {
    return Self{ .ptr = try ally.alignedAlloc(u8, 16, size) };
}

pub fn deinit(self: Self, ally: Allocator) void {
    ally.free(self.ptr);
}

pub fn asPtr(self: Self, comptime T: type) *align(16) T {
    if (builtin.mode == .Debug) {
        if (@sizeOf(T) != self.ptr.len) {
            std.debug.panic(
                "attempted to cast Value of size {} to type {} of size {}",
                .{self.ptr.len, T, @sizeOf(T)}
            );
        }
    }

    return @ptrCast(*align(16) T, self.ptr);
}

/// bitcast to the type desired
pub fn as(self: Self, comptime T: type) T {
    return self.asPtr(T).*;
}

fn toNumber(
    self: Self,
    bits: u8,
    layout: util.Number.Layout,
) TExpr.Data {
    // TODO could I make this more maintanable with inline else?
    const concrete: TExpr.Number.Concrete = switch (layout) {
        .int => .{
            .int = switch (bits) {
                64 => self.as(i64),
                32 => self.as(i32),
                16 => self.as(i16),
                8 => self.as(i8),
                else => unreachable
             }
         },
        .uint => .{
            .uint = switch (bits) {
                64 => self.as(u64),
                32 => self.as(u32),
                16 => self.as(u16),
                8 => self.as(u8),
                else => unreachable
             }
         },
        .float => .{
            .float = switch (bits) {
                64 => self.as(f64),
                32 => self.as(f32),
                else => unreachable
             }
        },
    };

    return .{ .number = .{ .bits = bits, .data = concrete } };
}

/// take a bunch of bits, along with a type, and magically turn them back into
/// a TExpr.
///
/// this is probably the most important function in the entire codebase, as it
/// is the thing that enables the most interesting features in fluent
pub fn resurrect(
    self: Self,
    ally: Allocator,
    env: Env,
    tid: TypeId
) Allocator.Error!TExpr {
    const ty = env.typeGet(tid);
    const data: TExpr.Data = switch (ty.*) {
        .@"bool" => .{ .@"bool" = self.as(u8) > 0 },
        .number => |num| self.toNumber(num.bits orelse 64, num.layout),
        else => {
            const text = tid.writeAlloc(ally, env.typewelt.*) catch unreachable;
            defer ally.free(text);

            std.debug.panic("TODO revive type {s}", .{text});
        }
    };

    return TExpr.init(null, tid, data);
}