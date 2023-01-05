//! value is the canonical low-level representation of a type-erased value. this
//! is what SSA IR constants use, and the bytecode VM follows its layout rules
//! in code generation.

// TODO move this to canon?

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const util = @import("util");
const Env = @import("env.zig");
const types = @import("types.zig");
const TypeId = types.TypeId;
const Type = types.Type;
const TExpr = @import("texpr.zig");
const FuncRef = @import("ssa.zig").FuncRef;
const canon = @import("canon.zig");

const Self = @This();

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

pub const ResError = Allocator.Error || error { Unresurrectable };

/// take a bunch of bits, along with a type, and magically turn them back into
/// a TExpr.
///
/// this is probably the most important function in the entire codebase, as it
/// is the thing that enables the most interesting features in fluent
pub fn resurrect(
    self: Self,
    env: Env,
    mem: []const u8,
    tid: TypeId
) ResError!TExpr {
    const ally = env.ally;
    const ty = env.tw.get(tid);
    const data: TExpr.Data = switch (ty.*) {
        .@"bool" => .{ .@"bool" = self.as(u8) > 0 },
        .number => |num| self.toNumber(num.bits orelse 64, num.layout),
        .ty => ty: {
            const index = canon.toCanonical(self.ptr);
            break :ty TExpr.Data{ .ty = TypeId{ .index = index } };
        },
        .array => |arr| arr: {
            const children = try ally.alloc(TExpr, arr.size);

            // resurrect elements
            const el_size = env.sizeOf(arr.of);
            const buf = try ally.alignedAlloc(u8, 16, el_size);
            defer ally.free(buf);
            const el_val = Self{ .ptr = buf };

            var i: usize = 0;
            while (i < arr.size) : (i += 1) {
                const src = self.ptr[i * el_size..(i + 1) * el_size];
                std.mem.copy(u8, el_val.ptr, src);

                children[i] = try el_val.resurrect(env, mem, arr.of);
            }

            break :arr TExpr.Data{ .array = children };
        },
        .ptr => |ptr| switch (ptr.kind) {
            .many => return error.Unresurrectable,
            .single => ptr: {
                // resurrect data being pointed to
                const size = env.sizeOf(ptr.to);
                const index = canon.toCanonical(self.ptr);

                const buf = try ally.alignedAlloc(u8, 16, size);
                defer ally.free(buf);

                std.mem.copy(u8, buf, mem[index..index + size]);

                // resurrect self from the child data
                const val = Self{ .ptr = buf };
                const child = try val.resurrect(env, mem, ptr.to);
                break :ptr TExpr.Data{ .ptr = try util.placeOn(ally, child) };
            },
            .slice => slice: {
                // get struct data
                const struct_index = canon.toCanonical(self.ptr);
                const struct_data = mem[struct_index..struct_index + 16];

                // get ptr + len
                const index = canon.toCanonical(struct_data[0..8]);
                const len = canon.toCanonical(struct_data[8..16]);

                // resurrect each subvalue
                const el_size = env.sizeOf(ptr.to);
                const slice = try ally.alloc(TExpr, len);

                const buf = try ally.alignedAlloc(u8, 16, el_size);
                defer ally.free(buf);
                const el = Self{ .ptr = buf };

                var i: usize = 0;
                while (i < len) : (i += 1) {
                    const start = index + i * el_size;
                    std.mem.copy(u8, buf, mem[start..start + el_size]);

                    slice[i] = try el.resurrect(env, mem, ptr.to);
                }

                break :slice TExpr.Data{ .slice = slice };
            },
        },
        // functions are lowered as FuncRef indices
        .func => .{ .func_ref = FuncRef.of(canon.toCanonical(self.ptr)) },
        else => {
            const text = tid.writeAlloc(ally, env.tw) catch {
                @panic("writeAlloc failed.");
            };
            defer ally.free(text);

            std.debug.panic("TODO resurrect type {s}", .{text});
        }
    };

    return TExpr.init(null, tid, data);
}