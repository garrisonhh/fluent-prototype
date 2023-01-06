//! switching between the differently structured but lossless representations of
//! Fluent data

const std = @import("std");
const Allocator = std.mem.Allocator;
const util = @import("util");
const TExpr = @import("../texpr.zig");
const Value = @import("../value.zig");
const Env = @import("../env.zig");
const Loc = @import("../../context.zig").Loc;
const types = @import("../types.zig");
const Type = types.Type;
const TypeId = types.TypeId;
const canon = @import("../canon.zig");
const Number = canon.Number;
const FuncRef = @import("../ssa.zig").FuncRef;

pub fn valueToNumber(value: Value, bits: u8, layout: Number.Layout) Number {
    // TODO could I make this more maintanable with inline else?
    const concrete: Number.Concrete = switch (layout) {
        .int => .{
            .int = switch (bits) {
                64 => value.as(i64),
                32 => value.as(i32),
                16 => value.as(i16),
                8 => value.as(i8),
                else => unreachable
             }
         },
        .uint => .{
            .uint = switch (bits) {
                64 => value.as(u64),
                32 => value.as(u32),
                16 => value.as(u16),
                8 => value.as(u8),
                else => unreachable
             }
         },
        .float => .{
            .float = switch (bits) {
                64 => value.as(f64),
                32 => value.as(f32),
                else => unreachable
             }
        },
    };

    return Number{
        .bits = bits,
        .data = concrete,
    };
}

pub const ResError = Allocator.Error || error { Unresurrectable };

/// take a bunch of bits, along with a type, and magically turn them back into
/// a TExpr.
///
/// this is probably the most important function in the entire codebase, as it
/// is the thing that enables the most interesting features in fluent
pub fn resurrect(
    env: Env,
    value: Value,
    mem: []const u8,
    loc: ?Loc,
    tid: TypeId
) ResError!TExpr {
    const ally = env.ally;
    const ty = env.tw.get(tid);
    const data: TExpr.Data = switch (ty.*) {
        .@"bool" => .{ .@"bool" = value.as(u8) > 0 },
        .number => |num| num: {
            const bits = num.bits orelse 64;
            break :num .{
                .number = canon.valueToNumber(value, bits, num.layout)
            };
        },
        .ty => ty: {
            const index = canon.toCanonical(value.ptr);
            break :ty TExpr.Data{ .ty = TypeId{ .index = index } };
        },
        .array => |arr| arr: {
            const children = try ally.alloc(TExpr, arr.size);

            // resurrect elements
            const el_size = env.sizeOf(arr.of);
            const buf = try ally.alignedAlloc(u8, 16, el_size);
            defer ally.free(buf);
            const el_val = Value{ .ptr = buf };

            var i: usize = 0;
            while (i < arr.size) : (i += 1) {
                const src = value.ptr[i * el_size..(i + 1) * el_size];
                std.mem.copy(u8, el_val.ptr, src);

                children[i] = try resurrect(env, el_val, mem, loc, arr.of);
            }

            break :arr TExpr.Data{ .array = children };
        },
        .ptr => |ptr| switch (ptr.kind) {
            .many => return error.Unresurrectable,
            .single => ptr: {
                // resurrect data being pointed to
                const size = env.sizeOf(ptr.to);
                const index = canon.toCanonical(value.ptr);

                const buf = try ally.alignedAlloc(u8, 16, size);
                defer ally.free(buf);

                std.mem.copy(u8, buf, mem[index..index + size]);

                // resurrect self from the child data
                const val = Value{ .ptr = buf };
                const child = try resurrect(env, val, mem, loc, ptr.to);
                break :ptr TExpr.Data{ .ptr = try util.placeOn(ally, child) };
            },
            .slice => slice: {
                // get struct data
                const struct_index = canon.toCanonical(value.ptr);
                const struct_data = mem[struct_index..struct_index + 16];

                // get ptr + len
                const index = canon.toCanonical(struct_data[0..8]);
                const len = canon.toCanonical(struct_data[8..16]);

                // resurrect each subvalue
                const el_size = env.sizeOf(ptr.to);
                const slice = try ally.alloc(TExpr, len);

                const buf = try ally.alignedAlloc(u8, 16, el_size);
                defer ally.free(buf);
                const el = Value{ .ptr = buf };

                var i: usize = 0;
                while (i < len) : (i += 1) {
                    const start = index + i * el_size;
                    std.mem.copy(u8, buf, mem[start..start + el_size]);

                    slice[i] = try resurrect(env, el, mem, loc,ptr.to);
                }

                break :slice TExpr.Data{ .slice = slice };
            },
        },
        // functions are lowered as FuncRef indices
        .func => TExpr.Data{
            .func_ref = FuncRef.of(canon.toCanonical(value.ptr))
        },
        else => {
            const text = tid.writeAlloc(ally, env.tw) catch {
                @panic("writeAlloc failed.");
            };
            defer ally.free(text);

            std.debug.panic("TODO resurrect type {s}", .{text});
        }
    };

    return TExpr.init(loc, true, tid, data);
}