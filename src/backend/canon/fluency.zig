//! switching between the differently structured but lossless representations of
//! Fluent data

const std = @import("std");
const Allocator = std.mem.Allocator;
const util = @import("util");
const Loc = util.Loc;
const TExpr = @import("../texpr.zig");
const Env = @import("../env.zig");
const types = @import("../types.zig");
const TypeId = types.TypeId;
const canon = @import("../canon.zig");
const Number = canon.Number;
const Value = canon.Value;
const BcRef = @import("../bytecode/bytecode.zig").InstRef;

// crucifixion =================================================================

pub const CrucifyError = Allocator.Error;

/// fills dst with zeroes and then copies canonical data
fn writeCanon(dst: []u8, n: u64) void {
    std.mem.set(u8, dst, 0);
    std.mem.copy(u8, dst, canon.from(&n));
}

fn rawCrucify(env: Env, buf: []u8, texpr: TExpr) CrucifyError!void {
    switch (texpr.data) {
        .unit => {},
        .@"bool" => |b| buf[0] = if (b) 1 else 0,
        .ty => |ty| writeCanon(buf, ty.index),
        .builtin => |b| writeCanon(buf, @enumToInt(b)),
        .number => |num| {
            var fba_buf: [16]u8 = undefined;
            var fba = std.heap.FixedBufferAllocator.init(&fba_buf);

            const val = num.asValue(fba.allocator()) catch unreachable;
            std.debug.assert(buf.len == val.buf.len);
            std.mem.copy(u8, buf, val.buf);
        },
        .array => |children| {
            const seg = @divExact(buf.len, children.len);
            for (children) |child, i| {
                const index = i * seg;
                try rawCrucify(env, buf[index..index + seg], child);
            }
        },
        .func_ref => |ref| {
            // translate to bytecode ref for the vm
            const bc_ref = env.compiled.get(ref) orelse {
                std.debug.panic(
                    "failed to get bcref for `{}` in crucify\n",
                    .{env.getFuncConst(ref).name}
                );
            };
            writeCanon(buf, bc_ref.index);
        },
        else => |tag| std.debug.panic("TODO crucify {}\n", .{tag})
    }
}

/// takes a texpr and converts it to bits allocated on the Env ally
pub fn crucify(env: Env, texpr: TExpr) CrucifyError!Value {
    std.debug.assert(texpr.known_const);

    const size = env.sizeOf(texpr.ty);
    const value = Value.of(try env.ally.alloc(u8, size));

    try rawCrucify(env, value.buf, texpr);

    return value;
}

// resurrection ================================================================

pub const ResError = Allocator.Error || error { Unresurrectable };

fn valueToNumber(value: Value, bits: u8, layout: Number.Layout) Number {
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
            const index = canon.to(value.buf);
            break :ty TExpr.Data{ .ty = TypeId{ .index = index } };
        },
        .builtin => b: {
            const b = @intToEnum(canon.Builtin, canon.to(value.buf));
            break :b TExpr.Data{ .builtin = b };
        },
        .array => |arr| arr: {
            const children = try ally.alloc(TExpr, arr.size);

            // resurrect elements
            const el_size = env.sizeOf(arr.of);
            const buf = try ally.alloc(u8, el_size);
            defer ally.free(buf);
            const el_val = Value.of(buf);

            var i: usize = 0;
            while (i < arr.size) : (i += 1) {
                const src = value.buf[i * el_size..(i + 1) * el_size];
                std.mem.copy(u8, el_val.buf, src);

                children[i] = try resurrect(env, el_val, mem, loc, arr.of);
            }

            break :arr TExpr.Data{ .array = children };
        },
        .ptr => |ptr| switch (ptr.kind) {
            .many => return error.Unresurrectable,
            .single => ptr: {
                // resurrect data being pointed to
                const size = env.sizeOf(ptr.to);
                const index = canon.to(value.buf);

                const val = try Value.init(ally, mem[index..index + size]);
                defer val.deinit(ally);

                // resurrect self from the child data
                const child = try resurrect(env, val, mem, loc, ptr.to);
                break :ptr TExpr.Data{ .ptr = try util.placeOn(ally, child) };
            },
            .slice => slice: {
                // get struct data
                const struct_index = canon.to(value.buf);
                const struct_data = mem[struct_index..struct_index + 16];

                // get ptr + len
                const index = canon.to(struct_data[0..8]);
                const len = canon.to(struct_data[8..16]);

                // resurrect each subvalue
                const el_size = env.sizeOf(ptr.to);
                const slice = try ally.alloc(TExpr, len);

                const el = Value.of(try ally.alloc(u8, el_size));
                defer el.deinit(ally);

                var i: usize = 0;
                while (i < len) : (i += 1) {
                    const start = index + i * el_size;
                    std.mem.copy(u8, el.buf, mem[start..start + el_size]);

                    slice[i] = try resurrect(env, el, mem, loc,ptr.to);
                }

                break :slice TExpr.Data{ .slice = slice };
            },
        },
        .func => func: {
            // functions are lowered as BcRef indices
            const bc = BcRef.of(@intCast(u32, canon.to(value.buf)));
            break :func TExpr.Data{ .func_ref = env.lowered.get(bc).? };
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