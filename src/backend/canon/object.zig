//! Object is a high-level interface over bytes laid out in fluent's data model.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Env = @import("../env.zig");
const canon = @import("../canon.zig");
const ReprId = canon.ReprId;
const Repr = canon.Repr;
const TypeId = canon.TypeId;
const Type = canon.Type;
const Value = canon.Value;
const Number = canon.Number;
const Builtin = canon.Builtin;

const Self = @This();

ty: TypeId,
repr: ReprId,
val: Value,

pub const InitError = Repr.Error;

pub fn init(env: *Env, ty: TypeId) InitError!Self {
    const repr = try env.reprOf(ty);
    const size = try env.rw.sizeOf(repr);

    return Self{
        .ty = ty,
        .repr = repr,
        .val = Value.of(try env.ally.alloc(u8, size)),
    };
}

pub fn deinit(self: Self, env: *Env) void {
    self.val.deinit(env.ally);
}

pub const render = @import("render_object.zig").render;

pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
    return Self{
        .ty = self.ty,
        .repr = self.repr,
        .val = .{ .buf = try ally.dupe(u8, self.val.buf) },
    };
}

/// a lot of types use u64 as repr
fn fromU64(env: *Env, ty: TypeId, n: u64) InitError!Self {
    const obj = try Self.init(env, ty);
    std.debug.assert(obj.val.buf.len == @sizeOf(u64));
    std.mem.set(u8, obj.val.buf, 0);
    std.mem.copy(u8, obj.val.buf, canon.from(&n));

    return obj;
}

/// a lot of types use u64 as repr
fn intoU64(self: Self) u64 {
    return self.val.as(u64);
}

// unit ========================================================================

pub fn fromUnit(env: *Env) InitError!Self {
    const ty = try env.identify(.unit);
    const obj = try Self.init(env, ty);
    std.debug.assert(obj.val.buf.len == 0);

    return obj;
}

// builtin =====================================================================

pub fn fromBuiltin(env: *Env, val: Builtin) InitError!Self {
    const ty = try env.identify(.builtin);
    return Self.fromU64(env, ty, @enumToInt(val));
}

pub fn intoBuiltin(self: Self) Builtin {
    return @intToEnum(Builtin, self.intoU64());
}

// bool ========================================================================

pub fn fromBool(env: *Env, @"bool": bool) InitError!Self {
    const ty = try env.identify(.bool);
    const obj = try Self.init(env, ty);
    std.debug.assert(obj.val.buf.len == 1);
    obj.val.buf[0] = @boolToInt(@"bool");

    return obj;
}

pub fn intoBool(self: Self) bool {
    return 0 != self.val.buf[0];
}

// type ========================================================================

pub fn fromType(env: *Env, val: TypeId) InitError!Self {
    const ty = try env.identify(.ty);
    return Self.fromU64(env, ty, val.index);
}

pub fn intoType(self: Self) TypeId {
    return TypeId{ .index = self.intoU64() };
}

// number ======================================================================

pub fn fromNumber(env: *Env, num: Number) InitError!Self {
    const num_val = try num.asValue(env.ally);
    defer num_val.deinit(env.ally);

    const ty = try env.identify(Type{
        .number = .{ .bits = num.bits, .layout = num.data },
    });
    const obj = Self.init(env, ty);
    std.mem.copy(u8, obj.val.buf, num_val.buf);

    return obj;
}

pub fn intoNumber(self: Self, env: Env) Number {
    const meta = env.tw.get(self.ty).number;
    const bits = meta.bits orelse 64;
    std.debug.assert(self.bits == self.val.buf.len * 8);

    return switch (bits) {
        inline 8, 16, 32, 64 => |ct_bits| switch (meta.layout) {
            inline .int, .uint, .float => |tag| convert: {
                const T = switch (tag) {
                    .int => @Type(.{
                        .Int = .{ .signedness = .signed, .bits = ct_bits },
                    }),
                    .uint => @Type(.{
                        .Int = .{ .signedness = .unsigned, .bits = ct_bits },
                    }),
                    .float => @Type(.{ .Float = .{ .bits = ct_bits } }),
                };

                const n = self.val.as(T);
                break :convert Number{
                    .bits = meta.bits,
                    .data = @unionInit(Number.Concrete, @tagName(tag), n),
                };
            },
        },
        else => unreachable,
    };
}
