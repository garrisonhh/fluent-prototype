//! Object is a high-level interface over bytes laid out in fluent's data model.

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const Env = @import("../env.zig");
const canon = @import("../canon.zig");
const Repr = canon.Repr;
const ReprId = canon.ReprId;
const Type = canon.Type;
const TypeId = canon.TypeId;
const Value = canon.Value;
const Number = canon.Number;
const Builtin = canon.Builtin;
const Basic = canon.Basic;
const interface = @import("object_interface.zig");

const Self = @This();

pub const Wrapper = interface.Wrapper;

ty: TypeId,
repr: ReprId,
val: Value,

pub const InitError = canon.ReprWelt.Error;

pub fn from(env: *Env, ty: TypeId, val: Value) InitError!Self {
    const repr = try env.reprOf(ty);
    return Self{
        .ty = ty,
        .repr = repr,
        .val = val,
    };
}

pub fn init(env: *Env, ty: TypeId) InitError!Self {
    const size = try env.sizeOf(ty);
    const val = try Value.alloc(env.ally, size);
    return Self.from(env, ty, val);
}

pub fn deinit(self: Self, env: *Env) void {
    self.val.deinit(env.ally);
}

pub const render = @import("render_object.zig").render;

pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
    return Self{
        .ty = self.ty,
        .repr = self.repr,
        .val = try Value.init(ally, self.val.buf),
    };
}

/// a lot of types use u64 as repr
fn fromU64(env: *Env, ty: TypeId, n: u64) InitError!Self {
    const obj = try Self.init(env, ty);
    @ptrCast(*u64, obj.val.buf).* = n;
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
    const obj = try Self.init(env, Basic.unit.get());
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

// ptr =========================================================================

fn verifyPtrType(comptime P: type) void {
    comptime {
        if (builtin.mode == .Debug) {
            const info = @typeInfo(P);
            std.debug.assert(info == .Pointer);
            std.debug.assert(info.Pointer.size != .Slice);
        }
    }
}

pub fn fromPtr(env: *Env, comptime P: type, val: P) InitError!Self {
    verifyPtrType(P);
    return Self.fromU64(env, try env.identifyZigType(P), @ptrToInt(val));
}

pub fn intoPtr(self: Self, comptime P: type) P {
    verifyPtrType(P);
    return @intToPtr(P, self.intoU64());
}

// number ======================================================================

pub fn fromNumber(env: *Env, num: Number) InitError!Self {
    const num_val = try num.asValue(env.ally);
    defer num_val.deinit(env.ally);

    const ty = try env.identify(Type{
        .number = .{ .bits = num.bits, .layout = num.data },
    });
    const obj = try Self.init(env, ty);
    std.mem.copy(u8, obj.val.buf, num_val.buf);

    return obj;
}

fn convertNumber(
    comptime layout: Number.Layout,
    comptime bits: comptime_int,
    value: Value,
) Number {
    // calculate zig type with comptime info
    const T = switch (layout) {
        .int => @Type(.{ .Int = .{ .signedness = .signed, .bits = bits } }),
        .uint => @Type(.{ .Int = .{ .signedness = .unsigned, .bits = bits } }),
        .float => @Type(.{ .Float = .{ .bits = bits } }),
    };

    return Number{
        .bits = bits,
        .data = @unionInit(
            Number.Concrete,
            @tagName(layout),
            value.as(T),
        ),
    };
}

pub fn intoNumber(self: Self, env: Env) Number {
    const meta = env.tw.get(self.ty).number;
    const bits = meta.bits orelse 64;
    std.debug.assert(bits == self.val.buf.len * 8);

    // this switch may look confusing, but it's just manipulating the zig
    // compiler into understanding all of the possible conversion types at
    // comptime
    return switch (meta.layout) {
        inline .int, .uint => |ct_layout| switch (bits) {
            inline 8, 16, 32, 64 => |ct_bits| convertNumber(
                ct_layout,
                ct_bits,
                self.val,
            ),
            else => unreachable,
        },
        inline .float => |ct_layout| switch (bits) {
            inline 32, 64 => |ct_bits| convertNumber(
                ct_layout,
                ct_bits,
                self.val,
            ),
            else => unreachable,
        },
    };
}
