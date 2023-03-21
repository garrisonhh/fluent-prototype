//! Object is a high-level interface over bytes laid out in fluent's data model,
//! that exist inside a fluent image.

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const Env = @import("../env.zig");
const canon = @import("../canon.zig");
const Repr = canon.Repr;
const ReprId = canon.ReprId;
const Type = canon.Type;
const TypeId = canon.TypeId;
const Number = canon.Number;
const Builtin = canon.Builtin;
const Basic = canon.Basic;
const Ptr = canon.Ptr;
const interface = @import("object_interface.zig");

const Self = @This();

pub const Wrapper = interface.Wrapper;

ty: TypeId,
repr: ReprId,
ptr: Ptr,

pub const InitError = canon.ReprWelt.Error || canon.Image.AllocError;

pub fn from(env: *Env, ty: TypeId, ptr: Ptr) InitError!Self {
    const repr = try env.reprOf(ty);
    return Self{
        .ty = ty,
        .repr = repr,
        .ptr = ptr,
    };
}

/// initialize and allocate heap ptr
pub fn init(env: *Env, ty: TypeId) InitError!Self {
    const size = try env.sizeOf(ty);
    const ptr = try env.alloc(.heap, size);
    return Self.from(env, ty, ptr);
}

/// deallocate ptr
pub fn deinit(self: Self, env: *Env) void {
    const sz = env.sizeOf(self.ty) catch unreachable;
    env.free(self.ptr, sz);
}

pub const render = @import("render_object.zig").render;

/// allocates a new object and shallow copies this one onto it
pub fn clone(self: Self, env: *Env) InitError!Self {
    const size = env.sizeOf(self.ty) catch unreachable;
    const ptr = try env.img.alloc(.heap, size);
    env.img.copy(ptr, self.ptr, size);

    return Self{
        .ty = self.ty,
        .repr = self.repr,
        .ptr = ptr,
    };
}

/// 'from' helper
fn fromByte(env: *Env, ty: TypeId, n: u8) InitError!Self {
    const obj = try Self.init(env, ty);
    env.img.into(obj.ptr, *u8).* = n;

    return obj;
}

/// 'into' helper
fn intoByte(self: Self, env: *const Env) u8 {
    return env.img.into(self.ptr, *const u8).*;
}

/// 'from' helper
fn fromU64(env: *Env, ty: TypeId, n: u64) InitError!Self {
    const obj = try Self.init(env, ty);
    env.img.into(obj.ptr, *u64).* = n;

    return obj;
}

/// 'into' helper
fn intoU64(self: Self, env: *const Env) u64 {
    return env.img.into(self.ptr, *const u64).*;
}

// unit ========================================================================

pub fn fromUnit(env: *Env) InitError!Self {
    return Self.init(env, Basic.unit.get());
}

// intoUnit is completely useless

// builtin =====================================================================

pub fn fromBuiltin(env: *Env, val: Builtin) InitError!Self {
    const ty = try env.identify(.builtin);
    return Self.fromU64(env, ty, @enumToInt(val));
}

pub fn intoBuiltin(self: Self, env: *const Env) Builtin {
    return @intToEnum(Builtin, self.intoU64(env));
}

// bool ========================================================================

pub fn fromBool(env: *Env, @"bool": bool) InitError!Self {
    const ty = try env.identify(.bool);
    return Self.fromByte(env, ty, @boolToInt(@"bool"));
}

pub fn intoBool(self: Self, env: *const Env) bool {
    return switch (self.intoByte(env)) {
        0 => false,
        1 => true,
        else => unreachable,
    };
}

// type ========================================================================

pub fn fromType(env: *Env, val: TypeId) InitError!Self {
    const ty = try env.identify(.ty);
    return Self.fromU64(env, ty, val.index);
}

pub fn intoType(self: Self, env: *const Env) TypeId {
    return TypeId{ .index = self.intoU64(env) };
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

pub fn intoPtr(self: Self, env: *const Env, comptime P: type) P {
    verifyPtrType(P);
    return @intToPtr(P, self.intoU64(env));
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
    env: *const Env,
    comptime layout: Number.Layout,
    comptime bits: comptime_int,
    ptr: Ptr,
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
            env.img.into(ptr, *const T).*,
        ),
    };
}

pub fn intoNumber(self: Self, env: *const Env) Number {
    const meta = env.tw.get(self.ty).number;
    const bits = meta.bits orelse 64;

    // this switch may look confusing, but it's just manipulating the zig
    // compiler into understanding all of the possible conversion types at
    // comptime
    return switch (meta.layout) {
        inline .int, .uint => |ct_layout| switch (bits) {
            inline 8, 16, 32, 64 => |ct_bits| convertNumber(
                env,
                ct_layout,
                ct_bits,
                self.ptr,
            ),
            else => unreachable,
        },
        inline .float => |ct_layout| switch (bits) {
            inline 32, 64 => |ct_bits| convertNumber(
                env,
                ct_layout,
                ct_bits,
                self.ptr,
            ),
            else => unreachable,
        },
    };
}
