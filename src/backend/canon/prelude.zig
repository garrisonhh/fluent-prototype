//! defining the initial state of a program's env, which includes all of the
//! basic fluent data types I want available immediately

const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");
const Symbol = com.Symbol;
const TypeWelt = @import("typewelt.zig");
const TypeId = TypeWelt.TypeId;
const Type = @import("type.zig").Type;
const Repr = @import("repr.zig").Repr;
const Env = @import("../env.zig");
const Object = @import("object.zig");
const Builtin = @import("../canon.zig").Builtin;

pub const Error = Object.InitError || Env.DefError;

fn def(env: *Env, str: []const u8, value: Object) Error!void {
    _ = try env.def(Env.ROOT, Symbol.init(str), value);
}

fn defBuiltin(env: *Env, str: []const u8, b: Builtin) Error!void {
    try def(env, str, try Object.fromBuiltin(env, b));
}

fn numericType(
    env: *Env,
    layout: com.Number.Layout,
    bits: ?u8,
) Allocator.Error!TypeId {
    return try env.identify(Type{
        .number = .{ .bits = bits, .layout = layout },
    });
}

fn sliceType(env: *Env, of: TypeId) Error!TypeId {
    return try env.identify(Type{
        .ptr = .{ .kind = .slice, .to = of },
    });
}

fn fnType(
    env: *Env,
    takes: []const TypeId,
    returns: TypeId,
) Error!TypeId {
    var buf: [256]TypeId = undefined;
    std.mem.copy(TypeId, &buf, takes);

    return try env.identify(Type{
        .func = Type.Func{
            .takes = buf[0..takes.len],
            .returns = returns,
        },
    });
}

const FieldTuple = struct {
    @"0": []const u8,
    @"1": TypeId,
};

fn collType(
    env: *Env,
    comptime coll_tag: Type.Tag,
    field_tuples: []const FieldTuple,
) Error!TypeId {
    const ally = env.ally;

    const fields = try ally.alloc(Type.Field, field_tuples.len);
    for (field_tuples) |tup, i| {
        fields[i] = Type.Field{
            .name = Symbol.init(try ally.dupe(u8, tup.@"0")),
            .of = tup.@"1",
        };
    }

    return try env.identify(@unionInit(Type, @tagName(coll_tag), fields));
}

/// the essential fluent types that are defined in the prelude
pub const Basic = enum {
    const Self = @This();

    // atomic
    any,
    type,
    unit,
    bool,
    builtin,

    // numbers
    compiler_int,
    compiler_float,
    u8,
    u16,
    u32,
    u64,
    i8,
    i16,
    i32,
    i64,
    f32,
    f64,

    // collections
    expr,

    var types: [std.enums.values(Self).len]TypeId = undefined;

    /// after prelude is initialized, this allows constant access for basic
    /// types
    pub fn get(self: Self) TypeId {
        return types[@enumToInt(self)];
    }

    /// if a name isn't provided, this type won't be defined
    fn getName(self: Self) ?[]const u8 {
        return switch (self) {
            // title case
            inline .any,
            .expr,
            => |tag| title: {
                const name = comptime @tagName(tag);
                break :title std.fmt.comptimePrint(
                    "{c}{s}",
                    .{ name[0], name[1..] },
                );
            },

            // lower case
            .type,
            .unit,
            .bool,
            .builtin,
            .compiler_int,
            .compiler_float,
            .u8,
            .u16,
            .u32,
            .u64,
            .i8,
            .i16,
            .i32,
            .i64,
            .f32,
            .f64,
            => @tagName(self),
        };
    }

    /// used to define types exhaustively on prelude generation
    fn identify(self: Self, env: *Env) Error!TypeId {
        return switch (self) {
            .type => try env.identify(.ty),
            .compiler_int => try numericType(env, .int, null),
            .compiler_float => try numericType(env, .float, null),

            inline .any,
            .unit,
            .bool,
            .builtin,
            => |tag| try env.identify(@unionInit(Type, @tagName(tag), {})),

            inline .u8,
            .u16,
            .u32,
            .u64,
            .i8,
            .i16,
            .i32,
            .i64,
            .f32,
            .f64,
            => |tag| num: {
                const name = @tagName(tag);
                const layout: com.Number.Layout = switch (name[0]) {
                    'i' => .int,
                    'u' => .uint,
                    'f' => .float,
                    else => unreachable,
                };
                const bits = std.fmt.parseInt(u8, name[1..], 10) catch {
                    unreachable;
                };

                break :num try numericType(env, layout, bits);
            },

            .expr => ex: {
                const data = try collType(env, .variant, &.{
                    .{ "unit", Self.unit.get() },
                    .{ "bool", Self.bool.get() },
                    .{ "type", Self.type.get() },
                    .{ "int", Self.compiler_int.get() },
                    .{ "float", Self.compiler_float.get() },
                });

                break :ex try collType(env, .@"struct", &.{
                    .{ "type", Self.type.get() },
                    .{ "data", data },
                });
            },
        };
    }

    fn defineAll(env: *Env) Error!void {
        for (std.enums.values(Self)) |val| {
            const ty = try val.identify(env);
            types[@enumToInt(val)] = ty;

            if (val.getName()) |name| {
                _ = try env.defType(Env.ROOT, Symbol.init(name), ty);
            }
        }
    }
};

pub fn initPrelude(env: *Env) !void {
    try Basic.defineAll(env);

    // define builtin consts
    try def(env, "true", try Object.fromBool(env, true));
    try def(env, "false", try Object.fromBool(env, false));

    // define builtin forms
    try defBuiltin(env, "ns", .ns);
    try defBuiltin(env, "def", .def);
    try defBuiltin(env, "as", .cast);
    try defBuiltin(env, "&", .addr_of);
    try defBuiltin(env, ".", .access);
    try defBuiltin(env, "array", .array);
    try defBuiltin(env, "tuple", .tuple);
    try defBuiltin(env, "lambda", .lambda);
    try defBuiltin(env, "if", .@"if");

    // define builtin functions
    // const bin_cond = try fnType(env, &.{ @"bool", @"bool" }, @"bool");
    // const un_cond = try fnType(env, &.{@"bool"}, @"bool");
    // const bin_i64 = try fnType(env, &.{ @"i64", @"i64" }, @"i64");
    // const bin_u64 = try fnType(env, &.{ @"u64", @"u64" }, @"u64");
    // const un_ty = try fnType(env, &.{@"type"}, @"type");
    // const bin_i64_cond = try fnType(env, &.{ @"i64", @"i64" }, @"bool");

    // const type_slice_ty = try env.identify(Type.initPtr(.slice, @"type"));
    // const fn_ty = try fnType(env, &.{ type_slice_ty, @"type" }, @"type");
    // const tuple_ty = try fnType(env, &.{type_slice_ty}, @"type");

    // try defBuiltin(env, "=", bin_i64_cond, .eq);
    // try defBuiltin(env, "+", bin_i64, .add);
    // try defBuiltin(env, "-", bin_i64, .sub);
    // try defBuiltin(env, "*", bin_i64, .mul);
    // try defBuiltin(env, "/", bin_i64, .div);
    // try defBuiltin(env, "%", bin_i64, .mod);
    // try defBuiltin(env, "<<", bin_u64, .shl);
    // try defBuiltin(env, ">>", bin_u64, .shr);

    // try defBuiltin(env, "and", bin_cond, .@"and");
    // try defBuiltin(env, "or", bin_cond, .@"or");
    // try defBuiltin(env, "not", un_cond, .not);

    // try defBuiltin(env, "Fn", fn_ty, .fn_ty);
    // try defBuiltin(env, "Slice", un_ty, .slice_ty);
    // try defBuiltin(env, "Tuple", tuple_ty, .tuple_ty);
}
