//! defining the initial state of a program's env.

const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");
const Symbol = com.Symbol;
const TypeWelt = @import("typewelt.zig");
const TypeId = TypeWelt.TypeId;
const Type = @import("type.zig").Type;
const Env = @import("../env.zig");
const TExpr = @import("../texpr.zig");
const Builtin = @import("../canon.zig").Builtin;

fn filterDefError(e: Env.DefError, comptime str: []const u8) Allocator.Error {
    return switch (e) {
        error.NameRedef,
        error.NameNoRedef,
        error.RenamedType,
        error.NameTooLong,
        => std.debug.panic(
            "uh oh, got {} in prelude while trying to define {s}.",
            .{ e, str },
        ),
        error.OutOfMemory => return @errSetCast(Allocator.Error, e),
    };
}

fn def(
    env: *Env,
    comptime str: []const u8,
    value: TExpr,
) Allocator.Error!void {
    const cloned = try value.clone(env.ally);
    _ = env.def(Env.ROOT, comptime Symbol.init(str), cloned) catch |e| {
        return filterDefError(e, str);
    };
}

fn defType(
    env: *Env,
    comptime str: []const u8,
    ty: Type,
) Allocator.Error!TypeId {
    const id = try env.identify(ty);
    _ = env.defType(Env.ROOT, comptime Symbol.init(str), id) catch |e| {
        return filterDefError(e, str);
    };

    return id;
}

fn defNumeric(
    env: *Env,
    comptime str: []const u8,
    layout: com.Number.Layout,
    bits: ?u8,
) Allocator.Error!TypeId {
    return try defType(env, str, Type{
        .number = .{ .layout = layout, .bits = bits },
    });
}

fn defBuiltin(
    env: *Env,
    comptime str: []const u8,
    ty: TypeId,
    b: Builtin,
) Allocator.Error!void {
    try def(env, str, TExpr.initBuiltin(null, ty, b));
}

fn fnType(
    env: *Env,
    takes: []const TypeId,
    returns: TypeId,
) Allocator.Error!TypeId {
    var buf: [256]TypeId = undefined;
    std.mem.copy(TypeId, &buf, takes);

    return try env.identify(Type{
        .func = Type.Func{
            .takes = buf[0..takes.len],
            .returns = returns,
        },
    });
}

pub fn generatePrelude(ally: Allocator) Allocator.Error!Env {
    var arena = std.heap.ArenaAllocator.init(ally);
    defer arena.deinit();
    const tmp = arena.allocator();

    var env = try Env.init(ally);

    // basic typedefs
    const any = try defType(&env, "Any", Type{ .any = {} });
    const @"type" = try defType(&env, "type", Type{ .ty = {} });
    const unit = try defType(&env, "unit", Type{ .unit = {} });
    const @"bool" = try defType(&env, "bool", Type{ .@"bool" = {} });
    const namespace = try defType(&env, "namespace", Type{ .namespace = {} });
    const flbuiltin = try defType(&env, "builtin", Type{ .builtin = {} });

    _ = any;
    _ = unit;
    _ = namespace;

    // define number types
    const compiler_int = try defNumeric(&env, "compiler-int", .int, null);
    const compiler_float = try defNumeric(&env, "compiler-float", .float, null);

    const @"i8" = try defNumeric(&env, "i8", .int, 8);
    const @"i16" = try defNumeric(&env, "i16", .int, 16);
    const @"i32" = try defNumeric(&env, "i32", .int, 32);
    const @"i64" = try defNumeric(&env, "i64", .int, 64);
    const @"u8" = try defNumeric(&env, "u8", .uint, 8);
    const @"u16" = try defNumeric(&env, "u16", .uint, 16);
    const @"u32" = try defNumeric(&env, "u32", .uint, 32);
    const @"u64" = try defNumeric(&env, "u64", .uint, 64);
    const @"f32" = try defNumeric(&env, "f32", .float, 32);
    const @"f64" = try defNumeric(&env, "f64", .float, 64);

    const int_ids = &[_]TypeId{ @"i8", @"i16", @"i32", @"i64", compiler_int };
    const uint_ids = &[_]TypeId{ @"u8", @"u16", @"u32", @"u64" };
    const float_ids = &[_]TypeId{ @"f32", @"f64", compiler_float };
    const int_ty = try Type.initSet(tmp, int_ids);
    const uint_ty = try Type.initSet(tmp, uint_ids);
    const float_ty = try Type.initSet(tmp, float_ids);

    const int = try defType(&env, "Int", int_ty);
    const uint = try defType(&env, "UInt", uint_ty);
    const float = try defType(&env, "Float", float_ty);

    var number_ids = std.ArrayList(TypeId).init(tmp);
    for ([_][]TypeId{ int_ids, uint_ids, float_ids }) |id_list| {
        try number_ids.appendSlice(id_list);
    }

    const number_ty = try Type.initSet(tmp, number_ids.items);
    const number = try defType(&env, "Number", number_ty);

    _ = int;
    _ = uint;
    _ = float;
    _ = number;

    // the bool consts
    const true_expr = TExpr.init(null, true, @"bool", .{ .@"bool" = true });
    const false_expr = TExpr.init(null, true, @"bool", .{ .@"bool" = false });
    try def(&env, "true", true_expr);
    try def(&env, "false", false_expr);

    // define builtins
    const bin_cond = try fnType(&env, &.{ @"bool", @"bool" }, @"bool");
    const un_cond = try fnType(&env, &.{@"bool"}, @"bool");
    const bin_i64 = try fnType(&env, &.{ @"i64", @"i64" }, @"i64");
    const bin_u64 = try fnType(&env, &.{ @"u64", @"u64" }, @"u64");
    const un_ty = try fnType(&env, &.{@"type"}, @"type");
    const bin_i64_cond = try fnType(&env, &.{ @"i64", @"i64" }, @"bool");

    const type_slice_ty = try env.identify(Type.initPtr(.slice, @"type"));
    const fn_ty = try fnType(&env, &.{ type_slice_ty, @"type" }, @"type");
    const tuple_ty = try fnType(&env, &.{type_slice_ty}, @"type");

    try defBuiltin(&env, "ns", flbuiltin, .ns);
    try defBuiltin(&env, "def", flbuiltin, .def);
    try defBuiltin(&env, "as", flbuiltin, .cast);
    try defBuiltin(&env, "&", flbuiltin, .addr_of);
    try defBuiltin(&env, ".", flbuiltin, .access);
    try defBuiltin(&env, "array", flbuiltin, .array);
    try defBuiltin(&env, "lambda", flbuiltin, .lambda);
    try defBuiltin(&env, "recur", flbuiltin, .recur);
    try defBuiltin(&env, "if", flbuiltin, .@"if");

    try defBuiltin(&env, "=", bin_i64_cond, .eq);
    try defBuiltin(&env, "+", bin_i64, .add);
    try defBuiltin(&env, "-", bin_i64, .sub);
    try defBuiltin(&env, "*", bin_i64, .mul);
    try defBuiltin(&env, "/", bin_i64, .div);
    try defBuiltin(&env, "%", bin_i64, .mod);
    try defBuiltin(&env, "<<", bin_u64, .shl);
    try defBuiltin(&env, ">>", bin_u64, .shr);

    try defBuiltin(&env, "and", bin_cond, .@"and");
    try defBuiltin(&env, "or", bin_cond, .@"or");
    try defBuiltin(&env, "not", un_cond, .not);

    try defBuiltin(&env, "Fn", fn_ty, .fn_ty);
    try defBuiltin(&env, "Slice", un_ty, .slice_ty);
    try defBuiltin(&env, "Tuple", tuple_ty, .tuple_ty);

    return env;
}
