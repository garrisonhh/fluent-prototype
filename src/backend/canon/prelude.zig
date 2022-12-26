//! defining the initial state of a program's env.

const std = @import("std");
const Allocator = std.mem.Allocator;
const util = @import("util");
const Symbol = util.Symbol;
const types = @import("../types.zig");
const Type = types.Type;
const TypeId = types.TypeId;
const TypeWelt = types.TypeWelt;
const Env = @import("../env.zig");
const TExpr = @import("../texpr.zig");
const Builtin = @import("../canon.zig").Builtin;

fn filterDefError(e: Env.DefError, comptime str: []const u8) Allocator.Error {
    return switch (e) {
        error.NameRedef, error.NameNoRedef, error.RenamedType, error.NameTooLong
            => {
            const fmt = "uh oh, got {} in prelude definition while trying "
                     ++ "to define {s}.";
            std.debug.panic(fmt, .{e, str});
        },
        error.OutOfMemory => return @errSetCast(Allocator.Error, e)
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
    ty: Type
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
    layout: util.Number.Layout,
    bits: ?u8
) Allocator.Error!TypeId {
    return try defType(env, str, Type{
        .number = .{ .layout = layout, .bits = bits }
    });
}

fn funcType(
    ally: Allocator,
    takes: []const TypeId,
    returns: TypeId
) Allocator.Error!Type {
    return Type{
        .func = Type.Func{
            .takes = try ally.dupe(TypeId, takes),
            .returns = returns
        }
    };
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
    _ = @"type";
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

    const int_ids = &[_]TypeId{@"i8", @"i16", @"i32", @"i64", compiler_int};
    const uint_ids = &[_]TypeId{@"u8", @"u16", @"u32", @"u64"};
    const float_ids = &[_]TypeId{@"f32", @"f64", compiler_float};
    const int_ty = try Type.initSet(tmp, int_ids);
    const uint_ty = try Type.initSet(tmp, uint_ids);
    const float_ty = try Type.initSet(tmp, float_ids);

    const int = try defType(&env, "Int", int_ty);
    const uint = try defType(&env, "UInt", uint_ty);
    const float = try defType(&env, "Float", float_ty);

    const number_ids = try tmp.alloc(
        TypeId,
        int_ids.len + uint_ids.len + float_ids.len
    );
    var i: usize = 0;
    for ([_][]TypeId{int_ids, uint_ids, float_ids}) |id_list| {
        for (id_list) |id| {
            number_ids[i] = id;
            i += 1;
        }
    }

    const number_ty = try Type.initSet(tmp, number_ids);
    const number = try defType(&env, "Number", number_ty);

    _ = int;
    _ = uint;
    _ = float;
    _ = number;

    // the bool consts
    try def(&env, "true", TExpr.init(null, @"bool", .{ .@"bool" = true }));
    try def(&env, "false", TExpr.init(null, @"bool", .{ .@"bool" = false }));

    // some nice metaprogramming to define all of the builtins
    inline for (comptime std.enums.values(Builtin)) |b| {
        if (comptime b.getName()) |name| {
            try def(&env, name, TExpr.initBuiltin(null, flbuiltin, b));
        }
    }

    return env;
}