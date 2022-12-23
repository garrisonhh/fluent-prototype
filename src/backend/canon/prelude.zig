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

fn defType(
    env: *Env,
    comptime str: []const u8,
    ty: Type
) Env.DefError!TypeId {
    const id = try env.identify(ty);
    _ = try env.defType(Env.ROOT, comptime Symbol.init(str), id);

    return id;
}

fn defNumeric(
    env: *Env,
    comptime str: []const u8,
    layout: util.Number.Layout,
    bits: ?u8
) Env.DefError!TypeId {
    return defType(env, str, Type{
        .number = .{ .layout = layout, .bits = bits }
    });
}

fn funcType(
    ally: Allocator,
    generics: []const TypeId,
    takes: []const TypeId,
    contexts: []const TypeId,
    returns: TypeId
) Allocator.Error!Type {
    return Type{
        .func = Type.Func{
            .generics = try ally.dupe(TypeId, generics),
            .takes = try ally.dupe(TypeId, takes),
            .contexts = try ally.dupe(TypeId, contexts),
            .returns = returns
        }
    };
}

pub fn generatePrelude(ally: Allocator) Env.DefError!Env {
    var arena = std.heap.ArenaAllocator.init(ally);
    defer arena.deinit();
    const tmp = arena.allocator();

    var env = try Env.init(ally);

    // basic typedefs
    const any = try defType(&env, "Any", Type{ .any = {} });
    const @"type" = try defType(&env, "type", Type{ .ty = {} });
    const unit = try defType(&env, "unit", Type{ .unit = {} });
    const @"bool" = try defType(&env, "bool", Type{ .@"bool" = {} });

    _ = any;
    _ = @"type";
    _ = unit;
    _ = @"bool";

    // define number types
    const compiler_int = try defNumeric(&env, "compiler-int", .int, null);
    const compiler_float = try defNumeric(&env, "compiler-int", .float, null);

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

    _ = int;
    _ = uint;
    _ = float;

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

    _ = number;

    // TODO add builtin functions back in here

    return env;
}