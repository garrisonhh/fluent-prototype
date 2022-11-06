//! defining fluent builtins.

const std = @import("std");
const util = @import("util");
const types = @import("types.zig");
const Env = @import("env.zig");

const Allocator = std.mem.Allocator;
const Symbol = util.Symbol;
const Type = types.Type;
const TypeId = types.TypeId;
const TypeWelt = types.TypeWelt;

pub const Builtin = enum {
    const Self = @This();

    do,
    as,
    list,

    // this obtuse piece of cude just generates a hashmap of the name of
    // each builtin to the enum value at compile time
    const nmap = nmap: {
        const fields = std.meta.fieldNames(Self);

        const Pair = struct { @"0": []const u8, @"1": Self };
        var kvs: [fields.len]Pair = undefined;
        for (fields) |field, i| {
            kvs[i] = Pair{
                .@"0" = field,
                .@"1" = @intToEnum(Self, i)
            };
        }

        break :nmap std.ComptimeStringMap(Self, kvs);
    };

    pub fn get(key: Symbol) ?Self {
        return nmap.get(key.str);
    }
};

fn sym(comptime str: []const u8) Symbol {
    return comptime Symbol.init(str);
}

fn funcType(
    generics: []TypeId,
    takes: []TypeId,
    contexts: []TypeId,
    returns: TypeId
) Type {
    return Type{
        .func = Type.Func{
            .generics = generics,
            .takes = takes,
            .contexts = contexts,
            .returns = returns
        }
    };
}

fn defOp(
    env: *Env,
    comptime name: []const u8,
    ty: TypeId,
    comptime op: Env.BuiltinOp
) !void {
    try env.def(sym(name), ty, .{ .builtin_op = op });
}

pub fn initPrelude(ally: Allocator, typewelt: *TypeWelt) !Env {
    var arena = std.heap.ArenaAllocator.init(ally);
    defer arena.deinit();
    const tmp_ally = arena.allocator();

    var env = try Env.initBase(ally, typewelt);

    _ = try env.typeDef(sym("type"), Type{ .ty = {} });
    _ = try env.typeDef(sym("unit"), Type{ .unit = {} });

    // define number types
    for ([_]util.Number.Layout{.int, .uint}) |layout| {
        var i: u3 = 0;
        while (i < 4) : (i += 1) {
            var name_buf: [16]u8 = undefined;
            const bits = 8 * (@as(u8, 1) << i);
            const name = try std.fmt.bufPrint(
                &name_buf,
                "{c}{d}",
                .{@tagName(layout)[0], bits}
            );

            const ty = Type{ .number = .{ .layout = layout, .bits = bits } };
            _ = try env.typeDef(Symbol.init(name), ty);
        }
    }

    const compiler_int = try env.typeDef(sym("compiler-int"), Type{
        .number = .{ .layout = .int, .bits = null }
    });
    const compiler_float = try env.typeDef(sym("compiler-float"), Type{
        .number = .{ .layout = .float, .bits = null }
    });

    const @"i8" = try env.typeIdentifyNumber(.int, 8);
    const @"i16" = try env.typeIdentifyNumber(.int, 16);
    const @"i32" = try env.typeIdentifyNumber(.int, 32);
    const @"i64" = try env.typeIdentifyNumber(.int, 64);
    const @"u8" = try env.typeIdentifyNumber(.uint, 8);
    const @"u16" = try env.typeIdentifyNumber(.uint, 16);
    const @"u32" = try env.typeIdentifyNumber(.uint, 32);
    const @"u64" = try env.typeIdentifyNumber(.uint, 64);
    const @"f32" = try env.typeDef(sym("f32"), Type{
        .number = .{ .layout = .float, .bits = 32 }
    });
    const @"f64" = try env.typeDef(sym("f64"), Type{
        .number = .{ .layout = .float, .bits = 64 }
    });

    const int_ids: []TypeId = &.{@"i8", @"i16", @"i32", @"i64", compiler_int};
    const uint_ids: []TypeId = &.{@"u8", @"u16", @"u32", @"u64"};
    const float_ids: []TypeId = &.{@"f32", @"f64", compiler_float};
    const int_ty = try Type.initSet(tmp_ally, int_ids);
    const uint_ty = try Type.initSet(tmp_ally, uint_ids);
    const float_ty = try Type.initSet(tmp_ally, float_ids);

    const int = try env.typeDef(sym("Int"), int_ty);
    const uint = try env.typeDef(sym("UInt"), uint_ty);
    const float = try env.typeDef(sym("Float"), float_ty);

    _ = int;
    _ = uint;
    _ = float;

    const number_ids = try tmp_ally.alloc(
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

    const number_ty = try Type.initSet(tmp_ally, number_ids);
    const number = try env.typeDef(sym("Number"), number_ty);

    // generics
    const _A = try env.typeIdentify(Type{ .generic = .{ .index = 0 } });
    const _B = try env.typeIdentify(Type{ .generic = .{ .index = 1 } });
    const _C = try env.typeIdentify(Type{ .generic = .{ .index = 2 } });

    _ = _B;
    _ = _C;

    // operators
    const binary_numeric = try env.typeIdentify(funcType(
        &.{number},
        &.{_A, _A},
        &.{},
        _A
    ));

    try defOp(&env, "+", binary_numeric, .add);
    try defOp(&env, "-", binary_numeric, .sub);
    try defOp(&env, "*", binary_numeric, .mul);
    try defOp(&env, "/", binary_numeric, .div);

    return env;
}