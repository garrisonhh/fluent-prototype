//! defining fluent builtins.

const std = @import("std");
const util = @import("util");
const types = @import("types.zig");
const Env = @import("env.zig");

const Allocator = std.mem.Allocator;
const Symbol = util.Symbol;
const Type = types.Type;
const TypeId = types.TypeId;

pub const Builtin = enum {
    const Self = @This();

    do,
    list,

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

fn funcType(takes: []TypeId, contexts: []TypeId, returns: TypeId) Type {
    return Type{
        .func = Type.Func{
            .takes = takes,
            .contexts = contexts,
            .returns = returns
        }
    };
}

pub fn initPrelude(ally: Allocator) !Env {
    var arena = std.heap.ArenaAllocator.init(ally);
    defer arena.deinit();
    const tmp_ally = arena.allocator();

    var env = Env.init(ally);

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

    const int_ids: []TypeId = &.{@"i8", @"i16", @"i32", @"i64"};
    const uint_ids: []TypeId = &.{@"u8", @"u16", @"u32", @"u64"};
    const float_ids: []TypeId = &.{@"f32", @"f64"};
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

    // math functions
    const bin_math = try env.typeIdentify(funcType(
        &.{number, number},
        &.{},
        number
    ));

    _ = try env.def(sym("+"), .{
        .ty = bin_math,
        .value = .{ .unimpl = {} },
    });

    return env;
}