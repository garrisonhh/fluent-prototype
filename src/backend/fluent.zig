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

fn funcType(
    ally: Allocator,
    takes: []TypeId,
    contexts: []TypeId,
    returns: TypeId
) Allocator.Error!Type {
    return Type{
        .func = Type.Func{
            .takes = try ally.dupe(TypeId, takes),
            .contexts = try ally.dupe(TypeId, contexts),
            .returns = returns
        }
    };
}

pub fn initPrelude(ally: Allocator) Env.DefError!Env {
    var arena = std.heap.ArenaAllocator.init(ally);
    defer arena.deinit();
    const tmp_ally = arena.allocator();

    var env = Env.init(ally);

    _ = try env.typeDef(sym("type"), Type{ .ty = {} });
    _ = try env.typeDef(sym("unit"), Type{ .unit = {} });

    const @"i64" = try env.typeDef(sym("i64"), Type{
        .number = .{ .layout = .int, .bits = 64 }
    });

    const bin_i64 = try env.typeIdentify(try funcType(
        tmp_ally,
        &.{@"i64", @"i64"},
        &.{},
        @"i64"
    ));

    _ = try env.def(sym("addi"), .{
        .ty = bin_i64,
        .value = .{ .unimpl = {} },
    });

    return env;
}