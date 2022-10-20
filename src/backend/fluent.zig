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
    _ = tmp_ally;

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

    const @"i64" = try env.typeIdentifyNumber(.int, 64);
    const @"f32" = try env.typeDef(sym("f32"), Type{
        .number = .{ .layout = .float, .bits = 32 }
    });
    const @"f64" = try env.typeDef(sym("f64"), Type{
        .number = .{ .layout = .float, .bits = 64 }
    });

    _ = @"f32";
    _ = @"f64";

    // math functions
    const bin_i64 = try env.typeIdentify(funcType(
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