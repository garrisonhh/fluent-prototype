//! defining fluent builtins.

const std = @import("std");
const util = @import("util");
const types = @import("types.zig");
const Env = @import("env.zig");

const Allocator = std.mem.Allocator;
const Symbol = util.Symbol;
const Type = types.Type;

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

pub fn initPrelude(ally: Allocator) Env.DefError!Env {
    var env = Env.init(ally);

    _ = try env.typeDef(sym("type"), Type{ .ty = {} });
    _ = try env.typeDef(sym("unit"), Type{ .unit = {} });
    _ = try env.typeDef(sym("i64"), Type{
        .number = .{ .layout = .int, .bits = 64 }
    });

    return env;
}