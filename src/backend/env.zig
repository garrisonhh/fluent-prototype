//! Env allows fluent to bind symbols to values. Envs can be stacked recursively
//! to allow for scopes within scopes.
//! instances own all bound keys and values.

const std = @import("std");
const util = @import("../util/util.zig");
const canvas = @import("../util/canvas.zig");
const fluent = @import("fluent.zig");

const stdout = std.io.getStdOut().writer();
const Allocator = std.mem.Allocator;
const SExpr = fluent.SExpr;
const ConsoleColor = canvas.ConsoleColor;

const Self = @This();

const Map = std.StringHashMap(SExpr);

ally: Allocator,
map: Map,
parent: ?*Self,

/// only global will ever want a null parent
pub fn init_internal(ally: Allocator, parent: ?*Self) Allocator.Error!Self {
    return Self{ .ally = ally, .map = Map.init(ally), .parent = parent };
}

pub fn init(ally: Allocator, parent: *Self) Allocator.Error!Self {
    return try init_internal(ally, parent);
}

pub fn deinit(self: *Self) void {
    // free symbols and deinit values
    var iter = self.map.iterator();
    while (iter.next()) |entry| {
        self.ally.free(entry.key_ptr.*);
        entry.value_ptr.deinit(self.ally);
    }

    self.map.deinit();
}

/// dupes symbol, assumes you already allocated or cloned value onto the env
/// allocator. errors on redefinition of a value.
pub fn define_raw(self: *Self, symbol: []const u8, value: SExpr) !void {
    if (self.get(symbol) != null) return error.SymbolAlreadyDefined;
    try self.map.put(try self.ally.dupe(u8, symbol), value);
}

/// clones value and defines it.
pub fn define(self: *Self, symbol: []const u8, value: SExpr) !void {
    try self.define_raw(symbol, try value.clone(self.ally));
}

pub fn get(self: Self, symbol: []const u8) ?SExpr {
    return if (self.map.get(symbol)) |val| val
           else if (self.parent) |parent|
           parent.get(symbol) else null;
}

/// TODO canvas repr
/// TODO generic Table builder for canvas (since this is a common use case)
/// TODO even refactor canvas, fun project and useful
pub fn display(
    self: Self,
    comptime label_fmt: []const u8,
    comptime label_args: anytype
) !void {
    try stdout.print(label_fmt ++ "\n", label_args);

    var iter = self.map.iterator();
    while (iter.next()) |entry| {
        const symbol = entry.key_ptr.*;
        const val = entry.value_ptr;
        var stype = try val.infer_type(self.ally, self, null);
        defer stype.deinit(self.ally);

        try stdout.print(
            "{}{s}{} | {}<{}>{} | {}\n",
            .{
                &ConsoleColor{ .fg = .red },
                symbol,
                &ConsoleColor{},
                &ConsoleColor{ .fg = .green },
                stype,
                &ConsoleColor{},
                val
            }
        );
    }

    try stdout.writeAll("\n");
}