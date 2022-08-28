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
const SType = fluent.SType;
const ConsoleColor = canvas.ConsoleColor;

const Self = @This();

/// bound types and values must be typed, but only in certain circumstances do
/// they need a value
pub const Bound = struct {
    stype: SType,
    value: ?SExpr = null,
};
const Map = std.StringHashMap(Bound);

ally: Allocator,
map: Map,
parent: ?*const Self,

/// only global will ever want a null parent
pub fn init_internal(
    ally: Allocator,
    parent: ?*const Self
) Allocator.Error!Self {
    return Self{ .ally = ally, .map = Map.init(ally), .parent = parent };
}

pub fn init(ally: Allocator, parent: *const Self) Allocator.Error!Self {
    return try init_internal(ally, parent);
}

pub fn deinit(self: *Self) void {
    // free symbols and deinit values
    var iter = self.map.iterator();
    while (iter.next()) |entry| {
        self.ally.free(entry.key_ptr.*);
        entry.value_ptr.stype.deinit(self.ally);
        if (entry.value_ptr.value) |expr| expr.deinit(self.ally);
    }

    self.map.deinit();
}

/// dupes symbol, assumes you already allocated or cloned binding onto the env
/// allocator. errors on redefinition of a binding.
pub fn define_raw(self: *Self, symbol: []const u8, to: Bound) !void {
    if (self.get(symbol) != null) return error.SymbolAlreadyDefined;
    try self.map.put(try self.ally.dupe(u8, symbol), to);
}

/// clones bound and defines it.
pub fn define(self: *Self, symbol: []const u8, to: Bound) !void {
    const cloned = Bound{
        .stype = try to.stype.clone(self.ally),
        .value = if (to.value) |value| try value.clone(self.ally) else null
    };

    try self.define_raw(symbol, cloned);
}

fn get(self: Self, symbol: []const u8) ?Bound {
    return if (self.map.get(symbol)) |bound| bound
           else if (self.parent) |parent|
           parent.get(symbol) else null;
}

pub fn get_type(self: Self, symbol: []const u8) ?SType {
    return if (self.get(symbol)) |bound| bound.stype else null;
}

pub fn get_value(self: Self, symbol: []const u8) ?SExpr {
    return if (self.get(symbol)) |bound| bound.value else null;
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
        const bound = entry.value_ptr;

        try stdout.print(
            "{}{s}{} | {}<{}>{} | {}\n",
            .{
                &ConsoleColor{ .fg = .red },
                symbol,
                &ConsoleColor{},
                &ConsoleColor{ .fg = .green },
                bound.stype,
                &ConsoleColor{},
                bound.value
            }
        );
    }

    try stdout.writeAll("\n");
}