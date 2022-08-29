//! Env allows fluent to bind symbols to values. Envs can be stacked recursively
//! to allow for scopes within scopes.
//! instances own all bound keys and values.

const std = @import("std");
const kz = @import("kritzler");
const util = @import("../util/util.zig");
const fluent = @import("fluent.zig");

const stdout = std.io.getStdOut().writer();
const Allocator = std.mem.Allocator;
const SExpr = fluent.SExpr;
const SType = fluent.SType;

const Self = @This();

/// bound types and values must be typed, but only in certain circumstances do
/// they need a value
pub const Bound = struct {
    pub const Data = union(enum) {
        // used for stuff like function parameters, where I only really know
        // the type
        virtual,
        builtin, // TODO data here. I imagine I'm storing ops
        value: SExpr,
    };

    stype: SType,
    data: Data,
};
const Map = std.StringHashMap(Bound);

ally: Allocator,
map: Map,
parent: ?*const Self,

pub fn init(ally: Allocator, parent: ?*const Self) Allocator.Error!Self {
    return Self{ .ally = ally, .map = Map.init(ally), .parent = parent };
}

pub fn deinit(self: *Self) void {
    // free symbols and deinit values
    var iter = self.map.iterator();
    while (iter.next()) |entry| {
        self.ally.free(entry.key_ptr.*);

        const bound = entry.value_ptr;
        bound.stype.deinit(self.ally);

        switch (bound.data) {
            .virtual, .builtin => {},
            .value => |value| value.deinit(self.ally)
        }
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
        .data = switch (to.data) {
            .virtual, .builtin => to.data,
            .value => |value| Bound.Data{ .value = try value.clone(self.ally) },
        }
    };

    try self.define_raw(symbol, cloned);
}

pub fn define_virtual(self: *Self, symbol: []const u8, stype: SType) !void {
    try self.define(symbol, Bound{
        .stype = stype,
        .data = .{ .virtual = {} }
    });
}

pub fn define_value(
    self: *Self,
    symbol: []const u8,
    stype: SType,
    value: SExpr
) !void {
    try self.define(symbol, Bound{
        .stype = stype,
        .data = .{ .value = value }
    });
}

fn get(self: Self, symbol: []const u8) ?Bound {
    return if (self.map.get(symbol)) |bound| bound
           else if (self.parent) |parent|
           parent.get(symbol) else null;
}

pub fn get_type(self: Self, symbol: []const u8) ?SType {
    return if (self.get(symbol)) |bound| bound.stype else null;
}

pub fn get_data(self: Self, symbol: []const u8) ?Bound.Data {
    return if (self.get(symbol)) |bound| bound.data else null;
}

pub fn display(
    self: Self,
    comptime label_fmt: []const u8,
    label_args: anytype
) !void {
    var table = try kz.Table(&.{
        .{ .title = "symbol", .fmt = "{s}", .color = kz.Color{ .fg = .red } },
        .{ .title = "type", .fmt = "<{}>", .color = kz.Color{ .fg = .green } },
        .{ .title = "data", .fmt = "{s}" },
    }).init(self.ally, label_fmt, label_args);

    var iter = self.map.iterator();
    while (iter.next()) |entry| {
        const symbol = entry.key_ptr.*;
        const bound = entry.value_ptr;

        const data = switch (bound.data) {
            .virtual, .builtin => std.meta.tagName(bound.data),
            .value => |value| try table.print("{}", .{value}),
        };

        try table.add_row(.{ symbol, bound.stype, data });
    }

    try table.flush(stdout);
}