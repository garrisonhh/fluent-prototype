//! Env allows fluent to bind symbols to values. Envs can be stacked recursively
//! to allow for scopes within scopes.
//!
//! Env is used throughout both semantic analysis and ir generation stages.
//! Fluent program compilation can be thought of as generating an Env filled
//! with SExpr values representing the ast of the program, and then procedurally
//! filling them out.
//!
//! instances own all bound keys and values.

const std = @import("std");
const kz = @import("kritzler");
const util = @import("../util/util.zig");
const fluent = @import("fluent.zig");
const ir = @import("ir.zig");
const Builtin = @import("lang.zig").Builtin;

const stdout = std.io.getStdOut().writer();
const Allocator = std.mem.Allocator;
const SExpr = fluent.SExpr;
const SType = fluent.SType;

const Self = @This();

/// bound types and values must be typed, but only in certain circumstances do
/// they need a value
pub const Bound = struct {
    pub const Data = union(enum) {
        value: SExpr, // uncompiled values
        builtin: Builtin,
        param: usize, // numbered function parameters
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
            .param, .builtin => {},
            .value => |value| value.deinit(self.ally),
        }
    }
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
            .param, .builtin => to.data,
            .value => |value| Bound.Data{ .value = try value.clone(self.ally) },
        }
    };

    try self.define_raw(symbol, cloned);
}

/// define() helper
pub fn define_param(
    self: *Self,
    symbol: []const u8,
    stype: SType,
    index: usize
) !void {
    try self.define(symbol, Bound{
        .stype = stype,
        .data = .{ .param = index }
    });
}

/// define() helper
pub fn define_builtin(
    self: *Self,
    symbol: []const u8,
    stype: SType,
    builtin: Builtin
) !void {
    try self.define(symbol, Bound{
        .stype = stype,
        .data = .{ .builtin = builtin }
    });
}

/// define() helper
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

/// define() helper
pub fn define_type(self: *Self, symbol: []const u8, stype: SType) !void {
    try self.define_value(
        symbol,
        SType{ .stype = {} },
        SExpr{ .stype = stype }
    );
}

fn get(self: Self, symbol: []const u8) ?Bound {
    return if (self.map.get(symbol)) |bound| bound
           else if (self.parent) |parent| parent.get(symbol)
           else null;
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

    // collect and order env's variables
    const EnvVar = Map.Unmanaged.Entry;
    const Closure = struct {
        // sorts first by bound type, then alphabetically
        fn ev_less_than(ctx: void, a: EnvVar, b: EnvVar) bool {
            _ = ctx;

            const cmp = @intCast(isize, @enumToInt(b.value_ptr.data))
                      - @intCast(isize, @enumToInt(a.value_ptr.data));

            if (cmp > 0) return true
            else if (cmp < 0) return false
            else return std.mem.lessThan(u8, a.key_ptr.*, b.key_ptr.*);
        }
    };

    var env_vars = try self.ally.alloc(EnvVar, self.map.count());
    defer self.ally.free(env_vars);

    var i: usize = 0;
    var iter = self.map.iterator();
    while (iter.next()) |entry| : (i += 1) env_vars[i] = entry;

    std.sort.sort(EnvVar, env_vars, {}, Closure.ev_less_than);

    // add to table and flush
    for (env_vars) |ev| {
        const symbol = ev.key_ptr.*;
        const bound = ev.value_ptr;

        const stype = &bound.stype;
        const data = switch (bound.data) {
            .param => |index| try table.print("param {}", .{index}),
            .builtin => |builtin| try table.print("{}", .{builtin}),
            .value => |value| try table.print("{}", .{value}),
        };

        try table.add_row(.{ symbol, stype, data });
    }

    try table.flush(stdout);
}
