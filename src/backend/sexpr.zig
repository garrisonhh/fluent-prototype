//! the raw, untyped, but structurally correct version of the AST. this acts as
//! the bridge between the more pedantic TExpr (typed expression) and raw forms
//! of fluent code.
//!
//! SExprs expect to own all of their data.

const std = @import("std");
const kz = @import("kritzler");
const util = @import("util");
const context = @import("../context.zig");

const Allocator = std.mem.Allocator;
const Symbol = util.Symbol;
const Loc = context.Loc;

const Self = @This();

pub const Number = struct {
    const Int = i64;
    const UInt = u64;
    const Float = f64;

    bits: ?u8,
    data: union(util.Number.Layout) {
        int: Int,
        uint: UInt,
        float: Float,
    },

    pub fn from(num: util.Number) util.ParseNumberError!Number {
        return Number{
            .bits = num.bits,
            .data = if (num.layout) |layout| switch (layout) {
                .int  => .{ .int = try num.to(Int) },
                .uint  => .{ .uint = try num.to(UInt) },
                .float => .{ .float = try num.to(Float) },
            } else if (num.post.len > 0) .{ .float = try num.to(Float) }
            else .{ .int = try num.to(Int) }
        };
    }

    fn dupeBytes(ally: Allocator, value: anytype) Allocator.Error![]const u8 {
        return try ally.dupe(u8, std.mem.asBytes(&value));
    }

    /// returns this value as bytes on an allocator
    pub fn asBytes(self: @This(), ally: Allocator) Allocator.Error![]const u8 {
        const bits = self.bits orelse 64;
        return switch (bits) {
            64 => switch (self.data) {
                .int => |n| dupeBytes(ally, n),
                .uint => |n| dupeBytes(ally, n),
                .float => |n| dupeBytes(ally, n),
            },
            32 => switch (self.data) {
                .int => |n| dupeBytes(ally, @intCast(i32, n)),
                .uint => |n| dupeBytes(ally, @intCast(u32, n)),
                .float => |n| dupeBytes(ally, @floatCast(f32, n)),
            },
            16 => switch (self.data) {
                .int => |n| dupeBytes(ally, @intCast(i16, n)),
                .uint => |n| dupeBytes(ally, @intCast(u16, n)),
                .float => unreachable,
            },
            8 => switch (self.data) {
                .int => |n| dupeBytes(ally, @intCast(i8, n)),
                .uint => |n| dupeBytes(ally, @intCast(u8, n)),
                .float => unreachable,
            },
            else => unreachable
        };
    }

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        _ = fmt;
        _ = options;

        switch (self.data) {
            .int => |i| try writer.print("{}i", .{i}),
            .uint => |u| try writer.print("{}u", .{u}),
            .float => |f| try writer.print("{d}f", .{f}),
        }

        if (self.bits) |bits| try writer.print("{}", .{bits});
    }
};

pub const Tag = std.meta.Tag(Data);

pub const Data = union(enum) {
    @"bool": bool,
    number: Number,
    string: Symbol,
    symbol: Symbol,
    call: []Self,

    pub fn clone(data: Data, ally: Allocator) Allocator.Error!Data {
        return switch (data) {
            .number => data,
            .string => |sym| Data{ .string = try sym.clone(ally) },
            .symbol => |sym| Data{ .symbol = try sym.clone(ally) },
            .call => |exprs| call: {
                const cloned = try ally.alloc(Self, exprs.len);
                for (exprs) |expr, i| cloned[i] = try expr.clone(ally);
                break :call Data{ .call = cloned };
            },
        };
    }
};

data: Data,
loc: Loc,

/// expects input to be already owned
pub fn initCall(loc: Loc, exprs: []Self) Self {
    return Self{
        .data = .{ .call = exprs },
        .loc = loc
    };
}

pub fn initBool(loc: Loc, @"bool": bool) Self {
    return Self{
        .data = .{ .@"bool" = @"bool" },
        .loc = loc,
    };
}

pub fn initNumber(loc: Loc, num: Number) Self {
    return Self{
        .data = .{ .number = num },
        .loc = loc,
    };
}

pub fn initSymbol(
    ally: Allocator,
    loc: Loc,
    str: []const u8
) Allocator.Error!Self {
    return Self{
        .data = .{ .symbol = Symbol.init(try ally.dupe(u8, str)) },
        .loc = loc,
    };
}

pub fn initString(
    ally: Allocator,
    loc: Loc,
    str: []const u8
) Allocator.Error!Self {
    return Self{
        .data = .{ .string = Symbol.init(try ally.dupe(u8, str)) },
        .loc = loc,
    };
}

pub fn deinit(self: Self, ally: Allocator) void {
    switch (self.data) {
        .call => |children| {
            for (children) |child| child.deinit(ally);
            ally.free(children);
        },
        .string, .symbol => |sym| ally.free(sym.str),
        else => {}
    }
}

pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
    return Self{
        .data = try self.data.clone(ally),
        .loc = self.loc,
    };
}

pub fn format(
    self: Self,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    _ = fmt;
    _ = options;

    switch (self.data) {
        .@"bool" => |val| try writer.print("{}", .{val}),
        .number => |num| try writer.print("{}", .{num}),
        .string => |sym| try writer.print("\"{s}\"", .{sym.str}),
        .symbol => |sym| try writer.print("{s}", .{sym.str}),
        .call => |exprs| {
            try writer.writeByte('(');
            for (exprs) |expr, i| {
                if (i > 0) try writer.writeByte(' ');
                try writer.print("{}", .{expr});
            }
            try writer.writeByte(')');
        }
    }
}