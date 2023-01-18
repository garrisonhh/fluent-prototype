//! the raw, untyped, but structurally correct version of the AST. this acts as
//! the bridge between the more pedantic TExpr (typed expression) and raw forms
//! of fluent code.
//!
//! SExprs expect to own all of their data.

const std = @import("std");
const Allocator = std.mem.Allocator;
const util = @import("util");
const Symbol = util.Symbol;
const Loc = util.Loc;
const canon = @import("canon.zig");
const Number = canon.Number;

const Self = @This();

pub const Tag = std.meta.Tag(Data);

pub const Data = union(enum) {
    @"bool": bool,
    number: Number,
    string: Symbol,
    symbol: Symbol,
    call: []Self,
    array: []Self,

    pub fn clone(data: Data, ally: Allocator) Allocator.Error!Data {
        return switch (data) {
            .number => data,
            .string => |sym| Data{ .string = try sym.clone(ally) },
            .symbol => |sym| Data{ .symbol = try sym.clone(ally) },
            inline .call, .array => |exprs, tag| many: {
                const cloned = try ally.alloc(Self, exprs.len);
                for (exprs) |expr, i| cloned[i] = try expr.clone(ally);

                break :many @unionInit(Data, tag, cloned);
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

pub fn initOwnedString(loc: Loc, str: []const u8) Self {
    return Self{
        .data = .{ .string = Symbol.init(str) },
        .loc = loc,
    };
}

pub fn initString(
    ally: Allocator,
    loc: Loc,
    str: []const u8
) Allocator.Error!Self {
    return Self.initOwnedString(loc, try ally.dupe(u8, str));
}

pub fn deinit(self: Self, ally: Allocator) void {
    switch (self.data) {
        .call, .array => |children| {
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
    comptime _: []const u8,
    _: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
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
        },
        .array => |exprs| {
            try writer.writeByte('[');
            for (exprs) |expr, i| {
                if (i > 0) try writer.writeByte(' ');
                try writer.print("{}", .{expr});
            }
            try writer.writeByte(']');
        }
    }
}