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

const Self = @This();

pub const Tag = std.meta.Tag(Data);
pub const Number = canon.Number;

pub const Data = union(enum) {
    number: Number,
    string: Symbol,
    symbol: Symbol,
    call: []Self,

    pub fn clone(data: Data, ally: Allocator) Allocator.Error!Data {
        return switch (data) {
            .number => data,
            inline .string, .symbol => |sym, tag|
                @unionInit(Data, @tagName(tag), try sym.clone(ally)),
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

pub fn init(loc: Loc, data: Data) Self {
    return Self{
        .loc = loc,
        .data = data,
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
    comptime _: []const u8,
    _: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    switch (self.data) {
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
    }
}