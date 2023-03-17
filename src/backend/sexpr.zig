//! the raw, untyped, but structurally correct version of the AST. this acts as
//! the bridge between the more pedantic TExpr (typed expression) and raw forms
//! of fluent code.
//!
//! SExprs expect to own all of their data.

const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");
const Symbol = com.Symbol;
const Loc = com.Loc;
const canon = @import("canon.zig");
const render_sexpr = @import("render_sexpr.zig");

const Self = @This();

pub const Tag = std.meta.Tag(Data);
pub const Number = canon.Number;

pub const Data = union(enum) {
    number: Number,
    string: Symbol,
    symbol: Symbol,
    call: []Self,
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
        else => {},
    }
}

pub const format = render_sexpr.format;
pub const render = render_sexpr.render;

pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
    return Self{
        .loc = self.loc,
        .data = switch (self.data) {
            .number => self.data,
            inline .string, .symbol => |sym, tag| @unionInit(
                Data,
                @tagName(tag),
                try sym.clone(ally),
            ),
            inline .call, .array => |exprs, tag| many: {
                const cloned = try ally.alloc(Self, exprs.len);
                for (exprs) |expr, i| cloned[i] = try expr.clone(ally);

                break :many @unionInit(Data, tag, cloned);
            },
        },
    };
}
