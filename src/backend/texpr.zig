//! contains all of the information of SExpr, but with extra type information.
//! this is the glorious holy altar to which we must sacrifice all data.
//!
//! instances own all data.

const std = @import("std");
const util = @import("util");
const kz = @import("kritzler");
const context = @import("../context.zig");
const types = @import("types.zig");
const Env = @import("env.zig");
const SExpr = @import("sexpr.zig");

const Allocator = std.mem.Allocator;
const Symbol = util.Symbol;
const Loc = context.Loc;
const TypeId = types.TypeId;

const Self = @This();

pub const Number = SExpr.Number;

pub const Tag = std.meta.Tag(Data);
pub const Data = union(enum) {
    @"bool": bool,
    number: Number,
    string: Symbol,
    symbol: Symbol,
    call: []Self,

    // special syntax
    do: []Self,
    list: []Self,
    cast: *Self,

    pub fn deinit(self: Data, ally: Allocator) void {
        switch (self) {
            .@"bool", .number => {},
            .string, .symbol => |sym| ally.free(sym.str),
            .call, .do, .list => |exprs| {
                for (exprs) |expr| expr.deinit(ally);
                ally.free(exprs);
            },
            .cast => |expr| {
                expr.deinit(ally);
                ally.destroy(expr);
            },
        }
    }

    /// for easy literal analysis
    pub fn fromSExprData(
        ally: Allocator,
        data: SExpr.Data
    ) Allocator.Error!Data {
        return switch (data) {
            .@"bool" => |val| Data{ .@"bool" = val },
            .number => |num| Data{ .number = num },
            .string => |sym| Data{ .string = try sym.clone(ally) },
            .symbol => |sym| Data{ .symbol = try sym.clone(ally) },
            else => unreachable
        };
    }

    fn cloneChildren(
        children: []const Self,
        ally: Allocator
    ) Allocator.Error![]Self {
        const cloned = try ally.alloc(Self, children.len);
        for (children) |child, i| cloned[i] = try child.clone(ally);

        return cloned;
    }

    pub fn clone(data: Data, ally: Allocator) Allocator.Error!Data {
        return switch (data) {
            .@"bool", .number => data,
            .string => |sym| Data{ .string = try sym.clone(ally) },
            .symbol => |sym| Data{ .symbol = try sym.clone(ally) },
            .call => |exprs| Data{ .call = try cloneChildren(exprs, ally) },
            .do => |exprs| Data{ .do = try cloneChildren(exprs, ally) },
            .list => |exprs| Data{ .list = try cloneChildren(exprs, ally) },
            .cast => |expr| Data{
                .cast = try util.placeOn(ally, try expr.clone(ally))
            },
        };
    }

    fn eqlChildren(children: []const Self, other_children: []const Self) bool {
        if (children.len != other_children.len) {
            return false;
        }

        var i: usize = 0;
        while (i < children.len) : (i += 1) {
            if (!children[i].eql(other_children[i])) {
                return false;
            }
        }

        return true;
    }

    fn eql(data: Data, other: Data) bool {
        if (@as(Tag, data) != @as(Tag, other)) {
            return false;
        }

        return switch (data) {
            .@"bool" => |b| b == other.@"bool",
            .number => |n| n.eql(other.number),
            .string => |sym| sym.eql(other.string),
            .symbol => |sym| sym.eql(other.symbol),
            .call => |call| eqlChildren(call, other.call),
            .do => |do| eqlChildren(do, other.do),
            .list => |list| eqlChildren(list, other.list),
            .cast => |to| to.eql(other.cast.*),
        };
    }
};

data: Data,
loc: ?Loc,
ty: TypeId,

pub fn init(loc: ?Loc, ty: TypeId, data: Data) Self {
    return Self{
        .data = data,
        .loc = loc,
        .ty = ty,
    };
}

pub fn deinit(self: Self, ally: Allocator) void {
    self.data.deinit(ally);
}

pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
    return Self{
        .data = try self.data.clone(ally),
        .loc = self.loc,
        .ty = self.ty,
    };
}

pub fn eql(self: Self, other: Self) bool {
    if (!self.ty.eql(other.ty)) {
        return false;
    }

    return self.data.eql(other.data);
}

/// gets children of any TExpr if they exist.
///
/// useful for recursive operations on TExprs.
pub fn getChildren(self: Self) []Self {
    return switch (self.data) {
        .call, .do, .list => |children| children,
        .cast => |expr| @ptrCast([*]Self, expr)[0..1],
        else => &.{}
    };
}

pub fn render(
    self: Self,
    ctx: *kz.Context,
    env: Env
) Allocator.Error!kz.Ref {
    const INDENT = 2;
    const faint = kz.Style{ .special = .faint };
    const magenta = kz.Style{ .fg = .magenta };
    const green = kz.Style{ .fg = .green };
    const red = kz.Style{ .fg = .red };

    // type for header
    const ty_text = try env.typeGet(self.ty).writeAlloc(ctx.ally, env);
    defer ctx.ally.free(ty_text);

    const ty = try ctx.print(faint, "{s}", .{ty_text});

    // other inline header stuff
    const data = switch (self.data) {
        .@"bool" => |val| try ctx.print(magenta, "{}", .{val}),
        .number => |num| try ctx.print(magenta, "{}", .{num}),
        .string => |sym| try ctx.print(green, "\"{}\"", .{sym}),
        .symbol => |sym| try ctx.print(red, "{}", .{sym}),
        .do, .cast, .call, .list
            => try ctx.print(.{}, "{s}", .{@tagName(self.data)}),
    };

    // header
    const header = try ctx.slap(ty, data, .right, .{ .space = 1 });

    // any children
    var children = ctx.stub();
    for (self.getChildren()) |child| {
        const tex = try child.render(ctx, env);
        children = try ctx.slap(children, tex, .bottom, .{});
    }

    // slap and return everything
    const offset = kz.Offset{INDENT, @intCast(isize, ctx.getSize(header)[1])};
    return try ctx.unify(header, children, offset);
}