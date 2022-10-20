//! contains all of the information of SExpr, but with extra analysis applied.
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

pub const Tag = std.meta.Tag(Data);
pub const Data = union(enum) {
    number: SExpr.Number,
    string: Symbol,
    symbol: Symbol,
    call: []Self,

    // special syntax
    do: []Self,
    list: []Self,

    /// for easy literal analysis
    pub fn fromSExprData(
        ally: Allocator,
        data: SExpr.Data
    ) Allocator.Error!Data {
        return switch (data) {
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
            .number => data,
            .string => |sym| Data{ .string = try sym.clone(ally) },
            .symbol => |sym| Data{ .symbol = try sym.clone(ally) },
            .call => |exprs| Data{ .call = try cloneChildren(exprs, ally) },
            .do => |exprs| Data{ .do = try cloneChildren(exprs, ally) },
            .list => |exprs| Data{ .list = try cloneChildren(exprs, ally) },
        };
    }
};

data: Data,
loc: Loc,
ty: TypeId,

fn init(loc: Loc, ty: TypeId, data: Data) Self {
    return Self{
        .data = data,
        .loc = loc,
        .ty = ty
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
        .ty = self.ty,
    };
}

pub fn render(
    self: Self,
    env: Env,
    ally: Allocator
) !kz.Texture {
    const INDENT = 4;
    const faint = kz.Format{ .special = .faint };
    const magenta = kz.Format{ .fg = .magenta };
    const green = kz.Format{ .fg = .green };
    const red = kz.Format{ .fg = .red };

    // render type
    const ty_tex = render_ty: {
        const ty = env.typeGet(self.ty);
        const text = try ty.writeAlloc(ally, env.typewelt);
        defer ally.free(text);

        break :render_ty try kz.Texture.print(ally, faint, "{s}", .{text});
    };
    defer ty_tex.deinit(ally);

    // render data
    var offset = kz.Offset{@intCast(isize, ty_tex.size[0] + 1), 0};
    const data_tex = switch (self.data) {
        .number => |num| try kz.Texture.print(ally, magenta, "{}", .{num}),
        .string => |sym|
            try kz.Texture.print(ally, green, "\"{}\"", .{sym}),
        .symbol => |sym| try kz.Texture.print(ally, red, "{}", .{sym}),
        .call, .do, .list => |exprs| call: {
            // collect
            var list = std.ArrayList(kz.Texture).init(ally);
            defer {
                for (list.items) |tex| tex.deinit(ally);
                list.deinit();
            }

            switch (self.data) {
                .do => {
                    const name = @tagName(self.data);
                    const tex = try kz.Texture.from(ally, kz.Format{}, name);
                    try list.append(tex);
                },
                else => {}
            }

            for (exprs) |expr| {
                try list.append(try expr.render(env, ally));
            }

            // stack
            offset = .{INDENT, 1};
            break :call
                try kz.Texture.stack(ally, list.items, .bottom, .close);
        },
    };
    defer data_tex.deinit(ally);

    return ty_tex.unify(ally, data_tex, offset);
}