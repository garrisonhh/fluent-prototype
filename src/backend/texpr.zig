//! contains all of the information of SExpr, but with extra type information.
//! this is the glorious holy altar to which we must sacrifice all data.
//!
//! instances own all data, except for names

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const util = @import("util");
const Symbol = util.Symbol;
const Name = util.Name;
const kz = @import("kritzler");
const context = @import("../context.zig");
const Loc = context.Loc;
const types = @import("types.zig");
const TypeId = types.TypeId;
const TypeWelt = types.TypeWelt;
const SExpr = @import("sexpr.zig");
const canon = @import("canon.zig");

const Self = @This();

pub const Number = canon.Number;

pub const Builtin = enum {
    ns,

    // ops
    list,
    cast,

    // control flow
    do,
    @"if",

    pub fn getName(b: Builtin) []const u8 {
        return switch (b) {
            inline else => |tag| @tagName(tag),
        };
    }
};

pub const Tag = std.meta.Tag(Data);

pub const Data = union(enum) {
    unit,
    @"bool": bool,
    number: Number,
    string: Symbol,
    ty: TypeId,
    name: Name,
    call: []Self,

    // special syntax, often used as the head of a call texpr
    // TODO I should have a `lambda` builtin and then a `template` builtin which
    // stores a function that hasn't been monomorphized. lambdas can then store
    // compiled SSA and/or bytecode, and templates can store generic parameters
    // and a SExpr which is only analyzed when used. when a template is
    // monomorphized, it can then be stored back in the env as a fully
    // functional lambda.
    builtin: Builtin,

    /// for easy literal analysis
    pub fn fromSExprData(
        ally: Allocator,
        data: SExpr.Data
    ) Allocator.Error!Data {
        return switch (data) {
            .@"bool" => |val| Data{ .@"bool" = val },
            .number => |num| Data{ .number = num },
            .string => |sym| Data{ .string = try sym.clone(ally) },
            else => unreachable
        };
    }

    fn eql(data: Data, other: Data) bool {
        if (@as(Tag, data) != @as(Tag, other)) {
            return false;
        }

        return switch (data) {
            .ty, .unit => true,
            .@"bool" => |b| b == other.@"bool",
            .number => |n| n.eql(other.number),
            .string => |sym| sym.eql(other.string),
            .name => |name| name.eql(other.name),
            .call => |exprs| call: {
                const other_exprs = other.call;
                if (exprs.len != other_exprs.len) {
                    break :call false;
                }

                for (exprs) |expr, i| {
                    if (expr.eql(other_exprs[i])) {
                        break :call false;
                    }
                }

                break :call true;
            },
            .builtin => |b| b == other.data.builtin,
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

pub fn initBuiltin(loc: ?Loc, ty: TypeId, b: Builtin) Self {
    return Self.init(loc, ty, Data{ .builtin = b });
}

pub fn initCall(
    ally: Allocator,
    loc: ?Loc,
    ty: TypeId,
    head: Self,
    nargs: usize
) Allocator.Error!Self {
    const exprs = try ally.alloc(Self, nargs + 1);
    exprs[0] = head;

    return Self.init(loc, ty, Data{ .call = exprs });
}

pub fn deinit(self: Self, ally: Allocator) void {
    switch (self.data) {
        .unit, .ty, .@"bool", .number, .name, .builtin => {},
        .string => |str| ally.free(str.str),
        .call => |children| {
            for (children) |child| child.deinit(ally);
            ally.free(children);
        }
    }
}

pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
    const data = switch (self.data) {
        .unit, .ty, .@"bool", .number, .name, .builtin => self.data,
        .string => |sym| Data{ .string = try sym.clone(ally) },
        .call => |children| call: {
            const cloned = try ally.alloc(Self, children.len);
            for (children) |child, i| {
                cloned[i] = try child.clone(ally);
            }

            break :call Data{ .call = cloned };
        }
    };

    return Self{
        .data = data,
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
    return if (self.data == .call) self.data.call else &.{};
}

pub fn isBuiltin(self: Self, tag: Builtin) bool {
    return self.data == .builtin and self.data.builtin == tag;
}

/// finds whether the TExpr represents a value, meaning that it requires no
/// further execution
///
/// TODO in many cases when I create a value TExpr I already know that it is a
/// value TExpr, so I should cache `known_value: bool` and add it to `init`
pub fn isValue(self: Self) bool {
    return switch (self.data) {
        .unit, .ty, .@"bool", .number, .string, .builtin => true,
        .name, .call => false,
    };
}

pub fn render(
    self: Self,
    ctx: *kz.Context,
    tw: TypeWelt
) Allocator.Error!kz.Ref {
    const INDENT = 2;
    const faint = kz.Style{ .special = .faint };
    const magenta = kz.Style{ .fg = .magenta };
    const green = kz.Style{ .fg = .green };
    const red = kz.Style{ .fg = .red };

    // type for header
    const ty_text = try tw.get(self.ty).writeAlloc(ctx.ally, tw);
    defer ctx.ally.free(ty_text);

    const ty_tex = try ctx.print(faint, "{s}", .{ty_text});

    // other inline header stuff
    const data = switch (self.data) {
        .call, .unit => try ctx.print(.{}, "{s}", .{@tagName(self.data)}),
        .ty => |ty| ty: {
            const str = try tw.get(ty).writeAlloc(ctx.ally, tw);
            defer ctx.ally.free(str);
            break :ty try ctx.print(green, "{s}", .{str});
        },
        .@"bool" => |val| try ctx.print(magenta, "{}", .{val}),
        .number => |num| try ctx.print(magenta, "{}", .{num}),
        .string => |str| try ctx.print(green, "\"{}\"", .{str}),
        .name => |name| try ctx.print(red, "{}", .{name}),
        .builtin => |b| try ctx.print(red, "{s}", .{@tagName(b)}),
    };

    // header
    const header = try ctx.slap(ty_tex, data, .right, .{ .space = 1 });

    // any children
    var children = try ctx.stub();
    for (self.getChildren()) |child| {
        const tex = try child.render(ctx, tw);
        children = try ctx.slap(children, tex, .bottom, .{});
    }

    // slap and return everything
    const offset = kz.Offset{INDENT, @intCast(isize, ctx.getSize(header)[1])};
    return try ctx.unify(header, children, offset);
}