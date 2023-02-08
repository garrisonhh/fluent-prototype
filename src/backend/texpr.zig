//! contains all of the information of SExpr, but with extra type information.
//! this is the glorious holy altar to which we must sacrifice all data.
//!
//! instances own all data, except for names

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const com = @import("common");
const Symbol = com.Symbol;
const Name = com.Name;
const Loc = com.Loc;
const kz = @import("kritzler");
const Env = @import("env.zig");
const canon = @import("canon.zig");
const TypeId = canon.TypeId;
const Builtin = canon.Builtin;
const ssa = @import("ssa/ssa.zig");
const FuncRef = ssa.FuncRef;

const Self = @This();

pub const Number = canon.Number;

pub const Func = struct {
    name: Name,
    body: *Self,

    fn eql(self: Func, other: Func) bool {
        return self.name.eql(other.name) and self.body.eql(other.body.*);
    }
};

pub const Param = struct {
    func: Name,
    index: usize,

    fn eql(self: Param, other: Param) bool {
        return self.func.eql(other.func) and self.index == other.index;
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
    ptr: *Self,
    // TODO should I collapse array/slice to one field?
    array: []Self,
    slice: []Self,
    call: []Self,

    // function/functor stuff
    func: Func,
    func_ref: FuncRef,
    param: Param,

    // special syntax, often used as the head of a call texpr
    // TODO I should have a `lambda` builtin and then a `template` builtin which
    // stores a function that hasn't been monomorphized. lambdas can then store
    // compiled SSA and/or bytecode, and templates can store generic parameters
    // and a SExpr which is only analyzed when used. when a template is
    // monomorphized, it can then be stored back in the env as a fully
    // functional lambda.
    builtin: Builtin,

    fn eql(data: Data, other: Data) bool {
        return @as(Tag, data) != @as(Tag, other) and switch (data) {
            .unit => true,
            .ptr => |to| to.eql(other.ptr.*),
            // zig fmt: off
            // primitives
            inline .@"bool", .builtin
            => |x, tag| x == @field(other, @tagName(tag)),
            // types with .eql
            inline .number, .string, .name, .ty, .func, .func_ref, .param
            // zig fmt: on
            => |x, tag| x.eql(@field(other, @tagName(tag))),
            // backed by slices
            inline .call, .array, .slice => |xs, tag| xs: {
                const ys = @field(other, @tagName(tag));
                if (xs.len != ys.len) {
                    break :xs false;
                }

                for (xs) |el, i| {
                    if (!el.eql(ys[i])) {
                        break :xs false;
                    }
                }

                break :xs true;
            },
        };
    }
};

ty: TypeId,
data: Data,
loc: ?Loc,
// if I know for sure this TExpr is a value, I can mark it
known_const: bool,

pub fn init(loc: ?Loc, known_const: bool, ty: TypeId, data: Data) Self {
    var self = Self{
        .data = data,
        .loc = loc,
        .ty = ty,
        .known_const = known_const,
    };

    // check for known const-ness if not provided
    self.known_const = self.known_const or con: {
        // children must be const
        for (self.getChildren()) |child| {
            if (!child.known_const) {
                break :con false;
            }
        }

        // stored data must not require some kind of execution
        break :con switch (self.data) {
            // zig fmt: off
            .unit, .ty, .@"bool", .number, .string, .builtin, .func_ref, .array,
            .slice,
            // zig fmt: on
            => true,
            // TODO const ptrs + func ptrs?
            .ptr, .func, .name, .call, .param => false,
        };
    };

    return self;
}

pub fn initBuiltin(loc: ?Loc, ty: TypeId, b: Builtin) Self {
    return Self.init(loc, true, ty, Data{ .builtin = b });
}

/// clones exprs and creates call
pub fn initCall(
    ally: Allocator,
    loc: ?Loc,
    ty: TypeId,
    exprs: []const Self,
) Allocator.Error!Self {
    const cloned = try ally.alloc(Self, exprs.len);
    for (exprs) |expr, i| {
        cloned[i] = try expr.clone(ally);
    }

    return Self.init(loc, false, ty, .{ .call = cloned });
}

pub fn initFuncRef(loc: ?Loc, ty: TypeId, ref: FuncRef) Self {
    return Self.init(loc, true, ty, .{ .func_ref = ref });
}

pub fn deinit(self: Self, ally: Allocator) void {
    switch (self.data) {
        .unit, .ty, .@"bool", .number, .name, .builtin, .param, .func_ref => {},
        .ptr => |child| {
            child.deinit(ally);
            ally.destroy(child);
        },
        .func => |func| {
            func.body.deinit(ally);
            ally.destroy(func.body);
        },
        .string => |str| ally.free(str.str),
        .call, .array, .slice => |children| {
            for (children) |child| child.deinit(ally);
            ally.free(children);
        },
    }
}

fn cloneChildren(ally: Allocator, exprs: []Self) Allocator.Error![]Self {
    const cloned = try ally.alloc(Self, exprs.len);
    for (exprs) |child, i| {
        cloned[i] = try child.clone(ally);
    }

    return cloned;
}

pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
    const data = switch (self.data) {
        // zig fmt: off
        .unit, .ty, .@"bool", .number, .name, .builtin, .param, .func_ref
        // zig fmt: on
        => self.data,
        .ptr => |child| Data{
            .ptr = try com.placeOn(ally, try child.clone(ally)),
        },
        .func => |func| Data{ .func = .{
            .name = func.name,
            .body = try com.placeOn(ally, try func.body.clone(ally)),
        } },
        .string => |sym| Data{ .string = try sym.clone(ally) },
        .call => |children| Data{ .call = try cloneChildren(ally, children) },
        .array => |children| Data{ .array = try cloneChildren(ally, children) },
        .slice => |children| Data{ .slice = try cloneChildren(ally, children) },
    };

    return Self{
        .data = data,
        .loc = self.loc,
        .ty = self.ty,
        .known_const = self.known_const,
    };
}

pub fn eql(self: Self, other: Self) bool {
    return self.ty.eql(other.ty) and self.data.eql(other.data);
}

/// gets children of any TExpr if they exist.
///
/// useful for recursive operations on TExprs.
pub fn getChildren(self: Self) []Self {
    return switch (self.data) {
        .call, .array, .slice => |xs| xs,
        .ptr => |child| @ptrCast(*[1]Self, child),
        .func => |func| @ptrCast(*[1]Self, func.body),
        else => &.{},
    };
}

pub fn isBuiltin(self: Self, tag: Builtin) bool {
    return self.data == .builtin and self.data.builtin == tag;
}

pub fn render(
    self: Self,
    ctx: *kz.Context,
    env: Env,
) Allocator.Error!kz.Ref {
    const INDENT = 2;
    const magenta = kz.Style{ .fg = .magenta };
    const green = kz.Style{ .fg = .green };
    const red = kz.Style{ .fg = .red };

    // inline header stuff
    const data = switch (self.data) {
        .call,
        .array,
        .slice,
        .ptr,
        .func,
        => try ctx.print(.{}, "{s}", .{@tagName(self.data)}),
        .unit => try ctx.print(.{}, "()", .{}),
        .func_ref => |fr| try ctx.slap(
            try ctx.print(.{}, "&func ", .{}),
            try ctx.print(red, "{}", .{env.getFuncConst(fr).name}),
            .right,
            .{},
        ),
        .param => |p| try ctx.print(
            .{},
            "parameter {d} of function {}",
            .{ p.index, p.func },
        ),
        .ty => |ty| try ty.render(ctx, env.tw),
        .@"bool" => |val| try ctx.print(magenta, "{}", .{val}),
        .number => |num| try ctx.print(magenta, "{}", .{num}),
        .string => |str| try ctx.print(green, "\"{}\"", .{str}),
        .name => |name| try ctx.print(red, "{}", .{name}),
        .builtin => |b| try ctx.print(red, "{s}", .{@tagName(b)}),
    };

    // header
    const ty_tex = try ctx.stack(&.{
        try ctx.print(.{}, "<", .{}),
        try self.ty.render(ctx, env.tw),
        try ctx.print(.{}, ">", .{}),
    }, .right, .{});

    const header = try ctx.slap(ty_tex, data, .right, .{ .space = 1 });

    // any children
    var children = try ctx.stub();
    for (self.getChildren()) |child| {
        const tex = try child.render(ctx, env);
        children = try ctx.slap(children, tex, .bottom, .{});
    }

    // slap and return everything
    const offset = kz.Offset{ INDENT, @intCast(isize, ctx.getSize(header)[1]) };
    return try ctx.unify(header, children, offset);
}
