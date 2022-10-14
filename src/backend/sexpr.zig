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

pub const Tag = std.meta.Tag(Data);

const Data = union(enum) {
    int: i64,
    uint: u64,
    float: f64,
    string: Symbol,
    symbol: Symbol,
    call: []Self,

    fn FieldTypeOf(comptime tag: Tag) type {
        return std.meta.fieldInfo(Data, tag).field_type;
    }
};

data: Data,
loc: context.Loc,

fn initFn(comptime tag: Tag) fn(Loc, Data.FieldTypeOf(tag)) Self {
    const Closure = struct {
        fn initFn(loc: Loc, data: Data.FieldTypeOf(tag)) Self {
            return Self{
                .data = @unionInit(Data, @tagName(tag), data),
                .loc = loc
            };
        }
    };

    return Closure.initFn;
}

pub const initInt = initFn(.int);
pub const initUInt = initFn(.uint);
pub const initFloat = initFn(.float);

/// expects input to be already owned
pub const initCall = initFn(.call);

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

pub fn format(
    self: Self,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    _ = fmt;
    _ = options;

    switch (self.data) {
        .int => |d| try writer.print("{}", .{d}),
        .uint => |d| try writer.print("{}", .{d}),
        .float => |f| try writer.print("{}", .{f}),
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