//! contains the essential representations of Fluent data

const std = @import("std");
const util = @import("../util/util.zig");
const frontend = @import("../frontend.zig");
const FlFile = @import("../file.zig");

const Allocator = std.mem.Allocator;
const Context = FlFile.Context;
const Expr = frontend.Expr;

/// flat type representation
pub const Type = enum {
    comptime {
        std.debug.assert(@enumToInt(@This().nil) == 0);
    }

    // axiomatic
    nil,
    unknown,

    // simple
    symbol,
    int,

    // structured
    list,
    tuple,
};

/// structured type representation
/// expects to own all children
pub const SType = union(Type) {
    const Self = @This();

    nil,
    unknown,
    symbol,
    int,

    list: *SType, // contains subtype
    tuple: []SType, // contains ordered subtypes

    pub fn deinit(self: Self, ally: Allocator) void {
        switch (self) {
            .list => |list| {
                list.deinit(ally);
                ally.destroy(list);
            },
            .tuple => |tuple| {
                for (tuple) |child| child.deinit(ally);
                ally.free(tuple);
            },
            else => {}
        }
    }

    pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
        return switch (self) {
            .list => |subtype| Self{
                .list = try util.place_on(ally, try subtype.clone(ally))
            },
            .tuple => |tuple| blk: {
                const subtypes = try ally.alloc(Self, tuple.len);
                for (tuple) |subtype, i| subtypes[i] = try subtype.clone(ally);

                break :blk Self{ .tuple = subtypes };
            },
            else => self
        };
    }

    fn format_r(self: Self, writer: anytype) @TypeOf(writer).Error!void {
        switch (self) {
            .list => |subtype| try writer.print("(list {})", .{subtype}),
            .tuple => |tuple| {
                try util.write_join("(tuple ", " ", ")", tuple, writer);
            },
            else => |t| try writer.writeAll(@tagName(t)),
        }
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        _ = fmt;
        _ = options;

        try self.format_r(writer);
    }
};

/// structured expression
/// expects to own all children
pub const SExpr = union(Type) {
    const Self = @This();

    nil,
    unknown, // should only appear in typing

    symbol: []const u8, // TODO precompute a hash?
    int: i64,

    list: []Self, // uniform type
    tuple: []Self, // varied types

    pub const TranslationError = Allocator.Error || std.fmt.ParseIntError;

    /// helper for from_expr
    fn from_children(
        ctx: *Context,
        children: []const Expr
    ) TranslationError![]Self {
        const translated = try ctx.ally.alloc(Self, children.len);
        for (children) |child, i| translated[i] = try from_expr(ctx, child);

        return translated;
    }

    /// translates the raw ast into an SExpr
    pub fn from_expr(ctx: *Context, expr: Expr) TranslationError!Self {
        return switch (expr.etype) {
            .nil => Self{ .nil = {} },
            .ident => Self{ .symbol = try ctx.ally.dupe(u8, expr.slice) },
            .int => Self{ .int = try std.fmt.parseInt(i64, expr.slice, 0) },
            .list => Self{ .list = try from_children(ctx, expr.children.?) },
            .call => Self{ .tuple = try from_children(ctx, expr.children.?) },
            else => std.debug.panic("TODO: translate {s}\n", .{expr.etype})
        };
    }

    pub fn deinit(self: Self, ally: Allocator) void {
        switch (self) {
            .symbol => |sym| ally.free(sym),
            .list, .tuple => |children| {
                for (children) |child| child.deinit(ally);
                ally.free(children);
            },
            else => {}
        }
    }

    pub const TypeInferError = error {
        ExpectationFailed,
    };

    /// infers type based on bottom up and top down indicators. since these may
    /// be in conflict, this performs some type checking, but there is nothing
    /// preventing this from producing potentially incomplete types
    /// TODO nicer errors
    pub fn infer_type(
        self: Self,
        ally: Allocator,
        maybe_expects: ?SType
    ) (Allocator.Error || TypeInferError)!SType {
        if (maybe_expects) |expects| {
            // bidirectional inference
            if (self == .unknown) {
                return try expects.clone(ally);
            } else if (@as(Type, self) != @as(Type, expects)) {
                return error.ExpectationFailed;
            } else {
                return switch (self) {
                    .list => |list| blk: {
                        const subtype =
                            if (list.len == 0)
                                try expects.list.clone(ally)
                            else
                                try self.list[0].infer_type(
                                    ally,
                                    expects.list.*
                                );

                        break :blk SType{
                            .list = try util.place_on(ally, subtype)
                        };
                    },
                    .tuple => |tuple| blk: {
                        const subtypes = try ally.alloc(SType, tuple.len);
                        for (tuple) |child, i| {
                            subtypes[i] = try child.infer_type(
                                ally,
                                expects.tuple[i]
                            );
                        }

                        break :blk SType{ .tuple = subtypes };
                    },
                    .unknown => unreachable,
                    else => try expects.clone(ally)
                };
            }
        } else {
            // unidirectional inference
            return switch (self) {
                .nil => SType{ .nil = {} },
                .unknown => SType{ .unknown = {} },
                .symbol => SType{ .symbol = {} },
                .int => SType{ .int = {} },
                .list => |list| blk: {
                    const subtype =
                        if (list.len == 0) SType{ .unknown = {} }
                        else try list[0].infer_type(ally, null);

                    break :blk SType{ .list = try util.place_on(ally, subtype) };
                },
                .tuple => |tuple| blk: {
                    const subtypes = try ally.alloc(SType, tuple.len);
                    for (tuple) |child, i| {
                        subtypes[i] = try child.infer_type(ally, null);
                    }

                    break :blk SType{ .tuple = subtypes };
                }
            };
        }
    }

    fn format_r(self: Self, writer: anytype) @TypeOf(writer).Error!void {
        switch (self) {
            .nil => try writer.writeAll("nil"),
            .unknown => try writer.writeAll("unknown"),
            .symbol => |sym| try writer.writeAll(sym),
            .int => |n| try writer.print("{d}", .{n}),
            .list => |list| try util.write_join("[", " ", "]", list, writer),
            .tuple => |tuple| try util.write_join("(", " ", ")", tuple, writer),
        }
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        _ = fmt;
        _ = options;

        try self.format_r(writer);
    }
};