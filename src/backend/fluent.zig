//! contains the essential representations of Fluent data

const std = @import("std");
const util = @import("../util/util.zig");
const frontend = @import("../frontend.zig");
const FlFile = @import("../file.zig");
const Env = @import("env.zig");

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

    // simple
    symbol,
    int,
    stype,

    // structured
    list,
    tuple,
};

/// structured type representation
/// expects to own all children
pub const SType = union(Type) {
    const Self = @This();

    nil,
    symbol,
    int,
    stype,

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
            .stype => try writer.writeAll("type"),
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

    // TODO precompute a hash? store strings only once (serenity's FlyString)?
    // many possible optimizations tbh
    symbol: []const u8,
    int: i64,
    stype: SType,

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

    /// helper for clone
    fn clone_children(
        ally: Allocator,
        children: []const Self
    ) Allocator.Error![]Self {
        const cloned = try ally.alloc(Self, children.len);
        for (children) |child, i| cloned[i] = try child.clone(ally);

        return cloned;
    }

    pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
        return switch(self) {
            .symbol => |sym| Self{ .symbol = try ally.dupe(u8, sym) },
            .list => |list| Self{ .list = try clone_children(ally, list) },
            .tuple => |tuple| Self{ .tuple = try clone_children(ally, tuple) },
            else => self
        };
    }

    pub const TypingError = error {
        ExpectationFailed,
        UninferrableType,
    };

    /// type inference. unidirectional by default, bidirectional when an
    /// expectation is provided.
    /// TODO nicer errors
    pub fn infer_type(
        self: Self,
        ally: Allocator,
        env: Env,
        maybe_expects: ?SType
    ) (Allocator.Error || TypingError)!SType {
        // ensure the flat type is the same
        if (maybe_expects) |expects| {
            if (self != .symbol and @as(Type, self) != @as(Type, expects)) {
                return TypingError.ExpectationFailed;
            }
        }

        return switch (self) {
            .nil => SType{ .nil = {} },
            .symbol => |sym| blk: {
                if (env.get(sym)) |value| {
                    // TODO cache this calculation maybe?
                    break :blk value.infer_type(ally, env, maybe_expects);
                } else if (maybe_expects) |expects| {
                    break :blk try expects.clone(ally);
                } else {
                    break :blk TypingError.UninferrableType;
                }
            },
            .int => SType{ .int = {} },
            .stype => SType{ .stype = {} },
            .list => |list| blk: {
                const subtype = find_subtype: {
                    if (list.len > 0) {
                        break :find_subtype try list[0].infer_type(
                            ally,
                            env,
                            null
                        );
                    } else if (maybe_expects) |expects| {
                        break :find_subtype try expects.list.clone(ally);
                    } else {
                        break :blk TypingError.UninferrableType;
                    }
                };

                break :blk SType{ .list = try util.place_on(ally, subtype) };
            },
            .tuple => |tuple| blk: {
                const subtypes = try ally.alloc(SType, tuple.len);
                for (tuple) |child, i| {
                    subtypes[i] = try child.infer_type(ally, env, null);
                }

                break :blk SType{ .tuple = subtypes };
            }
        };
    }

    fn format_r(self: Self, writer: anytype) @TypeOf(writer).Error!void {
        switch (self) {
            .nil => try writer.writeAll("nil"),
            .symbol => |sym| try writer.writeAll(sym),
            .int => |n| try writer.print("{d}", .{n}),
            .stype => |stype| try writer.print("<{}>", .{stype}),
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