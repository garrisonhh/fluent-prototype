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
    func,
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
    func: struct {
        params: []SType,
        returns: *SType,
    },

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
            .func => |func| {
                for (func.params) |param| param.deinit(ally);
                ally.free(func.params);

                func.returns.deinit(ally);
            },
            else => {}
        }
    }

    fn clone_children(
        ally: Allocator,
        children: []Self
    ) Allocator.Error![]Self {
        const copied = try ally.alloc(Self, children.len);
        for (children) |child, i| copied[i] = try child.clone(ally);

        return copied;
    }

    pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
        return switch (self) {
            .list => |subtype| Self{
                .list = try util.place_on(ally, try subtype.clone(ally))
            },
            .tuple => |tuple| Self{ .tuple = try clone_children(ally, tuple) },
            .func => |func| Self{
                .func = .{
                    .params = try clone_children(ally, func.params),
                    .returns = try util.place_on(
                        ally,
                        try func.returns.clone(ally)
                    )
                }
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
    // symbols are trivially optimizable tbh
    symbol: []const u8,
    int: i64,
    stype: SType,

    list: []Self, // uniform type
    tuple: []Self, // varied types
    func: struct {
        params: []Self, // should be an array of symbols
        body: *Self,
    },

    pub const TranslationError =
        Allocator.Error
     || std.fmt.ParseIntError
     || error {
        BadLambdaArgs,
    };

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
            .call => from_call: {
                const children = expr.children.?;

                // TODO find a better way to formalize builtins like this. I
                // mean maybe the truth is that builtins are called 'builtins'
                // because there isn't another way to do this, but I seriously
                // doubt that. in this particular case I think tokenizing
                // 'lambda' as its own token type (since it is syntax and not
                // a function, truthfully) might turn out to be an elegant
                // solution
                if (children[0].is_ident("lambda")) {
                    if (children.len != 3) {
                        return TranslationError.BadLambdaArgs;
                    }

                    const param_expr = children[1];
                    if (param_expr.etype != .list) {
                        return TranslationError.BadLambdaArgs;
                    }

                    break :from_call Self{
                        .func = .{
                            .params = try from_children(
                                ctx,
                                param_expr.children.?
                            ),
                            .body = try util.place_on(
                                ctx.ally,
                                try from_expr(ctx, children[2])
                            )
                        }
                    };
                } else {
                    break :from_call Self{
                        .tuple = try from_children(ctx, children)
                    };
                }
            },
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
        maybe_expects: ?*const SType
    ) (Allocator.Error || TypingError)!SType {
        // ensure the flat type is the same
        if (maybe_expects) |expects| {
            if (self != .symbol and @as(Type, self) != @as(Type, expects.*)) {
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
                errdefer ally.free(subtypes);

                if (maybe_expects) |expects| {
                    if (expects.* != .tuple
                     or expects.tuple.len != tuple.len) {
                        return TypingError.ExpectationFailed;
                    }

                    for (tuple) |child, i| {
                        subtypes[i] = try child.infer_type(
                            ally,
                            env,
                            &expects.tuple[i]
                        );
                    }
                } else {
                    for (tuple) |child, i| {
                        subtypes[i] = try child.infer_type(ally, env, null);
                    }
                }

                break :blk SType{ .tuple = subtypes };
            },
            .func => |func| blk: {
                if (maybe_expects == null) {
                    // function literals require expectations to infer their
                    // type (at least at the moment, eventually I think I should
                    // be able to infer the type of parameters based on how they
                    // are used within the function body)
                    return TypingError.UninferrableType;
                }

                const expects = maybe_expects.?;

                if (func.params.len != func.params.len) {
                    return TypingError.ExpectationFailed;
                }

                const takes = try ally.alloc(SType, func.params.len);
                for (func.params) |param, i| {
                    takes[i] = try param.infer_type(
                        ally,
                        env,
                        &expects.func.params[i]
                    );
                }

                const returns = try util.place_on(
                    ally,
                    try func.body.infer_type(ally, env, expects.func.returns)
                );

                break :blk SType{
                    .func = .{
                        .params = takes,
                        .returns = returns
                    }
                };
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
            .func => |func| {
                try writer.writeAll("(lambda ");
                try util.write_join("[", " ", "]", func.params, writer);
                try writer.print(" {})", .{func.body});
            },
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