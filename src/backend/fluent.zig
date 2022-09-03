//! contains the essential representations of Fluent data
//! most meaty function here is `SExpr.from_expr` which translates the frontend
//! AST into an SExpr tree.

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
    const Self = @This();

    // axiomatic
    unit,
    undef,

    // simple
    symbol,
    int,
    stype,

    // structured
    list,
    call,
    func,
    def,
};

/// structured type representation
/// expects to own everything it references. no cycles, no shared subtrees
pub const SType = union(Type) {
    const Self = @This();

    unit,
    // TODO am I reinventing null? maybe instead of this use a separate
    // TypePattern struct?
    undef,

    symbol,
    int,
    stype,

    list: *SType,
    call: []SType,
    func: struct {
        params: []SType,
        returns: *SType,
    },
    def,

    pub fn init_list(
        ally: Allocator,
        subtype: SType
    ) Allocator.Error!Self{
        return Self{ .list = try util.place_on(ally, try subtype.clone(ally)) };
    }

    pub fn init_call(
        ally: Allocator,
        children: []const SType
    ) Allocator.Error!Self{
        return Self{ .call = try clone_slice(ally, children) };
    }

    pub fn init_func(
        ally: Allocator,
        params: []const SType,
        returns: SType
    ) Allocator.Error!Self{
        return Self{
            .func = .{
                .params = try clone_slice(ally, params),
                .returns = try util.place_on(ally, try returns.clone(ally))
            }
        };
    }

    pub fn deinit(self: Self, ally: Allocator) void {
        switch (self) {
            .list => |list| {
                list.deinit(ally);
                ally.destroy(list);
            },
            .call => |call| {
                for (call) |child| child.deinit(ally);
                ally.free(call);
            },
            .func => |func| {
                for (func.params) |param| param.deinit(ally);
                ally.free(func.params);

                func.returns.deinit(ally);
            },
            else => {}
        }
    }

    pub fn clone_slice(
        ally: Allocator,
        slice: []const Self
    ) Allocator.Error![]Self {
        const copied = try ally.alloc(Self, slice.len);
        for (slice) |child, i| copied[i] = try child.clone(ally);

        return copied;
    }

    pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
        return switch (self) {
            .list => |subtype| try Self.init_list(ally, subtype.*),
            .call => |call| try Self.init_call(ally, call),
            .func => |func|
                try Self.init_func(ally, func.params, func.returns.*),
            else => self
        };
    }

    /// checks if SType tags match
    /// helper for `matches` and some other type inference related functions
    fn flat_matches(self: Self, pattern: Self) bool {
        return pattern == .undef or @as(Type, self) == @as(Type, pattern);
    }

    /// a unidirectional operation to determine if this type matches the
    /// provided template
    pub fn matches(self: Self, pattern: Self) bool {
        return pattern == .undef
            or @as(Type, self) == @as(Type, pattern) and switch (self) {
            .list => |subtype| subtype.matches(pattern.list.*),
            .call => |children| blk: {
                if (children.len != pattern.call.len) break :blk false;

                for (children) |child, i| {
                    if (!child.matches(pattern.call[i])) break :blk false;
                }

                break :blk true;
            },
            .func => |func| blk: {
                if (func.params.len != pattern.func.params.len) {
                    break :blk false;
                }

                for (func.params) |param, i| {
                    if (!param.matches(pattern.func.params[i])) {
                        break :blk false;
                    }
                }

                break :blk func.returns.matches(pattern.func.returns.*);
            },
            else => true
        };
    }

    // TODO format types with PascalCase
    fn format_r(self: Self, writer: anytype) @TypeOf(writer).Error!void {
        switch (self) {
            .stype => try writer.writeAll("type"),
            .list => |subtype| try writer.print("(list {})", .{subtype}),
            .call => |call| {
                try util.write_join("(call ", " ", ")", call, writer);
            },
            .func => |func| {
                try util.write_join("(fn [", " ", "]", func.params, writer);
                try writer.print(" {})", .{func.returns});
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

/// structured expression, the canonical representation of Fluent values
pub const SExpr = union(Type) {
    const Self = @This();

    unit,
    undef,

    // TODO precompute a hash? store strings only once (serenity's FlyString)?
    // symbols are trivially optimizable tbh
    symbol: []const u8,
    int: i64,
    stype: SType,

    list: []Self, // uniform type
    call: []Self, // varied types
    func: struct {
        params: []const []const u8, // an array of symbols for parameters
        body: *Self,
    },
    def: struct {
        symbol: []const u8,
        anno: *Self,
        body: *Self,
    },

    pub const TranslationError =
        Allocator.Error
     || std.fmt.ParseIntError
     || error {
        BadLambda,
        BadDef,
    };

    /// helper for from_expr()
    fn from_children(
        ctx: *Context,
        children: []const Expr
    ) TranslationError![]Self {
        const translated = try ctx.ally.alloc(Self, children.len);
        for (children) |child, i| translated[i] = try from_expr(ctx, child);

        return translated;
    }

    /// helper for from_expr()
    fn from_lambda(ctx: *Context, children: []Expr) TranslationError!Self {
        if (children.len != 3) return TranslationError.BadLambda;

        const param_expr = children[1];
        if (param_expr.etype != .list) return TranslationError.BadLambda;

        const symbols = param_expr.children.?;
        const params = try ctx.ally.alloc([]const u8, symbols.len);
        for (symbols) |symbol, i| {
            if (symbol.etype != .ident) return TranslationError.BadLambda;

            params[i] = try ctx.ally.dupe(u8, symbol.slice);
        }

        return Self{
            .func = .{
                .params = params,
                .body = try util.place_on(
                    ctx.ally,
                    try from_expr(ctx, children[2])
                )
            }
        };
    }

    /// helper for from_expr()
    fn from_def(ctx: *Context, children: []Expr) TranslationError!Self {
        if (children.len != 4) return TranslationError.BadDef;

        if (children[1].etype != .ident) return TranslationError.BadDef;

        return Self{
            .def = .{
                .symbol = try ctx.ally.dupe(u8, children[1].slice),
                .anno = try util.place_on(
                    ctx.ally,
                    try from_expr(ctx, children[2])
                ),
                .body = try util.place_on(
                    ctx.ally,
                    try from_expr(ctx, children[3])
                ),
            }
        };
    }

    /// translates the raw ast into an SExpr
    pub fn from_expr(ctx: *Context, expr: Expr) TranslationError!Self {
        return switch (expr.etype) {
            .ident => Self{ .symbol = try ctx.ally.dupe(u8, expr.slice) },
            .int => Self{ .int = try std.fmt.parseInt(i64, expr.slice, 0) },
            .list => Self{ .list = try from_children(ctx, expr.children.?) },
            .call => from_call: {
                const children = expr.children.?;
                if (children[0].is_ident("lambda")) {
                    break :from_call Self.from_lambda(ctx, children);
                } else if (children[0].is_ident("def")) {
                    break :from_call Self.from_def(ctx, children);
                } else {
                    break :from_call Self{
                        .call = try from_children(ctx, children)
                    };
                }
            },
            else => std.debug.panic("TODO: translate {s}\n", .{expr.etype})
        };
    }

    pub fn deinit(self: Self, ally: Allocator) void {
        switch (self) {
            .symbol => |sym| ally.free(sym),
            .list, .call => |children| {
                for (children) |child| child.deinit(ally);
                ally.free(children);
            },
            .func => |func| {
                for (func.params) |param| ally.free(param);
                ally.free(func.params);
                func.body.deinit(ally);
            },
            .def => |def| {
                def.anno.deinit(ally);
                def.body.deinit(ally);
            },
            else => {}
        }
    }

    /// helper for clone
    pub fn clone_slice(
        ally: Allocator,
        slice: []const Self
    ) Allocator.Error![]Self {
        const cloned = try ally.alloc(Self, slice.len);
        for (slice) |child, i| cloned[i] = try child.clone(ally);

        return cloned;
    }

    pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
        return switch(self) {
            .symbol => |sym| Self{ .symbol = try ally.dupe(u8, sym) },
            .list => |list| Self{ .list = try clone_slice(ally, list) },
            .call => |call| Self{ .call = try clone_slice(ally, call) },
            else => self
        };
    }

    fn format_r(self: Self, writer: anytype) @TypeOf(writer).Error!void {
        switch (self) {
            .unit, .undef => try writer.print("{s}", .{@tagName(self)}),
            .symbol => |sym| try writer.writeAll(sym),
            .int => |n| try writer.print("{d}", .{n}),
            .stype => |stype| try writer.print("{}", .{stype}),
            .list => |list| try util.write_join("[", " ", "]", list, writer),
            .call => |call| try util.write_join("(", " ", ")", call, writer),
            .func => |func| {
                try writer.writeAll("(lambda [");
                for (func.params) |param, i| {
                    if (i > 0) try writer.writeAll(" ");
                    try writer.writeAll(param);
                }
                try writer.print("] {})", .{func.body});
            },
            .def => |def| {
                try writer.print("(def {[symbol]s} {[anno]} {[body]})", def);
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