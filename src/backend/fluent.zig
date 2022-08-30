//! contains the essential representations of Fluent data

const std = @import("std");
const util = @import("../util/util.zig");
const frontend = @import("../frontend.zig");
const FlFile = @import("../file.zig");
const Env = @import("env.zig");

const Allocator = std.mem.Allocator;
const Context = FlFile.Context;
const Expr = frontend.Expr;

// TODO remove, debugging only
const Color = @import("kritzler").Color;
const stdout = std.io.getStdOut().writer();

/// flat type representation
pub const Type = enum {
    const Self = @This();

    comptime {
        std.debug.assert(@enumToInt(@This().nil) == 0);
    }

    // axiomatic
    nil,
    undef,

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
/// expects to own everything it references. no cycles, no shared subtrees
pub const SType = union(Type) {
    const Self = @This();

    nil,
    undef,
    symbol,
    int,
    stype,

    list: *SType, // contains subtype
    tuple: []SType, // contains ordered subtypes
    func: struct {
        params: []SType,
        returns: *SType,
    },

    pub fn init_list(
        ally: Allocator,
        subtype: SType
    ) Allocator.Error!Self{
        return Self{ .list = try util.place_on(ally, try subtype.clone(ally)) };
    }

    pub fn init_tuple(
        ally: Allocator,
        children: []const SType
    ) Allocator.Error!Self{
        return Self{ .tuple = try clone_slice(ally, children) };
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
            .tuple => |tuple| try Self.init_tuple(ally, tuple),
            .func => |func|
                try Self.init_func(ally, func.params, func.returns.*),
            else => self
        };
    }

    /// checks if SType tags match
    /// helper for `matches` and some other type inference related functions
    fn flat_matches(self: Self, template: Self) bool {
        return template == .undef or @as(Type, self) == @as(Type, template);
    }

    /// a unidirectional operation to determine if this type matches the
    /// provided template
    pub fn matches(self: Self, template: Self) bool {
        return self.flat_matches(template) and switch (self) {
            .list => |subtype| subtype.matches(template.list.*),
            .tuple => |children| blk: {
                for (children) |child, i| {
                    if (!child.matches(template.tuple[i])) break :blk false;
                }

                break :blk true;
            },
            .func => |func| blk: {
                for (func.params) |param, i| {
                    if (!param.matches(template.func.params[i])) {
                        break :blk false;
                    }
                }

                break :blk func.returns.matches(template.func.returns.*);
            },
            else => true
        };
    }

    fn format_r(self: Self, writer: anytype) @TypeOf(writer).Error!void {
        switch (self) {
            .stype => try writer.writeAll("type"),
            .list => |subtype| try writer.print("(list {})", .{subtype}),
            .tuple => |tuple| {
                try util.write_join("(tuple ", " ", ")", tuple, writer);
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

/// structured expression
/// expects to own everything it references. no cycles, no shared subtrees
pub const SExpr = union(Type) {
    const Self = @This();

    nil,
    undef,

    // TODO precompute a hash? store strings only once (serenity's FlyString)?
    // symbols are trivially optimizable tbh
    symbol: []const u8,
    int: i64,
    stype: SType,

    list: []Self, // uniform type
    tuple: []Self, // varied types
    func: struct {
        params: []const []const u8, // an array of symbols for parameters
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

                    const symbols = param_expr.children.?;
                    const params = try ctx.ally.alloc([]const u8, symbols.len);
                    for (symbols) |symbol, i| {
                        if (symbol.etype != .ident) {
                            return TranslationError.BadLambdaArgs;
                        }

                        params[i] = try ctx.ally.dupe(u8, symbol.slice);
                    }

                    break :from_call Self{
                        .func = .{
                            .params = params,
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
            .func => |func| {
                for (func.params) |param| ally.free(param);
                ally.free(func.params);
                func.body.deinit(ally);
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
            .tuple => |tuple| Self{ .tuple = try clone_slice(ally, tuple) },
            else => self
        };
    }

    pub const TypingError = error {
        ExpectationFailed,
        UninferrableType,
        EmptyFunctionCall,
        UnknownSymbol,
    };

    /// type inference. tries to be as bidirectional as possible, but you can
    /// escape this by passing in `SType{ .undef = {} }` as expectation.
    /// TODO nicer errors
    pub fn infer_type(
        self: Self,
        ally: Allocator,
        env: Env,
        expects: SType
    ) anyerror!SType {
        // check the flat type
        switch (self) {
            .symbol, .tuple => {},
            else => {
                if (expects != .undef
                and @as(Type, self) != @as(Type, expects)) {
                    return TypingError.ExpectationFailed;
                }
            }
        }

        const inferred = switch (self) {
            .nil => SType{ .nil = {} },
            .undef => unreachable,
            .symbol => |sym| blk: {
                const bound_stype = env.get_type(sym) orelse {
                    return TypingError.UnknownSymbol;
                };

                if (!bound_stype.matches(expects)) {
                    return TypingError.ExpectationFailed;
                }

                break :blk try bound_stype.clone(ally);
            },
            .int => SType{ .int = {} },
            .stype => SType{ .stype = {} },
            .list => |list| infer: {
                const subtype =
                    if (expects != .undef)
                        if (list.len > 0)
                            try list[0].infer_type(ally, env, expects.list.*)
                        else
                            try expects.list.clone(ally)
                    else if (list.len > 0)
                        try list[0].infer_type(ally, env, SType{ .undef = {} })
                    else
                        SType{ .undef = {} };

                if (list.len > 1) {
                    // type the rest of the list
                    for (list[1..]) |child| {
                        _ = try child.infer_type(ally, env, subtype);
                    }
                }

                break :infer SType{ .list = try util.place_on(ally, subtype) };
            },
            .tuple => |tuple| blk: {
                if (tuple.len == 0) return TypingError.EmptyFunctionCall;

                // most of the function type can be inferred from context
                const takes = try ally.alloc(SType, tuple.len - 1);
                defer ally.free(takes);

                for (tuple[1..]) |param, i| {
                    takes[i] = try param.infer_type(
                        ally,
                        env,
                        SType{ .undef = {}}
                    );
                }

                const call_expects = SType{
                    .func = .{
                        .params = takes,
                        .returns = try util.place_on(ally, expects)
                    }
                };

                // use inferred expectations to find the return type
                const called_type = try tuple[0].infer_type(
                    ally,
                    env,
                    call_expects
                );

                break :blk try called_type.func.returns.clone(ally);
            },
            .func => |func| blk: {
                // function literals require expectations to infer their
                // type
                if (expects == .undef) {
                    return TypingError.UninferrableType;
                }

                // return type can be fully checked + inferred from the body
                // expression
                var fun_env = try Env.init(ally, &env);
                defer fun_env.deinit();

                // define parameters in subenv
                for (func.params) |param, i| {
                    try fun_env.define_virtual(param, expects.func.params[i]);
                }

                // returns tolerates an `undef` return expectation
                const returns = try util.place_on(
                    ally,
                    try func.body.infer_type(
                        ally,
                        fun_env,
                        expects.func.returns.*
                    )
                );

                break :blk SType{
                    .func = .{
                        .params = expects.func.params,
                        .returns = returns
                    }
                };
            }
        };

        return inferred;
    }

    fn format_r(self: Self, writer: anytype) @TypeOf(writer).Error!void {
        switch (self) {
            .nil => try writer.writeAll("nil"),
            .undef => try writer.writeAll("undef"),
            .symbol => |sym| try writer.writeAll(sym),
            .int => |n| try writer.print("{d}", .{n}),
            .stype => |stype| try writer.print("<{}>", .{stype}),
            .list => |list| try util.write_join("[", " ", "]", list, writer),
            .tuple => |tuple| try util.write_join("(", " ", ")", tuple, writer),
            .func => |func| {
                try writer.writeAll("(lambda [");
                for (func.params) |param, i| {
                    if (i > 0) try writer.writeAll(" ");
                    try writer.writeAll(param);
                }
                try writer.print("] {})", .{func.body});
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