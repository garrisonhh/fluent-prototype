//! semantic analysis in the Fluent compiler is the process of performing static
//! type analysis on a raw SExpr tree to produce TypedExprs for IR generation

const std = @import("std");
const kz = @import("kritzler");
const util = @import("../util/util.zig");
const fluent = @import("fluent.zig");
const Env = @import("env.zig");

const Allocator = std.mem.Allocator;
const FlatType = fluent.Type;
const SType = fluent.SType;
const SExpr = fluent.SExpr;
const stdout = std.io.getStdOut().writer();

pub const TypingError = error {
    ExpectationFailed,
    UninferrableType,
    
    // symbol lookup
    UnknownSymbol,

    // calls
    CalledNothing,
    CalledNonFunction,
    
    // funcs
    FuncWithoutExpectation,
};

pub const Error = TypingError || Allocator.Error;

/// see `from_sexpr()`
///
/// maps 1-to-1 to SExpr in terms of structure, but contains extra type
/// information for a more complete AST
///
/// TODO I should just translate ast Exprs straight to this, make SExpr more
/// of a pure value type
pub const TypedExpr = union(FlatType) {
    const Self = @This();

    pub const TypedSymbol = struct {
        stype: SType,
        symbol: []const u8,

        fn clone(
            self: TypedSymbol,
            ally: Allocator
        ) Allocator.Error!TypedSymbol {
            return TypedSymbol{
                .stype = try self.stype.clone(ally),
                .symbol = try ally.dupe(u8, self.symbol)
            };
        }
    };

    pub const List = struct {
        subtype: SType,
        exprs: []Self,

        fn clone(self: List, ally: Allocator) Allocator.Error!List {
            const copied = try ally.alloc(Self, self.exprs.len);
            for (self.exprs) |expr, i| copied[i] = try expr.clone(ally);

            return List{
                .subtype = try self.subtype.clone(ally),
                .exprs = copied,
            };
        }
    };

    pub const Call = struct {
        returns: SType,
        exprs: []Self,

        fn clone(self: Call, ally: Allocator) Allocator.Error!Call {
            const copied = try ally.alloc(Self, self.exprs.len);
            for (self.exprs) |param, i| copied[i] = try param.clone(ally);

            return Call{
                .returns = try self.returns.clone(ally),
                .exprs = copied,
            };
        }
    };

    pub const Func = struct {
        params: []TypedSymbol,
        body: *Self,
    };

    pub const Def = struct {
        symbol: []const u8,
        anno: *Self,
        body: *SExpr,
    };

    unit,
    undef,

    symbol: TypedSymbol,
    int: i64,
    stype: SType,
    ptr: *Self,

    list: List,
    call: Call,
    func: Func,
    def: Def,

    pub fn deinit(self: Self, ally: Allocator) void {
        switch (self) {
            .symbol => |sym| {
                sym.stype.deinit(ally);
                ally.free(sym.symbol);
            },
            .list => |list| {
                list.subtype.deinit(ally);
                for (list.exprs) |expr| expr.deinit(ally);
                ally.free(list.exprs);
            },
            .call => |call| {
                call.returns.deinit(ally);
                for (call.exprs) |expr| expr.deinit(ally);
                ally.free(call.exprs);
            },
            .func => |func| {
                for (func.params) |sym| {
                    sym.stype.deinit(ally);
                    ally.free(sym.symbol);
                }
                ally.free(func.params);

                func.body.deinit(ally);
                ally.destroy(func.body);
            },
            .def => |def| {
                ally.free(def.symbol);
                def.anno.deinit(ally);
                ally.destroy(def.anno);
                def.body.deinit(ally);
                ally.destroy(def.body);
            },
            else => {}
        }
    }

    /// bidirectional type checking and inference. returns a TypedExpr tree on the
    /// ally. does not allow for conflicting information.
    ///
    /// *call externally through `analyze()`
    /// TODO nicer errors
    fn from_sexpr(
        ally: Allocator,
        env: Env,
        expr: SExpr,
        expects: SType
    ) anyerror!TypedExpr {
        // check the flat type
        switch (expr) {
            .symbol, .call => {},
            else => {
                if (expects != .undef
                and @as(FlatType, expr) != @as(FlatType, expects)) {
                    return TypingError.ExpectationFailed;
                }
            }
        }

        return switch (expr) {
            .undef => unreachable,
            .int => |n| Self{ .int = n },
            .stype => |t| Self{ .stype = try t.clone(ally) },
            .symbol => |sym| blk: {
                const bound_stype = env.get_type(sym) orelse {
                    std.debug.print("unknown symbol `{s}`\n", .{sym});
                    return TypingError.UnknownSymbol;
                };

                if (!bound_stype.matches(expects)) {
                    std.debug.print(
                        "`{s}` is {}, expected {}\n",
                        .{sym, bound_stype, expects}
                    );

                    return TypingError.ExpectationFailed;
                }

                break :blk Self{
                    .symbol = TypedSymbol{
                        .symbol = try ally.dupe(u8, sym),
                        .stype = try bound_stype.clone(ally),
                    }
                };
            },
            .list => |list| blk: {
                if (list.len > 0) {
                    // infer on children
                    var exprs = try ally.alloc(Self, list.len);

                    exprs[0] = try Self.from_sexpr(
                        ally,
                        env,
                        list[0],
                        if (expects == .undef) expects else expects.list.*
                    );

                    // children past the first are expected to match the first
                    const subtype = try exprs[0].find_type(ally);

                    for (list[1..]) |sexpr, i| {
                        exprs[i + 1] = try Self.from_sexpr(
                            ally,
                            env,
                            sexpr,
                            subtype
                        );
                    }

                    break :blk Self{
                        .list = List{
                            .subtype = subtype,
                            .exprs = exprs
                        }
                    };
                } else {
                    // zero-length list
                    if (expects == .undef) return TypingError.UninferrableType;

                    break :blk Self{
                        .list = List{
                            .subtype = try expects.clone(ally),
                            .exprs = &.{}
                        }
                    };
                }
            },
            .call => |call| blk: {
                if (call.len == 0) return TypingError.CalledNothing;

                // TODO lambdas here
                // TODO actual type checking against expectations here

                if (call[0] == .symbol) {
                    const exprs = try ally.alloc(Self, call.len);

                    // infer fn
                    exprs[0] = try Self.from_sexpr(
                        ally,
                        env,
                        call[0],
                        SType{ .undef = {} }
                    );

                    const func_data = exprs[0].symbol.stype.func;

                    // infer params
                    for (call[1..]) |param, i| {
                        exprs[i + 1] = try Self.from_sexpr(
                            ally,
                            env,
                            param,
                            func_data.params[i]
                        );
                    }

                    break :blk Self{
                        .call = Call{
                            .returns = try func_data.returns.clone(ally),
                            .exprs = exprs,
                        }
                    };
                } else {
                    @panic("TODO non-symbol calls");
                }
            },
            .func => |func| blk: {
                if (expects == .undef) {
                    return TypingError.FuncWithoutExpectation;
                }

                const exp_func = expects.func;
                const exp_params = exp_func.params;

                if (exp_params.len != func.params.len) {
                    return TypingError.ExpectationFailed;
                }

                // create params
                const params = try ally.alloc(TypedSymbol, func.params.len);
                for (func.params) |param_sym, i| {
                    params[i] = TypedSymbol{
                        .stype = try exp_params[i].clone(ally),
                        .symbol = try ally.dupe(u8, param_sym),
                    };
                }

                // translate body with defined params
                var sub_env = Env.init(ally, &env);
                defer sub_env.deinit();

                for (params) |param, i| {
                    try sub_env.define_local(param.symbol, param.stype, i);
                }

                const body = try util.place_on(ally, try Self.from_sexpr(
                    ally,
                    sub_env,
                    func.body.*,
                    exp_func.returns.*
                ));

                break :blk Self{
                    .func = Func{ .params = params, .body = body }
                };
            },
            .def => |def| Self{
                .def = Def{
                    .symbol = try ally.dupe(u8, def.symbol),
                    .anno = try util.place_on(ally, try Self.from_sexpr(
                        ally,
                        env,
                        def.anno.*,
                        SType{ .stype = {} }
                    )),
                    .body = try util.place_on(ally, try def.body.clone(ally)),
                }
            },
            else => std.debug.panic("TODO translate {s}", .{@tagName(expr)})
        };
    }

    /// determines the type of this expr when executed
    pub fn find_type(self: Self, ally: Allocator) Allocator.Error!SType {
        return switch (self) {
            .unit => SType{ .unit = {} },
            .undef => SType{ .undef = {} },
            .int => SType{ .int = {} },
            .stype => SType{ .stype = {} },
            .symbol => |sym| try sym.stype.clone(ally),
            .ptr => |sub| try SType.init_ptr(ally, try sub.find_type(ally)),
            .list => |list| try SType.init_list(ally, list.subtype),
            .call => |call| try call.returns.clone(ally),
            .func => |meta| blk: {
                const param_types = try ally.alloc(SType, meta.params.len);
                for (meta.params) |param, i| {
                    param_types[i] = try param.stype.clone(ally);
                }

                const returns = try util.place_on(
                    ally,
                    try meta.body.find_type(ally)
                );

                break :blk SType{
                    .func = .{
                        .params = param_types,
                        .returns = returns
                    }
                };
            },
            .def => SType{ .unit = {} }
        };
    }

    /// converts this expr to an untyped value
    pub fn to_sexpr(self: Self, ally: Allocator) Allocator.Error!SExpr {
        return switch (self) {
            .unit => SExpr{ .unit = {} },
            .undef => SExpr{ .undef = {} },
            .int => |n| SExpr{ .int = n },
            .stype => |t| SExpr{ .stype = try t.clone(ally) },
            .symbol => |sym| SExpr{ .symbol = try ally.dupe(u8, sym.symbol) },
            .ptr => @panic("TODO"),
            .list => |list| blk: {
                const values = try ally.alloc(SExpr, list.exprs.len);
                for (list.exprs) |expr, i| values[i] = try expr.to_sexpr(ally);

                break :blk SExpr{ .list = values };
            },
            .call => |call| blk: {
                const values = try ally.alloc(SExpr, call.exprs.len);
                for (call.exprs) |expr, i| values[i] = try expr.to_sexpr(ally);

                break :blk SExpr{ .call = values };
            },
            .func => @panic("TODO"),
            .def => |def| SExpr{
                .def = .{
                    .symbol = try ally.dupe(u8, def.symbol),
                    .anno = try util.place_on(
                        ally,
                        try def.anno.to_sexpr(ally)
                    ),
                    .body = try util.place_on(ally, try def.body.clone(ally)),
                }
            },
        };
    }

    /// recursively determines whether this is a data literal
    pub fn is_literal(self: Self) bool {
        return switch (self) {
            .unit, .undef, .int, .stype => true,
            .list => |data| blk: {
                for (data.exprs) |expr| {
                    if (!expr.is_literal()) break :blk false;
                }

                break :blk true;
            },
            else => false,
        };
    }

    pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
        return switch (self) {
            .unit, .undef, .int => self,
            .stype => |t| Self{ .stype = try t.clone(ally) },
            .symbol => |sym| Self{ .symbol = try sym.clone(ally) },
            .ptr => |sub| Self{
                .ptr = try util.place_on(ally, try sub.clone(ally))
            },
            .list => |list| Self{ .list = try list.clone(ally) },
            .call => |call| Self{ .call = try call.clone(ally) },
            .func => |func| blk: {
                const copied = try ally.alloc(TypedSymbol, func.params.len);
                for (func.params) |param, i| copied[i] = try param.clone(ally);

                break :blk Self{
                    .func = .{
                        .params = copied,
                        .body = try util.place_on(
                            ally,
                            try func.body.clone(ally)
                        ),
                    }
                };
            },
            .def => |def| Self{
                .def = .{
                    .symbol = try ally.dupe(u8, def.symbol),
                    .anno = try util.place_on(ally, try def.anno.clone(ally)),
                    .body = try util.place_on(ally, try def.body.clone(ally)),
                }
            },
        };
    }

    fn display_r(
        self: Self,
        canvas: *kz.Canvas,
        cursor: *kz.Vec2
    ) anyerror!void {
        const INDENT = 4;
        const pos = cursor.*;

        // type
        const type_msg = try canvas.print(
            "<{}>",
            .{self.find_type(canvas.arena.allocator())}
        );

        try canvas.scribble(
            pos + kz.Vec2{-@intCast(isize, type_msg.len) - 1, 0},
            kz.Color{ .fg = .green, .fmt = .bold },
            "{s}",
            .{type_msg}
        );

        // payload
        const lit_color = kz.Color{ .fg = .magenta };
        const sym_color = kz.Color{ .fg = .red };

        switch (self) {
            .unit, .undef => try canvas.scribble(
                pos,
                kz.Color{},
                "{s}",
                .{@tagName(self)}
            ),
            .int => |n| try canvas.scribble(pos, lit_color, "{}", .{n}),
            .symbol => |sym| {
                try canvas.scribble(pos, sym_color, "{s}", .{sym.symbol});
            },
            .stype => |t| try canvas.scribble(pos, lit_color, "{}", .{t}),
            .ptr => @panic("TODO display TypedExpr ptr"),
            .list => |list| {
                try canvas.scribble(pos, kz.Color{}, "{s}", .{@tagName(self)});

                cursor.*[0] += INDENT;
                for (list.exprs) |child| {
                    cursor.*[1] += 1;
                    try child.display_r(canvas, cursor);
                }
                cursor.*[0] -= INDENT;
            },
            .call => |call| {
                try canvas.scribble(pos, kz.Color{}, "call", .{});

                cursor.*[0] += INDENT;
                for (call.exprs) |child| {
                    cursor.*[1] += 1;
                    try child.display_r(canvas, cursor);
                }
                cursor.*[0] -= INDENT;
            },
            .func => @panic("TODO display func"),
            .def => |meta| {
                try canvas.scribble(pos, kz.Color{}, "def {s}", .{meta.symbol});

                cursor.* += kz.Vec2{INDENT, 1};
                try meta.anno.display_r(canvas, cursor);
                cursor.*[1] += 1;
                try canvas.scribble(cursor.*, kz.Color{}, "`{}`", .{meta.body});
                cursor.* += kz.Vec2{-INDENT, 1};
            },
        }
    }

    pub fn display(
        self: Self,
        ally: Allocator,
        comptime label_fmt: []const u8,
        label_args: anytype
    ) !void {
        var canvas = kz.Canvas.init(ally);
        defer canvas.deinit();

        try canvas.scribble(
            .{0, -1},
            kz.Color{ .fg = .cyan },
            label_fmt,
            label_args
        );

        var cursor = kz.Vec2{0, 0};
        try self.display_r(&canvas, &cursor);

        try canvas.flush(stdout);
    }
};

/// semantic analysis.
///
/// arena compatible
pub fn analyze(
    ally: Allocator,
    env: Env,
    sexpr: SExpr,
    expects: ?SType
) !TypedExpr {
    const expr = TypedExpr.from_sexpr(
        ally,
        env,
        sexpr,
        expects orelse SType{ .undef = {} }
    );

    // TODO ensure that defs are only global?
    // TODO function + type dependency solve?

    return expr;
}