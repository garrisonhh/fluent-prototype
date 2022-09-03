//! semantic analysis in the Fluent compiler is the process of predeclaring
//! everything in the global scope, and performing static type analysis on a raw
//! SExpr tree to produce TypedExprs for IR generation

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
    UnknownSymbol,
    UninferrableType,
    CalledNothing,
    CalledNonFunction,
};

pub const Error = TypingError || Allocator.Error;

/// maps 1-to-1 to SExpr in terms of structure, but contains extra type
/// information for a more complete AST
pub const TypedExpr = union(FlatType) {
    const Self = @This();

    pub const TypedSymbol = struct {
        stype: SType,
        symbol: []const u8,
    };

    pub const Structured = struct {
        stype: SType,
        exprs: []Self,
    };

    unit,
    undef,

    symbol: TypedSymbol,
    int: i64,
    stype: SType,

    list: Structured,
    call: Structured,
    func: struct {
        params: []TypedSymbol,
        body: *Self,
    },
    def: struct {
        symbol: []const u8,
        anno: *Self,
        // anno needs to be executed before the body's type can be inferred
        body: *SExpr,
    },

    /// bidirectional type checking and inference. returns a TypedExpr tree on the
    /// ally. does not allow for conflicting information.
    /// TODO nicer errors
    fn from_sexpr(
        ally: Allocator,
        env: Env,
        expr: SExpr,
        expects: SType
    ) Error!TypedExpr {
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
                    return TypingError.UnknownSymbol;
                };

                if (!bound_stype.matches(expects)) {
                    // TODO remove vvv
                    std.debug.panic("{} != {}\n", .{bound_stype, expects});

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
                        .list = Structured{
                            .stype = subtype,
                            .exprs = exprs
                        }
                    };
                } else {
                    // zero-length list
                    if (expects == .undef) return TypingError.UninferrableType;

                    break :blk Self{
                        .list = Structured{
                            .stype = try expects.clone(ally),
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
                        .call = .{
                            .stype = try func_data.returns.clone(ally),
                            .exprs = exprs,
                        }
                    };
                } else {
                    @panic("TODO non-symbol calls");
                }
            },
            .def => |def| Self{
                .def = .{
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

    /// the type of this expr when executed
    pub fn find_type(self: Self, ally: Allocator) Allocator.Error!SType {
        return switch (self) {
            .unit => SType{ .unit = {} },
            .undef => SType{ .undef = {} },
            .int => SType{ .int = {} },
            .stype => SType{ .stype = {} },
            .symbol => |meta| try meta.stype.clone(ally),
            .list => |meta| SType.init_list(ally, meta.stype),
            .call => |meta| try meta.stype.clone(ally),
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
            .list, .call => |meta| {
                try canvas.scribble(pos, kz.Color{}, "{s}", .{@tagName(self)});

                cursor.*[0] += INDENT;
                for (meta.exprs) |child| {
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

pub const TypedAst = struct {
    const Self = @This();

    arena: std.heap.ArenaAllocator,
    exprs: []TypedExpr,

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
    }
};

/// performs type checking and type inference to prepare an AST for IR lowering
pub fn analyze(
    ally: Allocator,
    env: Env,
    sexprs: []const SExpr
) !TypedAst {
    var arena = std.heap.ArenaAllocator.init(ally);
    const ast_ally = arena.allocator();

    var exprs = try ast_ally.alloc(TypedExpr, sexprs.len);

    for (sexprs) |sexpr, i| {
        exprs[i] = try TypedExpr.from_sexpr(
            ast_ally,
            env,
            sexpr,
            SType{ .undef = {} }
        );

        // TODO debug output vvv
        try exprs[i].display(ally, "from {} to", .{sexpr});
    }

    return TypedAst{
        .arena = arena,
        .exprs = exprs,
    };
}