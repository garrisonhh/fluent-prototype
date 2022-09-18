//! semantic analysis in the Fluent compiler is the process of performing static
//! type analysis on a raw SExpr tree to produce TypedExprs for IR generation

const std = @import("std");
const kz = @import("kritzler");
const util = @import("../util/util.zig");
const literals = @import("../util/literals.zig");
const fluent = @import("fluent.zig");
const frontend = @import("../frontend.zig");
const Env = @import("env.zig");

const Allocator = std.mem.Allocator;
const FlatType = fluent.FlatType;
const Type = fluent.Type;
const Pattern = fluent.Pattern;
const Value = fluent.Value;
const AstExpr = frontend.AstExpr;
const stdout = std.io.getStdOut().writer();

pub const SemaError = error {
    ExpectationFailed,
    UninferrableType,

    // parsing (syntax errors that aren't caught in the frontend)
    BadNumber,
    BadDef,
    BadFn,

    // symbol lookup
    EnvError,
    UnknownSymbol,

    // calls
    CalledNothing,
    CalledNonFunction,

    // funcs
    FuncWithoutExpectation,
};

pub const Error = SemaError || literals.ParseNumberError || Allocator.Error;

/// see `from_sexpr()`
///
/// maps 1-to-1 to SExpr in terms of structure, but contains extra type
/// information for a more complete AST
///
/// TODO I should just translate ast Exprs straight to this, make SExpr more
/// of a pure value type
pub const TypedExpr = union(enum) {
    const Self = @This();

    pub const TypedSymbol = struct {
        stype: Type,
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
        subtype: Type,
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
        returns: Type,
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
        body: *const AstExpr, // TypedExpr does not own this memory
    };

    undef,

    unit,
    int: i64,
    stype: Type,
    symbol: TypedSymbol,

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
            },
            else => {}
        }
    }

    /// determines the type of this expr when executed
    ///
    /// *I want this to stay incredibly trivial*
    pub fn find_type(self: Self, ally: Allocator) Allocator.Error!Type {
        return switch (self) {
            .unit => Type{ .unit = {} },
            .undef => Type{ .undef = {} },
            .int => Type{ .int = {} },
            .stype => Type{ .stype = {} },
            .symbol => |sym| try sym.stype.clone(ally),
            .list => |list| try Type.init_list(ally, list.subtype),
            .call => |call| try call.returns.clone(ally),
            .func => |meta| blk: {
                const param_types = try ally.alloc(Type, meta.params.len);
                for (meta.params) |param, i| {
                    param_types[i] = try param.stype.clone(ally);
                }

                const returns = try util.place_on(
                    ally,
                    try meta.body.find_type(ally)
                );

                break :blk Type{
                    .func = .{
                        .params = param_types,
                        .returns = returns
                    }
                };
            },
            .def => Type{ .unit = {} }
        };
    }

    /// converts this expr to an untyped value
    pub fn to_value(self: Self, ally: Allocator) Allocator.Error!Value {
        return switch (self) {
            .unit => Value{ .unit = {} },
            .undef => Value{ .undef = {} },
            .int => |n| Value{ .int = n },
            .stype => |t| Value{ .stype = try t.clone(ally) },
            .symbol => @panic("symbols aren't values"),
            .list => |list| blk: {
                const values = try ally.alloc(Value, list.exprs.len);
                for (list.exprs) |expr, i| values[i] = try expr.to_value(ally);

                break :blk Value{ .list = values };
            },
            .call => @panic("TODO to_sexpr of call"),
            .func => @panic("TODO to_sexpr of func"),
            .def => @panic("defs aren't values"),
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
        const tmp = canvas.arena.allocator();

        // type
        const type_msg = try canvas.print(
            "<{}>",
            .{self.find_type(canvas.arena.allocator())}
        );

        try canvas.scribble(
            pos + kz.Vec2{-@intCast(isize, type_msg.len) - 1, 0},
            kz.Color{ .fmt = .bold },
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
            .func => |func| {
                // write params to a buffer manually. lmao
                // TODO retool kritzler now that I've used it a while :)
                var chars: usize = 1;
                for (func.params) |sym| chars += sym.symbol.len + 1;

                var buf = try tmp.alloc(u8, chars);
                var i: usize = 1;

                buf[0] = '[';
                for (func.params) |sym| {
                    std.mem.copy(u8, buf[i..], sym.symbol);
                    buf[i + sym.symbol.len] = ' ';
                    i += sym.symbol.len + 1;
                }
                buf[i - 1] = ']';

                // scribble
                try canvas.scribble(pos, kz.Color{}, "fn {s}", .{buf});
                cursor.* += kz.Vec2{INDENT, 1};
                try func.body.display_r(canvas, cursor);
                cursor.*[0] -= INDENT;
            },
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

fn translate_list(
    ally: Allocator,
    env: Env,
    children: []AstExpr,
    expects: ?Pattern
) Error!TypedExpr {
    if (children.len == 0) {
        // empty list
        const subtype =
            if (try Pattern.to_type(ally, expects)) |stype| stype
            else Type{ .undef = {} };

        return TypedExpr{
            .list = TypedExpr.List{ .subtype = subtype, .exprs = &.{} }
        };
    } else {
        const exprs = try ally.alloc(TypedExpr, children.len);

        // type first element to get subtype expectations
        if (expects) |pat| {
            if (pat != .list) return SemaError.ExpectationFailed;

            const exp_sub = Pattern.unwrap(pat.list);
            exprs[0] = try translate(ally, env, children[0], exp_sub);
        } else {
            exprs[0] = try translate(ally, env, children[0], null);
        }

        const fst_type = try exprs[0].find_type(ally);
        defer fst_type.deinit(ally);

        // type the rest of the elements
        const fst_pat = try Pattern.from_type(ally, fst_type);
        defer fst_pat.deinit(ally);

        for (children[1..]) |child, i| {
            exprs[i + 1] = try translate(ally, env, child, fst_pat);
        }

        return TypedExpr{
            .list = TypedExpr.List{
                .subtype = try exprs[0].find_type(ally),
                .exprs = exprs
            }
        };
    }
}

fn translate_def(
    ally: Allocator,
    env: Env,
    children: []AstExpr,
    expects: ?Pattern
) Error!TypedExpr {
    _ = expects;

    if (children.len != 4) return SemaError.BadDef;

    const anno = try translate(ally, env, children[2], Pattern{ .stype = {} });

    return TypedExpr{
        .def = TypedExpr.Def{
            .symbol = try ally.dupe(u8, children[1].slice),
            .anno = try util.place_on(ally, anno),
            .body = &children[3]
        }
    };
}

fn translate_fn(
    ally: Allocator,
    env: Env,
    children: []AstExpr,
    expects: ?Pattern
) Error!TypedExpr {
    if (children.len != 3) return SemaError.BadFn;

    // get func pattern data
    const pat = expects orelse return SemaError.UninferrableType;
    if (pat != .func) return SemaError.ExpectationFailed;

    const func = pat.func;

    // parse params
    const param_list = children[1];
    if (param_list.etype != .list) return SemaError.BadFn;

    const param_exprs = param_list.children.?;
    if (param_exprs.len != func.params.len) return SemaError.BadFn;

    const params = try ally.alloc(TypedExpr.TypedSymbol, param_exprs.len);
    for (param_exprs) |expr, i| {
        params[i] = TypedExpr.TypedSymbol{
            .stype = (try Pattern.to_type(ally, func.params[i])) orelse {
                return SemaError.UninferrableType;
            },
            .symbol = try ally.dupe(u8, expr.slice)
        };
    }

    // translate body
    var sub_env = Env.init(ally, &env);
    defer sub_env.deinit();

    for (params) |param, i| {
        sub_env.define_local(param.symbol, param.stype, i) catch {
            return SemaError.EnvError;
        };
    }

    const exp_ret = Pattern.unwrap(func.returns);
    const body = try translate(ally, sub_env, children[2], exp_ret);

    return TypedExpr{
        .func = TypedExpr.Func{
            .params = params,
            .body = try util.place_on(ally, body)
        }
    };
}

// callbacks for translating builtin syntax
const TranslateFn = fn(Allocator, Env, []AstExpr, ?Pattern) Error!TypedExpr;
const syntax_table = std.ComptimeStringMap(TranslateFn, .{
    .{"def", translate_def},
    .{"fn", translate_fn},
});

fn translate_call(
    ally: Allocator,
    env: Env,
    children: []AstExpr,
    expects: ?Pattern
) Error!TypedExpr {
    if (children.len == 0) return SemaError.CalledNothing;

    const fn_expr = children[0];
    switch (fn_expr.etype) {
        .symbol => {
            // syntax
            if (syntax_table.get(fn_expr.slice)) |cb| {
                return try cb(ally, env, children, expects);
            }

            const exprs = try ally.alloc(TypedExpr, children.len);

            // fn
            exprs[0] = try translate(ally, env, fn_expr, null);

            const fn_type = try exprs[0].find_type(ally);
            defer fn_type.deinit(ally);

            if (fn_type != .func) return SemaError.CalledNonFunction;

            // function type is known in advance, so parameters are checked
            // against function
            for (children[1..]) |child, i| {
                const ptype = fn_type.func.params[i];
                const ppat = try Pattern.from_type(ally, ptype);
                defer ppat.deinit(ally);

                exprs[i + 1] = try translate(ally, env, child, ppat);
            }

            const returns = try fn_type.func.returns.clone(ally);

            return TypedExpr{
                .call = TypedExpr.Call{
                    .returns = returns,
                    .exprs = exprs
                }
            };
        },
        .call => {
            @panic("TODO translate anonymous fn literal call");
        },
        else => return SemaError.CalledNonFunction
    }
}

fn translate(
    ally: Allocator,
    env: Env,
    ast_expr: AstExpr,
    expects: ?Pattern
) Error!TypedExpr {
    // initial translation
    const expr = switch (ast_expr.etype) {
        .number => num: {
            const num = try literals.parse_number(ally, ast_expr.slice);
            defer num.deinit(ally);

            switch (num.layout) {
                .int => break :num TypedExpr{ .int = try num.to(i64) },
                .uint => @panic("TODO"),
                .float => @panic("TODO"),
            }
        },
        .string => @panic("TODO strings"),
        .character => @panic("TODO chars"),
        .symbol => symbol: {
            const symbol = ast_expr.slice;
            const stype = env.get_type(symbol) orelse {
                return SemaError.UnknownSymbol;
            };

            break :symbol TypedExpr{
                .symbol = TypedExpr.TypedSymbol{
                    .stype = try stype.clone(ally),
                    .symbol = try ally.dupe(u8, symbol)
                }
            };
        },
        .list => try translate_list(ally, env, ast_expr.children.?, expects),
        .call => try translate_call(ally, env, ast_expr.children.?, expects),
        .file => @panic("I use the `file` etype?")
    };

    // final type check
    const stype = try expr.find_type(ally);
    defer stype.deinit(ally);

    if (!stype.matches(expects)) {
        return SemaError.ExpectationFailed;
    }

    return expr;
}

/// check that `def` statements are only global
fn validate_defs(expr: TypedExpr, in_global: bool) SemaError!void {
    switch (expr) {
        .list => |list| {
            for (list.exprs) |child| try validate_defs(child, false);
        },
        .call => |call| {
            for (call.exprs) |child| try validate_defs(child, false);
        },
        .func => |func| try validate_defs(func.body.*, false),
        .def => |def| {
            if (!in_global) return SemaError.BadDef;
            try validate_defs(def.anno.*, false);
        },
        else => {}
    }
}

/// semantic analysis.
/// TODO eventually I want to do function/type dependency solving. currently
/// execution is exclusively linear (what they call a one-pass compiler) and
/// this will definitely end up being problematic once I start writing
/// larger programs (or just want to write trampolines etc.)
///
/// *arena compatible*
pub fn analyze(
    ally: Allocator,
    env: Env,
    ast_expr: AstExpr,
    expects: ?Type
) !TypedExpr {
    const pat = if (expects) |stype| try Pattern.from_type(ally, stype)
                else null;

    const expr = try translate(ally, env, ast_expr, pat);

    try validate_defs(expr, true);

    return expr;
}