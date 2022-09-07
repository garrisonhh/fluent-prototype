//! compiling SExprs into three-address-code quadruples.
//!
//! Ops are the 'quadruple' format of three address code. every Block contains
//! some number of statically known local variables, which ops reference in
//! their addressing.

const std = @import("std");
const kz = @import("kritzler");
const util = @import("../util/util.zig");
const fluent = @import("fluent.zig");
const sema = @import("sema.zig");
const Env = @import("env.zig");

const Allocator = std.mem.Allocator;
const FlatType = fluent.Type;
const SType = fluent.SType;
const SExpr = fluent.SExpr;
const TypedExpr = sema.TypedExpr;
const stdout = std.io.getStdOut().writer();

pub const OpCode = enum {
    const Self = @This();

    // unique
    @"const", // load a constant
    copy, // copy a local to another local
    // TODO param, // load a local to a (virtual) parameter slot
    // TODO call, // call a function with loaded parameters

    // ptr ops
    // TODO create,
    // TODO destroy,
    peek, // load
    poke, // store
    pinc,
    padd,

    // list ops
    alloc, // takes type, size; allocates list
    // TODO free,
    index, // takes list, index; gets ptr to index
    list_ptr, // cast list to ptr

    // types
    @"fn",

    // math
    iinc,
    idec,
    iadd,
    isub,
    imul,
    idiv,
    imod,

    /// metadata for how an opcode operates
    /// TODO I think this is overengineered
    const OpMeta = union(enum) {
        // how to determine output type
        const Output = union(enum) {
            // absolute type
            abs: SType,

            // relative type
            typeof_a,
            typeof_b,

            // unique behavior
            ref, // take ref to a
            deref, // deref a
            list_ref,
            list_deref,
        };

        // unary load constant
        @"const",
        // unary referencing locals, stores result type
        unary: Output,
        // binary referencing locals, stores result type
        binary: Output,
        // no result type
        unary_effect,
        binary_effect,
    };

    fn get_metadata(self: Self) OpMeta {
        const flow_table = comptime blk: {
            const Output = OpMeta.Output;

            const un_rel = OpMeta{ .unary = Output{ .typeof_a = {} } };
            const un_int = OpMeta{
                .unary = Output{ .abs = SType{ .int = {} } }
            };

            const un_ptr_math = OpMeta{ .unary = Output{ .typeof_a = {} } };
            const bin_ptr_math = OpMeta{ .binary = Output{ .typeof_a = {} } };
            const list_ptr = OpMeta{ .unary = Output{ .list_ref = {} } };

            const bin_type = OpMeta{
                .binary = Output{ .abs = SType{ .stype = {} } }
            };
            const bin_int = OpMeta{
                .binary = Output{ .abs = SType{ .int = {} } }
            };

            break :blk util.EnumTable(Self, OpMeta).init(.{
                .{.@"const", OpMeta{ .@"const" = {} }},
                .{.copy, un_rel},

                .{.peek, OpMeta{ .unary = Output{ .deref = {} } }},
                .{.poke, OpMeta{ .binary_effect = {} }},
                .{.pinc, un_ptr_math},
                .{.padd, bin_ptr_math},

                .{.alloc, OpMeta{ .binary = Output{ .ref = {} } }},
                .{.index, OpMeta{ .binary = Output{ .deref = {} } }},
                .{.list_ptr, list_ptr},

                .{.@"fn", bin_type},

                .{.iinc, un_int},
                .{.idec, un_int},
                .{.iadd, bin_int},
                .{.isub, bin_int},
                .{.imul, bin_int},
                .{.idiv, bin_int},
                .{.imod, bin_int},
            });
        };

        return flow_table.get(self);
    }
};

/// ops are the three-address-code representation of dynamic fluent code
pub const Op = struct {
    const Self = @This();

    // used for the 4 fields, including backing int for enum
    pub const UInt = u16;

    // keep opcode in a 64-bit word
    comptime {
        std.debug.assert(@sizeOf(Self) <= 8);
    }

    code: OpCode, // operation to perform
    a: UInt,
    b: UInt = 0,
    to: UInt = 0, // where result is stored

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        _ = fmt;
        _ = options;

        switch (self.code) {
            // .param => try writer.print("param {} = l{}", .{self.to, self.a}),
            // .call => try writer.print("l{} = call f{}", .{self.to, self.a}),
            .copy => try writer.print("l{} = copy l{}", .{self.to, self.a}),
            else => switch (self.code.get_metadata()) {
                .@"const" => try writer.print(
                    "l{} = c{}",
                    .{self.to, self.a}
                ),
                .unary => try writer.print(
                    "l{} = {s} l{}",
                    .{self.to, @tagName(self.code), self.a}
                ),
                .binary => try writer.print(
                    "l{} = {s} l{} l{}",
                    .{self.to, @tagName(self.code), self.a, self.b}
                ),
                .unary_effect => try writer.print(
                    "{s} l{}",
                    .{@tagName(self.code), self.a}
                ),
                .binary_effect => try writer.print(
                    "{s} l{} l{}",
                    .{@tagName(self.code), self.a, self.b}
                ),
            }
        }
    }
};

/// basic representation of linearized code. every block represents a procedure,
/// with some number of parameters (including zero) and returning one value.
pub const Block = struct {
    const Self = @This();

    name: []const u8,

    consts: []const SExpr,
    locals: []const SType,
    ops: []const Op,

    inputs: usize, // locals[0..input] are the parameters for the block
    output: Op.UInt, // index of the output local

    pub fn deinit(self: Self, ally: Allocator) void {
        ally.free(self.name);
        ally.free(self.consts);
        ally.free(self.locals);
        ally.free(self.ops);
    }

    pub fn display(
        self: Self,
        ally: Allocator
    ) (Allocator.Error || @TypeOf(stdout).Error)!void {
        try stdout.print(
            "{}block{} {}{s}{}:\n\n",
            .{
                &kz.Color{ .fg = .cyan },
                &kz.Color{},
                &kz.Color{ .fg = .red },
                self.name,
                &kz.Color{}
            }
        );

        // lists
        try kz.fast_list(
            ally,
            .{ .title = "consts", .color = kz.Color{ .fg = .magenta } },
            self.consts,
            stdout
        );

        try kz.fast_list(
            ally,
            .{
                .title = "locals",
                .fmt = "<{}>",
                .color = kz.Color{ .fg = .green }
            },
            self.locals,
            stdout
        );

        try kz.fast_list(
            ally,
            .{ .title = "ops" },
            self.ops,
            stdout
        );

        // typing
        try stdout.print(
            "{}type{} {}",
            .{&kz.Color{ .fg = .cyan }, &kz.Color{}, &kz.Color{ .fg = .green }}
        );

        if (self.inputs > 0) {
            for (self.locals[0..self.inputs]) |local| {
                try stdout.print("{} ", .{local});
            }

            try stdout.writeAll("-> ");
        }

        try stdout.print("{}{}\n", .{self.locals[self.output], &kz.Color{}});

        // output
        try stdout.print(
            "{}block returns{} l{}\n\n",
            .{&kz.Color{ .fg = .cyan }, &kz.Color{}, self.output}
        );
    }
};

/// for building blocks :)
const Mason = struct {
    const Self = @This();

    ally: Allocator,
    consts: std.ArrayList(SExpr),
    locals: std.ArrayList(SType),
    ops: std.ArrayList(Op),

    name: []const u8,
    inputs: usize,

    pub fn init(
        ally: Allocator,
        name: []const u8,
        inputs: []const SType
    ) Allocator.Error!Self {
        var locals = std.ArrayList(SType).init(ally);

        try locals.appendSlice(try SType.clone_slice(ally, inputs));

        return Self{
            .ally = ally,
            .consts = std.ArrayList(SExpr).init(ally),
            .locals = locals,
            .ops = std.ArrayList(Op).init(ally),
            .name = try ally.dupe(u8, name),
            .inputs = inputs.len,
        };
    }

    /// take all owned memory and turn it into a block
    fn build(self: *Self, output: Op.UInt) Block {
        return Block{
            .name = self.name,
            .consts = self.consts.toOwnedSlice(),
            .locals = self.locals.toOwnedSlice(),
            .ops = self.ops.toOwnedSlice(),
            .inputs = self.inputs,
            .output = output
        };
    }

    /// inlines a block given a slice of locals to use as parameters.
    /// returns adjusted output ref.
    fn inline_block(
        self: *Self,
        block: Block,
        params: []Op.UInt
    ) Allocator.Error!Op.UInt {
        std.debug.assert(params.len == block.inputs);

        const const_offset = @intCast(Op.UInt, self.consts.items.len);
        const local_offset = @intCast(Op.UInt, self.locals.items.len);

        // copy constants and locals
        for (block.consts) |@"const"| {
            try self.consts.append(try @"const".clone(self.ally));
        }

        for (block.locals) |local| {
            try self.locals.append(try local.clone(self.ally));
        }

        // add copy ops
        for (params) |index, i| {
            try self.ops.append(Op{
                .code = .copy,
                .a = index,
                .to = @intCast(Op.UInt, i) + local_offset
            });
        }

        // copy ops and adjusting references along the way
        for (block.ops) |op| {
            const adjusted = switch (op.code.get_metadata()) {
                .@"const" => Op{
                    .code = .@"const",
                    .a = op.a + const_offset,
                    .to = op.to + local_offset
                },
                .unary => Op{
                    .code = op.code,
                    .a = op.a + local_offset,
                    .to = op.to + local_offset
                },
                .binary => Op{
                    .code = op.code,
                    .a = op.a + local_offset,
                    .b = op.b + local_offset,
                    .to = op.to + local_offset
                },
                .unary_effect => Op{
                    .code = op.code,
                    .a = op.a + local_offset
                },
                .binary_effect => Op{
                    .code = op.code,
                    .a = op.a + local_offset,
                    .b = op.b + local_offset
                },
            };

            try self.ops.append(adjusted);
        }

        return block.output + local_offset;
    }

    /// returns index of constant
    fn add_const(self: *Self, @"const": SExpr) Allocator.Error!Op.UInt {
        const index = @intCast(Op.UInt, self.consts.items.len);
        try self.consts.append(@"const");

        return index;
    }

    /// returns ref to local
    fn add_local(self: *Self, local: SType) Allocator.Error!Op.UInt {
        const index = @intCast(Op.UInt, self.locals.items.len);
        try self.locals.append(local);

        return index;
    }

    /// returns ref to local output of op
    fn add_op(self: *Self, op: Op) Allocator.Error!Op.UInt {
        try self.ops.append(op);
        return op.to;
    }
};

/// lowers the operation of loading a constant to a local
/// *you don't need to copy passed in value*
fn build_const(mason: *Mason, value: TypedExpr) anyerror!Op.UInt {
    const expects = try value.find_type(mason.ally);
    defer expects.deinit(mason.ally);

    return try mason.add_op(Op{
        .code = .@"const",
        .a = try mason.add_const(try value.to_sexpr(mason.ally)),
        .to = try mason.add_local(try value.find_type(mason.ally))
    });
}

/// helper for build_operator()
fn operator_result(
    ally: Allocator,
    params: []const TypedExpr,
    output: OpCode.OpMeta.Output
) anyerror!SType {
    // TODO this will cause panics until I finish executing type exprs in sema

    return switch (output) {
        .abs => |t| try t.clone(ally),
        .typeof_a => try params[0].find_type(ally),
        .typeof_b => try params[1].find_type(ally),
        .ref => try SType.init_ptr(ally, params[0].stype),
        .deref => try params[0].ptr.find_type(ally),
        .list_ref => try SType.init_list(ally, params[0].stype),
        .list_deref => try params[0].list.subtype.clone(ally),
    };
}

fn build_operator(
    mason: *Mason,
    env: Env,
    operator: OpCode,
    params: []const TypedExpr
) anyerror!Op.UInt {
    const ally = mason.ally;
    return switch (operator.get_metadata()) {
        .@"const" => unreachable,
        .unary => |out| try mason.add_op(Op{
            .code = operator,
            .a = try build_expr(mason, env, params[0]),
            .to = try mason.add_local(try operator_result(ally, params, out))
        }),
        .binary => |out| try mason.add_op(Op{
            .code = operator,
            .a = try build_expr(mason, env, params[0]),
            .b = try build_expr(mason, env, params[1]),
            .to = try mason.add_local(try operator_result(ally, params, out))
        }),
        .unary_effect, .binary_effect => unreachable,
    };
}

/// lowers function call
/// TODO this function demonstrates that passing around expected types between
/// lowering functions might be useful or even necessary. another solution is
/// producing a typed AST.
fn build_call(mason: *Mason, env: Env, expr: TypedExpr) anyerror!Op.UInt {
    const func = expr.call.exprs[0];
    const params = expr.call.exprs[1..];

    if (func == .symbol) {
        const bound = env.get_data(func.symbol.symbol).?;

        if (bound == .builtin) {
            // builtin function/operator
            return switch (bound.builtin) {
                .opcode => |code| try build_operator(mason, env, code, params),
            };
        } else {
            std.debug.panic("TODO lower non-builtin functions", .{});
        }
    } else if (func == .func) {
        // determine parameter types for inference + lower parameters
        var param_types = try mason.ally.alloc(SType, params.len);
        var param_refs = try mason.ally.alloc(Op.UInt, params.len);
        defer mason.ally.free(param_types);
        defer mason.ally.free(param_refs);

        for (params) |param, i| {
            param_types[i] = try param.find_type(mason.ally);
            param_refs[i] = try build_expr(mason, env, param);
        }

        // lower lambda with inferred parameter types
        const lambda = try lower_func(mason.ally, env, "lambda", func);

        return try mason.inline_block(lambda, param_refs);
    } else {
        unreachable;
    }
}

fn build_list(mason: *Mason, env: Env, expr: TypedExpr) anyerror!Op.UInt {
    const ally = mason.ally;
    _ = env;

    const subtype = expr.list.subtype;
    const exprs = expr.list.exprs;

    // type and size
    const size = @intCast(i64, exprs.len);
    const type_ref = try build_const(mason, TypedExpr{ .stype = subtype });
    const size_ref = try build_const(mason, TypedExpr{ .int = size });

    // list allocation
    const list_type = try SType.init_list(ally, try subtype.clone(ally));
    const list_ref = try mason.add_op(Op{
        .code = .alloc,
        .a = type_ref,
        .b = size_ref,
        .to = try mason.add_local(list_type)
    });

    // set indices
    const ptr_type = try SType.init_ptr(ally, try subtype.clone(ally));
    const ptr_ref = try mason.add_op(Op{
        .code = .list_ptr,
        .a = list_ref,
        .to = try mason.add_local(ptr_type)
    });

    for (exprs) |child, i| {
        // increment pointer
        if (i > 0) {
            _ = try mason.add_op(Op{
                .code = .pinc,
                .a = ptr_ref,
                .to = ptr_ref
            });
        }

        // poke value
        _ = try mason.add_op(Op{
            .code = .poke,
            .a = ptr_ref,
            .b = try build_expr(mason, env, child)
        });
    }

    return list_ref;
}

/// returns where this expr produces its value (`to`)
fn build_expr(mason: *Mason, env: Env, expr: TypedExpr) anyerror!Op.UInt {
    if (expr.is_literal()) return try build_const(mason, expr);

    return switch (expr) {
        // these are literals
        .unit, .undef, .int, .stype => unreachable,
        .symbol => |sym| switch (env.get_data(sym.symbol).?) {
            .value => |value| try build_expr(mason, env, value),
            .builtin => std.debug.panic("TODO lower bound builtin", .{}),
            // THIS EXPRESSION IS FUCKING BEAUTIFUL.
            .param => |index| @intCast(Op.UInt, index),
            else => |tag| std.debug.panic(
                "TODO build_expr for {s}",
                .{@tagName(tag)}
            )
        },
        .call => try build_call(mason, env, expr),
        .list => try build_list(mason, env, expr),
        else => std.debug.panic(
            "TODO lower {} TypedExprs",
            .{@as(FlatType, expr)}
        )
    };
}

/// lowers a function to a block
fn lower_func(
    ally: Allocator,
    env: Env,
    name: []const u8,
    fn_expr: TypedExpr
) anyerror!Block {
    const func = fn_expr.func;

    // construct param env
    var sub_env = try Env.init(ally, &env);
    defer sub_env.deinit();

    for (func.params) |param, i| {
        try sub_env.define_param(param.symbol, param.stype, i);
    }

    // do masonry
    const param_types = try ally.alloc(SType, func.params.len);
    defer ally.free(param_types);

    for (func.params) |param, i| param_types[i] = try param.stype.clone(ally);

    var mason = try Mason.init(ally, name, param_types);
    const output = try build_expr(&mason, sub_env, func.body.*);

    return mason.build(output);
}

/// lower a program (file or repl-level) expression to a block
pub fn lower_expr(
    ally: Allocator,
    env: Env,
    name: []const u8,
    expr: TypedExpr
) !Block {
    var mason = try Mason.init(ally, name, &.{});
    return mason.build(try build_expr(&mason, env, expr));
}
