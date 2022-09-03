//! compiling SExprs into three-address-code quadruples.
//!
//! Ops are the 'quadruple' format of three address code. every Block contains
//! some number of statically known local variables, which ops reference in
//! their addressing.

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

pub const OpCode = enum {
    const Self = @This();

    // unique
    @"const", // load a constant
    copy, // copy a local to another local
    param, // load a local to a (virtual) parameter slot
    call, // call a function with loaded parameters

    // math
    iadd,
    isub,
    imul,
    idiv,
    imod,

    /// metadata for how an opcode operates
    const Flow = union(enum) {
        /// load constant
        @"const",
        /// unary referencing locals
        unary: struct {
            a: SType,
            to: SType,
        },
        /// binary referencing locals
        binary: struct {
            a: SType,
            b: SType,
            to: SType,
        },
    };

    fn get_flow(self: Self) Flow {
        const flow_table = comptime blk: {
            const int_stype = SType{ .int = {} };
            const undef_stype = SType{ .undef = {} };

            const unary_undef = Flow{
                .unary = .{
                    .a = undef_stype,
                    .to = undef_stype
                }
            };
            const bin_int_math = Flow{
                .binary = .{
                    .a = int_stype,
                    .b = int_stype,
                    .to = int_stype
                },
            };

            break :blk util.EnumTable(Self, Flow).init(.{
                .{.@"const", Flow{ .@"const" = {} }},
                .{.copy, unary_undef},
                .{.param, unary_undef},
                .{.call, unary_undef},

                .{.iadd, bin_int_math},
                .{.isub, bin_int_math},
                .{.imul, bin_int_math},
                .{.idiv, bin_int_math},
                .{.imod, bin_int_math},
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
    to: UInt, // where result is stored

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        _ = fmt;
        _ = options;

        switch (self.code) {
            .param => try writer.print("param {} = l{}", .{self.to, self.a}),
            .call => try writer.print("l{} = call f{}", .{self.to, self.a}),
            .copy => try writer.print("l{} = copy l{}", .{self.to, self.a}),
            else => switch (self.code.get_flow()) {
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
        try kz.forms.fast_list(
            ally,
            .{ .title = "consts" },
            self.consts,
            stdout
        );

        try kz.forms.fast_list(
            ally,
            .{
                .title = "locals",
                .fmt = "<{}>",
                .color = kz.Color{ .fg = .green }
            },
            self.locals,
            stdout
        );

        try kz.forms.fast_list(
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
            const adjusted = switch (op.code.get_flow()) {
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
fn build_const(mason: *Mason, env: Env, value: SExpr) anyerror!Op.UInt {
    _ = mason;
    _ = env;
    _ = value;

    @panic("TODO need to use TypedExpr.find_type()");

    // return try mason.add_op(Op{
        // .code = .@"const",
        // .a = try mason.add_const(value),
        // .to = try mason.add_local(
            // try value.infer_type(mason.ally, env, SType{ .undef = {} })
        // )
    // });
}

fn build_operator(
    mason: *Mason,
    env: Env,
    operator: OpCode,
    params: []const SExpr
) anyerror!Op.UInt {
    return switch (operator.get_flow()) {
        .@"const" => unreachable,
        .unary => |flow| try mason.add_op(Op{
            .code = operator,
            .a = try build_expr(mason, env, params[0]),
            .to = try mason.add_local(try flow.to.clone(mason.ally)),
        }),
        .binary => |flow| try mason.add_op(Op{
            .code = operator,
            .a = try build_expr(mason, env, params[0]),
            .b = try build_expr(mason, env, params[1]),
            .to = try mason.add_local(try flow.to.clone(mason.ally)),
        }),
    };
}

/// lowers function call
/// TODO this function demonstrates that passing around expected types between
/// lowering functions might be useful or even necessary. another solution is
/// producing a typed AST.
fn build_call(mason: *Mason, env: Env, expr: SExpr) anyerror!Op.UInt {
    const func = expr.call[0];
    const params = expr.call[1..];

    if (func == .symbol) {
        const bound = env.get_data(func.symbol).?;

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
            param_types[i] = try param.infer_type(
                mason.ally,
                env,
                SType{ .undef = {} }
            );

            param_refs[i] = try build_expr(mason, env, param);
        }

        // lower lambda with inferred parameter types
        const lambda = try lower_func(
            mason.ally,
            env,
            "anonymous",
            func.func.params,
            param_types,
            func.func.body.*
        );

        return try mason.inline_block(lambda, param_refs);
    } else {
        unreachable;
    }
}

/// returns where this expr produces its value (`to`)
fn build_expr(mason: *Mason, env: Env, expr: SExpr) anyerror!Op.UInt {
    return switch (expr) {
        .int, .stype =>
            try build_const(mason, env, try expr.clone(mason.ally)),
        .call => try build_call(mason, env, expr),
        // TODO can/should I cache these in Env bindings?
        .symbol => |symbol| switch (env.get_data(symbol).?) {
            .value => |value| try build_expr(mason, env, value),
            .builtin => std.debug.panic("TODO lower bound builtin", .{}),
            // THIS EXPRESSION IS FUCKING BEAUTIFUL.
            .param => |index| @intCast(Op.UInt, index),
            else => @panic("TODO build expr with typedexprs instead")
        },
        else => std.debug.panic("TODO lower {} SExprs", .{@as(FlatType, expr)})
    };
}

/// lowers a function to a block
fn lower_func(
    ally: Allocator,
    env: Env,
    name: []const u8,
    param_names: []const []const u8,
    param_types: []const SType,
    body: SExpr
) anyerror!Block {
    // construct param env
    std.debug.assert(param_names.len == param_types.len);

    var sub_env = try Env.init(ally, &env);
    defer sub_env.deinit();

    for (param_names) |param_name, i| {
        try sub_env.define_param(param_name, param_types[i], i);
    }

    // do masonry
    var mason = try Mason.init(ally, name, param_types);
    const output = try build_expr(&mason, sub_env, body);

    return mason.build(output);
}

/// lower a repl expression to a block
///
/// TODO I've been operating with the assumption that all types are inferrable
/// with SExpr.infer_type(). instead of full semantic analysis, performing a
/// step to check for inferrability before lower()ing might be the optimal way
/// to go about verifying my assumptions?
pub fn lower_repl_expr(ally: Allocator, env: Env, expr: SExpr) !Block {
    return try lower_func(ally, env, "repl_expr", &.{}, &.{}, expr);
}
