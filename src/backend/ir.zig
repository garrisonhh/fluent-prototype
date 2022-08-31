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

    @"const", // load a constant

    // math
    iadd,
    isub,
    imul,
    idiv,
    imod,

    /// metadata for how an opcode operates
    const Flow = union(enum) {
        @"const",
        unary: struct {
            a: SType,
            to: SType,
        },
        binary: struct {
            a: SType,
            b: SType,
            to: SType,
        },
    };

    fn get_flow(self: Self) Flow {
        const flow_table = comptime blk: {
            const int_stype = SType{ .int = {} };

            const bin_int_math = Flow{
                .binary = .{
                    .a = int_stype,
                    .b = int_stype,
                    .to = int_stype
                },
            };

            break :blk util.EnumTable(Self, Flow).init(.{
                .{.@"const", Flow{ .@"const" = {} }},

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
            .@"const" => try writer.print("{} = const {}", .{self.to, self.a}),
            .iadd, .isub, .imul, .idiv, .imod => try writer.print(
                "{} = {s} {} {}",
                .{self.to, @tagName(self.code), self.a, self.b}
            ),
        }
    }
};

/// basic representation of linearized code
pub const Block = struct {
    const Self = @This();

    consts: []const SExpr,
    locals: []const SType,
    ops: []const Op,

    pub fn deinit(self: Self, ally: Allocator) void {
        ally.free(self.consts);
        ally.free(self.locals);
        ally.free(self.ops);
    }

    pub fn display(
        self: Self,
        ally: Allocator,
        comptime label_fmt: []const u8,
        label_args: anytype
    ) (Allocator.Error || @TypeOf(stdout).Error)!void {
        try stdout.print("{}", .{kz.Color{ .fg = .cyan }});
        try stdout.print(label_fmt, label_args);
        try stdout.print("{}\n\n", .{kz.Color{}});

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
    }
};

/// for building blocks :)
const Mason = struct {
    const Self = @This();

    ally: Allocator,
    consts: std.ArrayList(SExpr),
    locals: std.ArrayList(SType),
    ops: std.ArrayList(Op),

    fn init(ally: Allocator) Self {
        return Self{
            .ally = ally,
            .consts = std.ArrayList(SExpr).init(ally),
            .locals = std.ArrayList(SType).init(ally),
            .ops = std.ArrayList(Op).init(ally),
        };
    }

    /// take all owned memory and turn it into a block
    fn build(self: *Self) Block {
        return Block{
            .consts = self.consts.toOwnedSlice(),
            .locals = self.locals.toOwnedSlice(),
            .ops = self.ops.toOwnedSlice(),
        };
    }

    /// adds a block to mason and adjusts all indexed references. only works
    /// for 'pure' blocks (that don't refer to anything in this block)
    fn append_block(self: *Self, block: Block) Allocator.Error!void {
        // dupe inputs
        const consts = try SExpr.clone_slice(self.ally, block.consts);
        const locals = try SType.clone_slice(self.ally, block.locals);
        const ops = try self.ally.dupe(Op, block.ops);

        // adjust constant and local references
        const constant_offset = @intCast(Op.UInt, self.consts.items.len);
        const local_offset = @intCast(Op.UInt, self.locals.items.len);

        for (ops) |*op| {
            op.to += local_offset;

            switch (op.code.get_class()) {
                .@"const" => op.a += constant_offset,
                .unary => op.a += local_offset,
                .binary => {
                    op.a += local_offset;
                    op.b += local_offset;
                },
            }
        }

        // add to lists
        try self.consts.appendSlice(consts);
        try self.locals.appendSlice(locals);
        try self.ops.appendSlice(block.ops);
    }

    fn add_const(self: *Self, @"const": SExpr) Allocator.Error!Op.UInt {
        const index = @intCast(Op.UInt, self.consts.items.len);
        try self.consts.append(@"const");

        return index;
    }

    fn add_local(self: *Self, local: SType) Allocator.Error!Op.UInt {
        const index = @intCast(Op.UInt, self.locals.items.len);
        try self.locals.append(local);

        return index;
    }

    fn add_op(self: *Self, op: Op) Allocator.Error!Op.UInt {
        const index = @intCast(Op.UInt, self.ops.items.len);
        try self.ops.append(op);

        return index;
    }
};

/// lowers the operation of loading a constant to a local
fn lower_const(mason: *Mason, env: Env, value: SExpr) anyerror!Op.UInt {
    return try mason.add_op(Op{
        .code = .@"const",
        .a = try mason.add_const(value),
        .to = try mason.add_local(
            try value.infer_type(mason.ally, env, SType{ .undef = {} })
        )
    });
}

fn lower_operator(
    mason: *Mason,
    env: Env,
    operator: OpCode,
    params: []const SExpr
) anyerror!Op.UInt {
    return switch (operator.get_flow()) {
        .@"const" => unreachable,
        .unary => |flow| try mason.add_op(Op{
            .code = operator,
            .a = try lower_expr(mason, env, params[0]),
            .to = try mason.add_local(try flow.to.clone(mason.ally)),
        }),
        .binary => |flow| try mason.add_op(Op{
            .code = operator,
            .a = try lower_expr(mason, env, params[0]),
            .b = try lower_expr(mason, env, params[1]),
            .to = try mason.add_local(try flow.to.clone(mason.ally)),
        }),
    };
}

fn lower_call(mason: *Mason, env: Env, expr: SExpr) anyerror!Op.UInt {
    const func = expr.tuple[0];
    const params = expr.tuple[1..];

    if (func == .symbol) {
        const bound = env.get_data(func.symbol).?;

        if (bound == .builtin) {
            // builtin function/operator
            return switch (bound.builtin) {
                .opcode => |code| try lower_operator(mason, env, code, params),
            };
        } else {
            std.debug.panic("TODO lower non-builtin functions", .{});
        }
    } else {
        std.debug.panic("TODO lower function literal", .{});
    }
}

/// returns where this expr produces its value (`to`)
fn lower_expr(mason: *Mason, env: Env, expr: SExpr) anyerror!Op.UInt {
    return switch (expr) {
        .nil, .int, .stype =>
            try lower_const(mason, env, try expr.clone(mason.ally)),
        .tuple => try lower_call(mason, env, expr),
        else => std.debug.panic("TODO lower {} SExprs", .{@as(FlatType, expr)})
    };
}

/// lower an expression to a block
pub fn lower(ally: Allocator, env: Env, expr: SExpr) !Block {
    var mason = Mason.init(ally);
    _ = try lower_expr(&mason, env, expr);

    return mason.build();
}