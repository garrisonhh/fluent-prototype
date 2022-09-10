//! defining the base Fluent language.

const std = @import("std");
const fluent = @import("fluent.zig");
const plumbing = @import("../plumbing.zig");
const ir = @import("ir.zig");
const Env = @import("env.zig");

const Allocator = std.mem.Allocator;
const SType = fluent.SType;
const SExpr = fluent.SExpr;
const Op = ir.Op;
const OpCode = ir.OpCode;
const Block = ir.Block;

/// helper for builtin operators
fn define_op(
    env: *Env,
    name: []const u8,
    stype: SType,
    opcode: OpCode
) !void {
    std.debug.assert(stype == .func);

    // figure out locals
    var num_params = stype.func.params.len;
    var local_buf: [256]SType = undefined;
    for (stype.func.params) |param, i| local_buf[i] = param;

    local_buf[num_params] = stype.func.returns.*;

    // figure out op
    const op = switch (num_params) {
        1 => Op{
            .code = opcode,
            .a = 0,
            .to = 1
        },
        2 => Op{
            .code = opcode,
            .a = 0,
            .b = 1,
            .to = 2
        },
        else => unreachable
    };

    // create block and define
    _ = try env.define_block(
        name,
        stype,
        Block{
            .name = name,
            .consts = &.{},
            .locals = local_buf[0..num_params + 1],
            .ops = &[_]Op{op},
            .inputs = num_params,
            .output = op.to
        }
    );
}

pub fn create_prelude(ally: Allocator) !Env {
    var prelude = Env.init(ally, null);

    var arena = std.heap.ArenaAllocator.init(ally);
    defer arena.deinit();
    const tmp = arena.allocator();

    // consts
    const type_type = SType{ .stype = {} };
    const int_type = SType{ .int = {} };

    // type primitives
    try prelude.define_type("Type", type_type);
    try prelude.define_type("Int", int_type);

    {
        const params = [_]SType{try SType.init_list(tmp, type_type), type_type};
        const fn_type = try SType.init_func(tmp, &params, type_type);

        try define_op(&prelude, "Fn", fn_type, .@"fn");
    }

    // math
    {
        const params = [_]SType{int_type, int_type};
        const bin_imath_type = try SType.init_func(tmp, &params, int_type);

        try define_op(&prelude, "+", bin_imath_type, .iadd);
        try define_op(&prelude, "-", bin_imath_type, .isub);
        try define_op(&prelude, "*", bin_imath_type, .imul);
        try define_op(&prelude, "/", bin_imath_type, .idiv);
        try define_op(&prelude, "%", bin_imath_type, .imod);
    }

    try prelude.display("prelude", .{});

    return prelude;
}