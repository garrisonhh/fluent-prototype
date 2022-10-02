//! defining the base Fluent language.

const std = @import("std");
const fluent = @import("fluent.zig");
const plumbing = @import("../plumbing.zig");
const ir = @import("ir.zig");
const Env = @import("env.zig");

const Allocator = std.mem.Allocator;
const Type = fluent.Type;
const Value = fluent.Value;
const Op = ir.Op;
const OpCode = ir.OpCode;
const Block = ir.Block;

/// helper for builtin operators
fn define_op(
    env: *Env,
    name: []const u8,
    stype: Type,
    opcode: OpCode
) !void {
    std.debug.assert(stype == .func);

    // figure out locals
    var num_params = stype.func.params.len;
    var local_buf: [256]Type = undefined;
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
            .consts = &.{},
            .locals = local_buf[0..num_params + 1],
            .labels = &.{},
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
    const type_type = Type{ .stype = {} };
    const int_type = Type{ .int = {} };
    const bool_type = Type{ .boolean = {} };

    // consts in prelude
    try prelude.define_value("true", bool_type, Value{ .boolean = true });
    try prelude.define_value("false", bool_type, Value{ .boolean = false });

    // type primitives
    try prelude.define_type("Type", type_type);
    try prelude.define_type("Int", int_type);
    try prelude.define_type("Bool", bool_type);

    const fn_params = [_]Type{try Type.init_list(tmp, type_type), type_type};
    const fn_type = try Type.init_func(tmp, &fn_params, type_type);

    try define_op(&prelude, "Fn", fn_type, .@"fn");

    // math
    const bin_int_params = [_]Type{int_type, int_type};
    const bin_imath_type = try Type.init_func(tmp, &bin_int_params, int_type);

    try define_op(&prelude, "+", bin_imath_type, .iadd);
    try define_op(&prelude, "-", bin_imath_type, .isub);
    try define_op(&prelude, "*", bin_imath_type, .imul);
    try define_op(&prelude, "/", bin_imath_type, .idiv);
    try define_op(&prelude, "%", bin_imath_type, .imod);

    // logic
    const cond_params = [_]Type{bool_type, bool_type};
    const cond_type = try Type.init_func(tmp, &cond_params, bool_type);
    const lnot_type = try Type.init_func(tmp, &[_]Type{bool_type}, bool_type);

    try define_op(&prelude, "and", cond_type, .land);
    try define_op(&prelude, "or", cond_type, .lor);
    try define_op(&prelude, "not", lnot_type, .lnot);

    // numeric conditions
    const bin_icond_type = try Type.init_func(tmp, &bin_int_params, bool_type);

    try define_op(&prelude, "=", bin_icond_type, .ieq);
    try define_op(&prelude, "<", bin_icond_type, .ilt);

    try prelude.display("prelude", .{});

    return prelude;
}