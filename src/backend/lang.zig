//! defining the base Fluent language.

const std = @import("std");
const fluent = @import("fluent.zig");
const plumbing = @import("../plumbing.zig");
const ir = @import("ir.zig");
const Env = @import("env.zig");

const Allocator = std.mem.Allocator;
const SType = fluent.SType;
const SExpr = fluent.SExpr;

/// fluent's builtin functions, used in `eval` and Env mappings
pub const Builtin = union(enum) {
    const Self = @This();

    opcode: ir.OpCode,

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        _ = fmt;
        _ = options;

        switch (self) {
            .opcode => |code| try writer.print("opcode {s}", .{@tagName(code)}),
        }
    }
};

pub fn create_prelude(ally: Allocator) !Env {
    var prelude = try Env.init(ally, null);

    var arena = std.heap.ArenaAllocator.init(ally);
    defer arena.deinit();
    const tmp = arena.allocator();

    // type primitives
    try prelude.define_type("type", SType{ .stype = {} });
    try prelude.define_type("int", SType{ .int = {} });

    // `fn`
    // const fn_type_fn = try SType.init_func(
        // tmp,
        // &.{
            // try SType.init_list(tmp, SType{ .stype = {} }),
            // SType{ .stype = {} }
        // },
        // SType{ .stype = {} }
    // );

    // try prelude.define_builtin("fn", fn_type_fn, .{ .opcode = .func_type });

    // math
    const bin_int_math_op = try SType.init_func(
        tmp,
        &.{SType{ .int = {} }, SType{ .int = {} }},
        SType{ .int = {} }
    );

    try prelude.define_builtin("+", bin_int_math_op, .{ .opcode = .iadd });
    try prelude.define_builtin("-", bin_int_math_op, .{ .opcode = .isub });
    try prelude.define_builtin("*", bin_int_math_op, .{ .opcode = .imul });
    try prelude.define_builtin("/", bin_int_math_op, .{ .opcode = .idiv });
    try prelude.define_builtin("%", bin_int_math_op, .{ .opcode = .imod });

    try prelude.display("prelude", .{});

    return prelude;
}