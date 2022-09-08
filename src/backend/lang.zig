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
    var prelude = Env.init(ally, null);

    var arena = std.heap.ArenaAllocator.init(ally);
    defer arena.deinit();
    const tmp = arena.allocator();

    // type primitives
    try prelude.define_type("Type", SType{ .stype = {} });
    try prelude.define_type("Int", SType{ .int = {} });

    {
        const type_type = SType{ .stype = {} };
        const params = [_]SType{try SType.init_list(tmp, type_type), type_type};
        const fn_type = try SType.init_func(tmp, &params, type_type);

        try prelude.define_builtin("Fn", fn_type, .{ .opcode = .@"fn" });
    }

    // math
    {
        const int_type = SType{ .int = {} };
        const params = [_]SType{int_type, int_type};
        const bin_math = try SType.init_func(tmp, &params, int_type);

        try prelude.define_builtin("+", bin_math, .{ .opcode = .iadd });
        try prelude.define_builtin("-", bin_math, .{ .opcode = .isub });
        try prelude.define_builtin("*", bin_math, .{ .opcode = .imul });
        try prelude.define_builtin("/", bin_math, .{ .opcode = .idiv });
        try prelude.define_builtin("%", bin_math, .{ .opcode = .imod });
    }

    try prelude.display("prelude", .{});

    return prelude;
}