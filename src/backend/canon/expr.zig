const Object = @import("object.zig");
const TypeWelt = @import("typewelt.zig");
const TypeId = TypeWelt.TypeId;

pub const ExprTemplate = struct {
    type: TypeId,
    data: union(enum) {
        unit: void,
        bool: bool,
        type: TypeId,
        uint: u64,
        int: i64,
        float: f64,
        string: []const u8,
    },
};

/// Fluent's AST represented as a Fluent data structure
pub const Expr = Object.Wrapper(ExprTemplate);

test "exprs" {
    const std = @import("std");
    const expect = std.testing.expect;
    const prelude = @import("prelude.zig");
    const Basic = prelude.Basic;
    const Env = @import("../env.zig");
    const kz = @import("kritzler");

    const writer = std.io.getStdErr().writer();
    try writer.writeByte('\n');

    var env = try Env.init(std.testing.allocator);
    defer env.deinit();

    try prelude.initPrelude(&env);

    try Expr.I.dump(writer);

    // unit
    {
        const expr = try Expr.init(&env);
        defer expr.deinit();

        expr.set(.type, Basic.unit.get());
        expr.get(.data).set(.unit, {});

        const data = expr.get(.data).into();
        try expect(data == .unit);
        try expect(data.unit == {});

        try writer.writeAll("\n[unit]\n");
        try kz.display(env.ally, {}, expr, writer);
    }

    // bool
    {
        const expr = try Expr.init(&env);
        defer expr.deinit();

        expr.set(.type, Basic.bool.get());
        expr.get(.data).set(.bool, true);

        const data = expr.get(.data).into();
        try expect(data == .bool);
        try expect(data.bool == true);

        try writer.writeAll("\n[bool]\n");
        try kz.display(env.ally, {}, expr, writer);
    }

    // uint
    {
        const expr = try Expr.init(&env);
        defer expr.deinit();

        expr.set(.type, Basic.u32.get());
        expr.get(.data).set(.uint, 32);

        const data = expr.get(.data).into();
        try expect(data == .uint);
        try expect(data.uint == 32);

        try writer.writeAll("\n[u32]\n");
        try kz.display(env.ally, {}, expr, writer);
    }

    // int
    {
        const expr = try Expr.init(&env);
        defer expr.deinit();

        expr.set(.type, Basic.i8.get());
        expr.get(.data).set(.int, -32);

        const data = expr.get(.data).into();
        try expect(data == .int);
        try expect(data.int == -32);

        try writer.writeAll("\n[i8]\n");
        try kz.display(env.ally, {}, expr, writer);
    }

    // string
    {
        const expr = try Expr.init(&env);
        defer expr.deinit();

        const str = "hi, my name is garrison";

        expr.set(.type, try env.identifyZigType([str.len]u8));
        try expr.get(.data).setInto(.string).dupe(str);
        defer expr.get(.data).get(.string).free();

        const data = expr.get(.data).into();
        try expect(data == .string);
        try expect(std.mem.eql(u8, data.string.slice(), str));

        try writer.writeAll("\n[string]\n");
        try kz.display(env.ally, {}, expr, writer);
    }

    try writer.writeByte('\n');
}
