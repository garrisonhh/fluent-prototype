const Object = @import("object.zig");
const TypeWelt = @import("typewelt.zig");
const TypeId = TypeWelt.TypeId;

pub const ExprTemplate = struct {
    const Self = @This();

    type: TypeId,
    data: union(enum) {
        unit: void,
        bool: bool,
        type: TypeId,
        uint: u64,
        int: i64,
        float: f64,
        string: []const u8,
        call: []Self,
    },
};

fn ExprMixin(comptime Self: type) type {
    return struct {
        pub fn deinit(self: Self) void {
            const env = self.env;

            switch (self.get(.data).into()) {
                .string => |sl| env.free(sl.ptr(), sl.len()),
                else => {},
            }
        }
    };
}

/// Fluent's AST represented as a Fluent data structure
pub const Expr = Object.Wrapper(ExprTemplate, ExprMixin);

test "exprs" {
    const std = @import("std");
    const expect = std.testing.expect;
    const expectEqual = std.testing.expectEqual;
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
        try expectEqual({}, data.unit);

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
        try expectEqual(true, data.bool);

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
        try expectEqual(@as(u64, 32), data.uint);

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
        try expectEqual(@as(i64, -32), data.int);

        try writer.writeAll("\n[i8]\n");
        try kz.display(env.ally, {}, expr, writer);
    }

    // string
    {
        const expr = try Expr.init(&env);
        defer expr.deinit();

        const str = "hi, my name is garrison";
        const owned_ptr = try env.alloc(.heap, str.len);
        const owned = env.img.intoSlice(owned_ptr, u8, str.len);
        std.mem.copy(u8, owned, str);

        expr.set(.type, try env.identifyZigType([str.len]u8));
        expr.get(.data).setInto(.string).setInto(owned_ptr, str.len);

        const data = expr.get(.data).into();
        try expect(data == .string);
        try expect(std.mem.eql(u8, data.string.into(), str));

        try writer.writeAll("\n[string]\n");
        try kz.display(env.ally, {}, expr, writer);
    }

    try writer.writeByte('\n');
}
