const std = @import("std");
const stdout = std.io.getStdOut().writer();
const backend = @import("backend.zig");
const context = @import("context.zig");
const plumbing = @import("plumbing.zig");

fn exec(env: *backend.Env, handle: context.FileHandle) !backend.TExpr {
    return plumbing.exec(env, handle, .expr) catch |e| {
        _ = try context.flushMessages();
        return e;
    };
}

fn expectCompiles(str: []const u8) !void {
    const ally = std.testing.allocator;

    try context.init(ally);
    defer context.deinit();

    const file = try context.addExternalSource("test", str);

    var env = try backend.generatePrelude(ally);
    defer env.deinit();

    const expr = try exec(&env, file);
    defer expr.deinit(env.ally);
}

fn expectEqual(expected: []const u8, actual: []const u8) !void {
    const ally = std.testing.allocator;

    try context.init(ally);
    defer context.deinit();

    var env = try backend.generatePrelude(ally);
    defer env.deinit();

    const exp_file = try context.addExternalSource("expected", expected);
    const act_file = try context.addExternalSource("actual", actual);

    const exp_val = try exec(&env, exp_file);
    const act_val = try exec(&env, act_file);

    try std.testing.expect(exp_val.eql(act_val));
}

test "prelude types" {
    try expectCompiles("namespace");
    try expectCompiles("type");
    try expectCompiles("unit");
    try expectCompiles("bool");

    try expectCompiles("i8");
    try expectCompiles("i16");
    try expectCompiles("i32");
    try expectCompiles("i64");
    try expectCompiles("u8");
    try expectCompiles("u16");
    try expectCompiles("u32");
    try expectCompiles("u64");
    try expectCompiles("f32");
    try expectCompiles("f64");

    try expectCompiles("Any");
    try expectCompiles("Int");
    try expectCompiles("UInt");
    try expectCompiles("Float");
    try expectCompiles("Number");
}

test "number literals" {
    var buf: [256]u8 = undefined;
    var n: []u8 = undefined;

    n = try std.fmt.bufPrint(&buf, "{d}", .{0xDEAD_BEEF});
    try expectEqual("0xDEAD_BEEF", n);
    try expectEqual("0b1010", "10");
    try expectEqual("0o777", "511");
    n = try std.fmt.bufPrint(&buf, "{d}", .{0.0001});
    try expectEqual("0.0001", n);
    n = try std.fmt.bufPrint(&buf, "{d}", .{0.01});
    try expectEqual("0.01", n);
    n = try std.fmt.bufPrint(&buf, "{d}", .{0x1.1});
    try expectEqual("0x1.1", n);
}

test "bool literals" {
    try expectCompiles("true");
    try expectCompiles("false");
}

test "conditional operators" {
    try expectEqual("and true true", "true");
    try expectEqual("and false true", "false");
    try expectEqual("and true false", "false");
    try expectEqual("and false false", "false");

    try expectEqual("or true true", "true");
    try expectEqual("or false true", "true");
    try expectEqual("or true false", "true");
    try expectEqual("or false false", "false");

    try expectEqual("not true", "false");
    try expectEqual("not false", "true");
}

test "arithmetic operators" {
    try expectEqual("+ 2 3", "5i64");
    try expectEqual("- 3 2", "1i64");
    try expectEqual("* 3 2", "6i64");
    try expectEqual("/ 5 2", "2i64");
    try expectEqual("% 5 2", "1i64");
}

test "if" {
    try expectEqual("if true 1 0", "1");
    try expectEqual("if false 1 0", "0");
}
