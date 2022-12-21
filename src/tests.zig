const std = @import("std");
const exec = @import("plumbing.zig").execute;
const backend = @import("backend.zig");
const context = @import("context.zig");

fn expectEqual(expected: []const u8, actual: []const u8) !void {
    const ally = std.testing.allocator;

    try context.init(ally);
    defer context.deinit();

    var tw = backend.TypeWelt.init(ally);
    defer tw.deinit();

    var env = try backend.initPrelude(ally, &tw);
    defer env.deinit();

    const exp_file = try context.addExternalSource("expected", expected);
    const act_file = try context.addExternalSource("actual", actual);

    const exp_val = try exec(ally, &env, exp_file);
    const act_val = try exec(ally, &env, act_file);

    try std.testing.expect(exp_val.eql(act_val));
}

test "0xdeadbeef" {
    var buf: [16]u8 = undefined;
    const n = try std.fmt.bufPrint(&buf, "{d}", .{0xDEAD_BEEF});
    try expectEqual("0xDEAD_BEEF", n);
}

test "0b1010" {
    try expectEqual("0b1010", "10");
}

test "0o777" {
    try expectEqual("0o777", "511");
}

test "0.0001" {
    var buf: [16]u8 = undefined;
    const n = try std.fmt.bufPrint(&buf, "{d}", .{0.0001});
    try expectEqual("0.0001", n);
}

test "0.01" {
    var buf: [16]u8 = undefined;
    const n = try std.fmt.bufPrint(&buf, "{d}", .{0.01});
    try expectEqual("0.01", n);
}

test "0x1.1" {
    var buf: [16]u8 = undefined;
    const n = try std.fmt.bufPrint(&buf, "{d}", .{0x1.1});
    try expectEqual("0x1.1", n);
}

test "and" {
    try expectEqual("and true true", "true");
    try expectEqual("and false true", "false");
    try expectEqual("and true false", "false");
    try expectEqual("and false false", "false");
}

test "or" {
    try expectEqual("or true true", "true");
    try expectEqual("or false true", "true");
    try expectEqual("or true false", "true");
    try expectEqual("or false false", "false");
}

test "not" {
    try expectEqual("not true", "false");
    try expectEqual("not false", "true");
}

test "arithmetic" {
    try expectEqual(
        \\/ (+ 45 69)
        \\  2
        ,
        "57"
    );
}

test "if" {
    try expectEqual("if true 1 0", "1");
    try expectEqual("if false 1 0", "0");
}
