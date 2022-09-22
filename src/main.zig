const std = @import("std");
const kz = @import("kritzler");
const util = @import("util/util.zig");
const plumbing = @import("plumbing.zig");
const backend = @import("backend.zig");

const stdout = std.io.getStdOut().writer();
const Allocator = std.mem.Allocator;

const c = @cImport({
    @cInclude("linenoise.h");
});

/// returns string allocated onto ally
fn repl_read(ally: Allocator) ![]const u8 {
    const INDENT = 2;

    var buf = std.ArrayList(u8).init(ally);
    var level: usize = 0;

    while (true) {
        // print prompt with indent
        const indent = try ally.alloc(u8, level * INDENT);
        defer ally.free(indent);

        std.mem.set(u8, indent, ' ');

        const prompt = try std.fmt.allocPrintZ(ally, "> {s}", .{indent});
        defer ally.free(prompt);

        // get text with linenoise
        const raw_line: ?[*:0]u8 = c.linenoise(prompt);
        defer c.linenoiseFree(raw_line);

        const line =
            if (raw_line == null or raw_line.?[0] == 0) ""
            else raw_line.?[0..std.mem.len(raw_line.?)];

        // parse line
        for (line) |ch| {
            switch (ch) {
                '(', '[' => level += 1,
                ')', ']' => if (level > 0) {
                    level -= 1;
                },
                else => {}
            }
        }

        // append line as text
        try buf.appendSlice(indent);
        try buf.appendSlice(line);
        try buf.append('\n');

        // break once expr is finished
        if (level == 0) break;
    }

    return buf.toOwnedSlice();
}

fn repl(ally: Allocator, prelude: backend.Env) !void {
    var repl_env = backend.Env.init(ally, &prelude);
    defer repl_env.deinit();

    while (true) {
        const input = try repl_read(ally);
        defer ally.free(input);

        // check for empty input (exit program)
        var is_empty: bool = true;
        for (input) |ch| {
            if (!std.ascii.isSpace(ch)) {
                is_empty = false;
                break;
            }
        }

        if (is_empty) break;

        // eval and print
        var result = try plumbing.execute(ally, &repl_env, "repl", input);
        defer result.deinit(ally);

        try stdout.print("{}\n", .{result});
    }
}

pub fn main() !void {
    // var gpa = std.heap.GeneralPurposeAllocator(.{
        // .stack_trace_frames = 1000,
    // }){};
    // defer _ = gpa.deinit();
    // const ally = gpa.allocator();
    const ally = std.heap.page_allocator;

    var prelude = try backend.create_prelude(ally);
    defer prelude.deinit();

    // do language tests (I just want all of it to compile, lol)
    const tests = [_][]const u8{
        // out-of-order defs
        \\b
        \\(def b T (+ a 1))
        \\(def a T (* 10 10))
        \\(def T Type Int)
        ,

        \\(def a T (* 10 10))
        \\(def b X (+ a 1))
        \\(def X Type T)
        \\(def T Type Int)
        \\b
        ,

        // different kinds of literals
        "0xDEAD_BEEF",
        "0b1010",
        "0o777",
        "true",
        "false",
        "[1 -2 3]",

        // math
        "(/ (+ 45 69) 2)",

        // logic
        "(and (or true false) (not false))",
        "(or false (not true))",

        // function type
        \\(def int-to-int Type
        \\  (Fn [Int] Int))
        ,

        // interreliant defs
        \\(def c Int (+ a b))
        \\(def a Int (* 34 56))
        \\(def b Int (+ a 1))
        \\c
        ,

        // simple function
        \\(def my-add (Fn [Int Int] Int)
        \\  (fn [a b] (+ a b)))
        \\
        \\(my-add 45 56)
        ,

        // conditional
        \\[
        \\  (if true 1 0)
        \\  (if false 1 0)
        \\]
        ,
    };

    for (tests) |@"test"| {
        var env = backend.Env.init(ally, &prelude);
        defer env.deinit();

        // print test input
        try stdout.print(
            "{}test{}:\n",
            .{&kz.Color{ .fg = .cyan }, &kz.Color{}}
        );

        var lines = std.mem.tokenize(u8, @"test", "\n");
        while (lines.next()) |line| try stdout.print("> {s}\n", .{line});

        try stdout.writeAll("\n");

        // run test
        var result = plumbing.execute(ally, &env, "test", @"test") catch |e| {
            try stdout.print(
                "{}test failed with {}{}:\n{s}\n\n",
                .{&kz.Color{ .fg = .red }, e, &kz.Color{}, @"test"}
            );
            continue;
        };
        defer result.deinit(ally);

        try stdout.print("{}\n\n", .{result});
    }
}