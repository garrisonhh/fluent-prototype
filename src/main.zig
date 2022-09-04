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
        var result = try plumbing.evaluate(ally, &repl_env, "repl", input);
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
        "(/ (+ 45 69) 2)",
        "type",
        "(def a int (+ 1 2))",
    };

    for (tests) |@"test"| {
        var result = try plumbing.evaluate(ally, &prelude, "test", @"test");
        defer result.deinit(ally);

        try stdout.print(
            "{}testing:{}\n> {s}\n{}\n\n",
            .{
                &kz.Color{ .fg = .cyan },
                &kz.Color{},
                @"test",
                result
            }
        );
    }
}