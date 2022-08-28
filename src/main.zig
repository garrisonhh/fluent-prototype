const std = @import("std");
const util = @import("util/util.zig");
const canvas = @import("util/canvas.zig");
const plumbing = @import("plumbing.zig");
const backend = @import("backend.zig");

const stdout = std.io.getStdOut().writer();
const Allocator = std.mem.Allocator;

const c = @cImport({
    @cInclude("linenoise.h");
});

fn repl_eval_print(ally: Allocator, env: backend.Env, text: []const u8) !void {
    // evaluate
    var result = try plumbing.comprehend_text(ally, "repl", text);
    defer result.deinit(ally);

    var stype = try result.infer_type(ally, env, backend.SType{ .undef = {} });
    defer stype.deinit(ally);

    // display nicely
    const color = canvas.ConsoleColor{ .fg = .green };
    const reset = canvas.ConsoleColor{};

    try stdout.print("{}<{}>{}\n{}\n\n", .{color, stype, reset, result});
}

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

pub fn main() !void {
    // var gpa = std.heap.GeneralPurposeAllocator(.{
        // .stack_trace_frames = 1000,
    // }){};
    // defer _ = gpa.deinit();
    // const ally = gpa.allocator();
    const ally = std.heap.page_allocator;

    var env = try backend.create_global_env(ally);
    defer env.deinit();

    // repl loop
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

        try repl_eval_print(ally, env, input);
    }
}