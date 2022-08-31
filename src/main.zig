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

fn repl_eval_print(ally: Allocator, env: backend.Env, text: []const u8) !void {
    // evaluate
    var result = try plumbing.comprehend_text(ally, "repl", text);
    defer result.deinit(ally);

    var stype = try result.infer_type(ally, env, backend.SType{ .undef = {} });
    defer stype.deinit(ally);

    var ir = try backend.lower(ally, env, result);
    defer ir.deinit(ally);

    // display nicely
    const color = kz.Color{ .fg = .green };
    const reset = kz.Color{};

    try stdout.print("{}<{}>{}\n{}\n\n", .{color, stype, reset, result});
    try ir.display(ally, "lowered to:", .{});
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

    var prelude = try backend.create_prelude(ally);
    defer prelude.deinit();

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

        try repl_eval_print(ally, prelude, input);
    }
}