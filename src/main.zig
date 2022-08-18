const std = @import("std");
const parse = @import("parse.zig");
const sema = @import("sema.zig");
const dynamic = @import("dynamic.zig");
const FlFile = @import("file.zig");
const Expr = @import("fluent/expr.zig");

const stdout = std.io.getStdOut().writer();
const Allocator = std.mem.Allocator;
const Scope = sema.Scope;

const c = @cImport({
    @cInclude("linenoise.h");
});

fn eval_repl(
    ally: Allocator,
    scope: *Scope,
    text: []const u8
) !void {
    var result =
        (try dynamic.eval(ally, scope, "stdin", text)) orelse return;
    defer result.deinit(ally);

    try stdout.print("{}\n", .{result.fmt(.{})});
}

/// returns string allocated onto ally
fn take_repl_input(ally: Allocator) ![]const u8 {
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
    // var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    // defer _ = gpa.deinit();
    // const ally = gpa.allocator();
    const ally = std.heap.page_allocator;

    var global = try sema.Scope.init_global(ally);
    defer global.deinit();

    while (true) {
        const input = try take_repl_input(ally);
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

        try eval_repl(ally, &global, input);
    }
}