const std = @import("std");
const parse = @import("parse.zig");
const sema = @import("sema.zig");

const stdout = std.io.getStdOut().writer();
const Allocator = std.mem.Allocator;
const LispFile = @import("file.zig");
const Expr = @import("ast/expr.zig");

const c = @cImport({
    @cInclude("linenoise.h");
});

const MessageContext = struct {
    ally: Allocator,
    lfile: *const LispFile,
    msg_list: std.ArrayList(LispFile.Message),
};

fn gen_type_message(ctx: *MessageContext, expr: *const Expr) !void {
    const text = try std.fmt.allocPrint(ctx.ally, "{}", .{expr.ltype});
    const msg = ctx.lfile.new_message(.debug, text, expr.slice);

    try ctx.msg_list.append(msg);
}

const REPLCommand = union(enum) {
    exit,
};

fn parse_repl_command(ally: Allocator, text: []const u8) !?REPLCommand {
    // tokenize
    var tokens = std.ArrayList([]const u8).init(ally);
    defer tokens.deinit();

    var token_iter = std.mem.tokenize(u8, text, " \t");
    while (token_iter.next()) |token| try tokens.append(token);

    // parse
    if (tokens.items.len == 0) return null;

    const cmd = tokens.items[0];

    if (std.mem.eql(u8, cmd, "exit")) return REPLCommand{ .exit = {} };

    return error.UnknownREPLCommand;
}

fn eval_repl_expr(
    ally: Allocator,
    global: *const sema.TypeScope,
    text: []const u8
) !void {
    var lfile = try LispFile.init(ally, "stdin", text);
    defer lfile.deinit(ally);

    // generate ast
    if (try parse.parse(ally, global, &lfile, .expr)) |ast| {
        // print ast
        try stdout.print("ast:\n{}\n", .{ast.fmt(.{})});

        // print types of exprs
        var arena = std.heap.ArenaAllocator.init(ally);
        defer arena.deinit();

        var msg_ctx = MessageContext{
            .ally = arena.allocator(),
            .lfile = &lfile,
            .msg_list = std.ArrayList(LispFile.Message).init(ally)
        };
        defer msg_ctx.msg_list.deinit();

        try ast.traverse(&msg_ctx, gen_type_message);
        try LispFile.print_messages(
            ally,
            msg_ctx.msg_list.items,
            stdout
        );
    }
}

pub fn main() !void {
    // TODO use gpa to debug memory stuff
    // var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    // defer _ = gpa.deinit();
    const ally = std.heap.page_allocator; // gpa.allocator();
    var text = std.ArrayList(u8).init(ally);
    defer text.deinit();

    var global = try sema.TypeScope.init_global(ally);
    defer global.deinit();

    var empty_lines: usize = 0;

    while (true) {
        // get text with linenoise
        const raw_line: ?[*:0]u8 = c.linenoise("> ");

        const line =
            if (raw_line == null or raw_line.?[0] == 0) ""
            else raw_line.?[0..std.mem.len(raw_line.?)];

        // check for eval or store
        const parse_repl = std.mem.startsWith(u8, line, "!");

        if (line.len == 0 or parse_repl) {
            empty_lines += 1;
            if (empty_lines >= 3) break;

            try eval_repl_expr(ally, &global, text.items);
            text.clearAndFree();
        } else {
            empty_lines = 0;

            try text.appendSlice(line);
            try text.append('\n');
        }

        // parse repl commands when inputted
        if (parse_repl) repl: {
            const cmd =
                (try parse_repl_command(ally, line[1..]))
                orelse {
                    try stdout.writeAll(
                        "no idea what that command meant.\n"
                    );
                    break :repl;
                };

            switch (cmd) {
                .exit => {
                    try stdout.writeAll("goodbye!\n");
                    break;
                }
            }
        }
    }
}