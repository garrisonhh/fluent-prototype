const std = @import("std");
const kz = @import("kritzler");
const util = @import("util");
const context = @import("context.zig");
const plumbing = @import("plumbing.zig");
const backend = @import("backend.zig");

const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();
const Allocator = std.mem.Allocator;

const c = @cImport({
    @cInclude("linenoise.h");
});

/// returns file
fn repl_read(ally: Allocator) !context.FileHandle {
    const INDENT = 2;

    var buf = std.ArrayList(u8).init(ally);
    defer buf.deinit();

    var level: usize = 0;

    while (true) {
        // print prompt with indent
        const indent = try ally.alloc(u8, level * INDENT);
        defer ally.free(indent);

        std.mem.set(u8, indent, ' ');

        const prompt = try std.fmt.allocPrintZ(ally, "> {s}", .{indent});
        defer ally.free(prompt);

        // get text with linenoise
        const raw_line: ?[*:0]u8 = c.linenoise(prompt.ptr);
        defer c.linenoiseFree(raw_line);

        const line = if (raw_line) |raw| raw[0..std.mem.len(raw)] else "";

        // parse line
        for (line) |ch| {
            switch (ch) {
                '(', '[' => level += 1,
                ')', ']' => if (level > 0) {
                    level -= 1;
                },
                else => {}
            }}
        try buf.appendSlice(line);
        try buf.append('\n');

        // break once expr is finished
        if (level == 0) break;
    }

    return context.addExternalSource("repl", buf.items);
}

fn repl(ally: Allocator) !void {
    while (true) {
        const input = try repl_read(ally);

        // check for empty input (exit program)
        var is_empty: bool = true;
        for (input.getSource()) |ch| {
            if (!std.ascii.isSpace(ch)) {
                is_empty = false;
                break;
            }
        }

        if (is_empty) break;

        // eval and print
        try plumbing.execute(ally, input);
    }
}

fn fluent_tests(ally: Allocator) !void {
    const tests = [_][]const u8{
        // different kinds of literals
        "0xDEAD_BEEFi64",
        "0b1010",
        "0o777",
        "1.0001",
        "0.01f32",
        "0x1.1",
        "\"Hello, World!\"",
        "_this_is_a_hole",
        "true",
        "false",
        "[1, -2, 3]",
        "[[1], [2, 3], [4, 5, 6], _what_could_this_be]",

        // math
        "addi 1 1",
        //\\/ (+ 45 69)
        //\\  2
        //,

        // \\and
        // \\  or true false
        // \\  not false
        // ,
        // \\or false
        // \\  not true
        // ,

        // \\// function type
        // \\def int-to-int Type
        // \\    Fn [Int] Int
        // ,

        // \\// interreliant defs
        // \\def c Int (+ a b)
        // \\def a Int (* 34 56)
        // \\def b Int (+ a 1)
        // \\c
        // ,

        // \\// out-of-order defs
        // \\def a T (* 10 10)
        // \\def b X (+ a 1)
        // \\def X Type T
        // \\def T Type Int
        // \\b
        // ,

        // \\// simple function
        // \\def my-add (Fn [Int, Int] Int)
        // \\    fn [a, b] (+ a b)
        // \\
        // \\my-add 45 56
        // ,

        // \\// conditional
        // \\[
        // \\  (if true 1 0),
        // \\  (if false 1 0),
        // \\]
        // ,
    };

    for (tests) |@"test"| testing: {
        // print test input
        try stdout.writeAll("[Test]\n");

        var lines = std.mem.tokenize(u8, @"test", "\n");
        while (lines.next()) |line| try stdout.print("> {s}\n", .{line});

        try stdout.writeAll("\n");

        // run test
        const handle = try context.addExternalSource("test", @"test");

        plumbing.execute(ally, handle) catch |e| {
            if (e == error.FluentError) {
                try context.flushMessages();
                try stdout.writeAll("test failed.\n\n");

                continue;
            } else {
                try stdout.print(
                    "test encountered compiler error: {}{s}{}\n",
                    .{&kz.Format{ .fg = .red }, @errorName(e), &kz.Format{}}
                );

                break :testing;
            }
        };
        try stdout.writeAll("test succeeded.\n\n");
    }
}

/// the output of argument parsing
const Command = union(enum) {
    const Self = @This();
    const Enum = @typeInfo(Self).Union.tag_type.?;

    help: void,
    repl: void,
    tests: void,
    run: []const u8,

    const Meta = struct {
        desc: []const u8,
        params: []const []const u8 = &.{},
    };
    const MetaTable = util.EnumTable(Enum, Meta, .{
        .{.help, Meta{ .desc = "display this prompt" }},
        .{.repl, Meta{ .desc = "start interactive mode" }},
        .{.tests, Meta{ .desc = "run internal tests" }},
        .{.run, Meta{
            .desc = "run a program dynamically",
            .params = &[_][]const u8{"file"}
        }},
    });

    // maps name -> tag
    const tag_map = std.ComptimeStringMap(Enum, pairs: {
        const KV = struct {
            @"0": []const u8,
            @"1": Enum
        };

        const fields = @typeInfo(Enum).Enum.fields;
        var entries: [fields.len]KV = undefined;
        for (fields) |field, i| {
            entries[i] = KV{
                .@"0" = field.name,
                .@"1" = @intToEnum(Enum, field.value),
            };
        }

        break :pairs entries;
    });
};

fn print_help() @TypeOf(stdout).Error!void {
    try stdout.print("commands:\n\n", .{});

    for (std.enums.values(Command.Enum)) |tag| {
        const meta = Command.MetaTable.get(tag);
        try stdout.print("{s:<16}{s}\n", .{@tagName(tag), meta.desc});
    }
}

fn execute_file(ally: Allocator, path: []const u8) !void {
    // required backing stuff
    const handle = context.loadSource(path) catch |e| {
        if (e == error.FileNotFound) {
            try stderr.print("could not find file at '{s}'.\n", .{path});
            return;
        } else return e;
    };

    // time execution
    plumbing.execute(ally, handle) catch |e| {
        try stdout.print("execution failed: {}\n", .{e});
    };

    try stdout.print("execution succeeded.\n", .{});
}

const CommandError = error {
    BadArgs,
};

fn read_args(ally: Allocator) ![][]u8 {
    var arg_iter = try std.process.argsWithAllocator(ally);
    defer arg_iter.deinit();

    var args = std.ArrayList([]u8).init(ally);
    while (arg_iter.next()) |arg| {
        try args.append(try ally.dupe(u8, arg[0..:0]));
    }

    return args.toOwnedSlice();
}

fn parse_args(ally: Allocator) !Command {
    const args = try read_args(ally);
    defer {
        for (args) |arg| ally.free(arg);
        ally.free(args);
    }

    if (args.len < 2) return CommandError.BadArgs;

    const tag = Command.tag_map.get(args[1]) orelse {
        return CommandError.BadArgs;
    };
    const meta = Command.MetaTable.get(tag);

    if (args.len != 2 + meta.params.len) return CommandError.BadArgs;

    return switch (tag) {
        .help => Command{ .help = {} },
        .repl => Command{ .repl = {} },
        .tests => Command{ .tests = {} },
        .run => Command{ .run = try ally.dupe(u8, args[2]) },
    };
}

pub fn main() !void {
    // boilerplate stuff
    // var gpa = std.heap.GeneralPurposeAllocator(.{
        // .stack_trace_frames = 1000,
    // }){};
    // defer _ = gpa.deinit();
    // const ally = gpa.allocator();
    const ally = std.heap.page_allocator;

    context.init(ally);
    defer context.deinit();

    // the rest of the fucking owl
    const cmd = parse_args(ally) catch |e| err: {
        if (e == CommandError.BadArgs) {
            break :err Command{ .help = {} };
        }

        return e;
    };

    switch (cmd) {
        .help => try print_help(),
        .repl => try repl(ally),
        .tests => try fluent_tests(ally),
        .run => |path| {
            try execute_file(ally, path);
            ally.free(path);
        }
    }
}