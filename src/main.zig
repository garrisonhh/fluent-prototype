const std = @import("std");
const Allocator = std.mem.Allocator;
const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();
const builtin = @import("builtin");
const kz = @import("kritzler");
const util = @import("util");
const context = @import("context.zig");
const plumbing = @import("plumbing.zig");
const backend = @import("backend.zig");
const Env = backend.Env;

// TODO get rid of this and use the zig linenoise port I contributed to
const c = @cImport({
    @cInclude("linenoise.h");
});

/// returns file
fn replRead(ally: Allocator) !context.FileHandle {
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

    // every read needs to have a unique namespace
    const C = struct { var reads: usize = 0; };

    var tmp: [32]u8 = undefined;
    const filename = std.fmt.bufPrint(&tmp, "repl-{d}", .{C.reads}) catch {
        unreachable;
    };
    C.reads += 1;

    return context.addExternalSource(filename, buf.items);
}

/// look for a `.fluentinit` script to run on repl startup
fn runFluentInit(ally: Allocator, env: *Env) !void {
    const PATH = ".fluentinit";
    const dir = std.fs.cwd();

    dir.access(PATH, .{ .mode = .read_only }) catch return;
    try executeFile(ally, env, PATH);
}

fn repl(ally: Allocator, env: *Env) !void {
    try runFluentInit(ally, env);

    loop: while (true) {
        const input = try replRead(ally);

        // check for empty input (exit program)
        var is_empty: bool = true;
        for (input.getSource()) |ch| {
            if (!std.ascii.isSpace(ch)) {
                is_empty = false;
                break;
            }
        }

        if (is_empty) break;

        // eval and print any messages
        const value = plumbing.exec(env, input, .expr) catch |e| {
            if (e == error.FluentError) {
                try context.flushMessages();
                continue :loop;
            } else {
                return e;
            }
        };
        defer value.deinit(ally);

        // render
        var ctx = kz.Context.init(ally);
        defer ctx.deinit();

        const tex = try value.render(&ctx, env.*);

        try ctx.write(tex, stdout);
        try stdout.writeByte('\n');
    }
}

/// the output of argument parsing
const Command = union(enum) {
    const Self = @This();
    const Enum = @typeInfo(Self).Union.tag_type.?;

    help: void,
    repl: void,
    run: []const u8,

    const Meta = struct {
        desc: []const u8,
        params: []const []const u8 = &.{},
    };
    const MetaTable = util.EnumTable(Enum, Meta, .{
        .{.help, Meta{ .desc = "display this prompt" }},
        .{.repl, Meta{ .desc = "start interactive mode" }},
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

fn printHelp() @TypeOf(stdout).Error!void {
    try stdout.print("commands:\n\n", .{});

    for (std.enums.values(Command.Enum)) |tag| {
        const meta = Command.MetaTable.get(tag);
        try stdout.print("{s:<16}{s}\n", .{@tagName(tag), meta.desc});
    }
}

fn executeFile(ally: Allocator, env: *Env, path: []const u8) !void {
    // required backing stuff
    const handle = context.loadSource(path) catch |e| {
        if (e == error.FileNotFound) {
            try stderr.print("could not find file at '{s}'.\n", .{path});
            return;
        } else return e;
    };

    // time execution
    const value = plumbing.exec(env, handle, .file) catch |e| {
        if (e == error.FluentError) {
            try context.flushMessages();
            return;
        } else {
            return e;
        }
    };
    defer value.deinit(env.ally);

    // render
    var ctx = kz.Context.init(ally);
    defer ctx.deinit();

    const tex = try value.render(&ctx, env.*);

    try ctx.write(tex, stdout);
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

fn parseArgs(ally: Allocator) !Command {
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
        .run => Command{ .run = try ally.dupe(u8, args[2]) },
    };
}

pub fn main() !void {
    // boilerplate stuff
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .stack_trace_frames = 1000,
    }){};
    defer _ = gpa.deinit();
    // const ally = gpa.allocator();
    const ally = std.heap.raw_c_allocator;

    try context.init(ally);
    defer context.deinit();

    var prelude = try backend.generatePrelude(ally);
    defer prelude.deinit();

    // the rest of the fucking owl
    const cmd = parseArgs(ally) catch |e| err: {
        if (e == CommandError.BadArgs) {
            break :err Command{ .help = {} };
        }

        return e;
    };

    if (builtin.mode == .Debug) {
        // display prelude as a sanity check
        try stdout.writeAll("[Prelude]\n");
        try prelude.dump(ally, stdout);
        try stdout.writeByte('\n');
    }

    switch (cmd) {
        .help => try printHelp(),
        .repl => try repl(ally, &prelude),
        .run => |path| {
            try executeFile(ally, &prelude, path);
            ally.free(path);
        }
    }
}