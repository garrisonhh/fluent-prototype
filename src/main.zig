const std = @import("std");
const Allocator = std.mem.Allocator;
const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();
const builtin = @import("builtin");
const kz = @import("kritzler");
const linenoize = @import("linenoize");
const Linenoise = linenoize.Linenoise;
const linenoiseEdit = linenoize.linenoiseEdit;
const util = @import("util");
const FileRef = util.FileRef;
const Project = util.Project;
const Message = util.Message;
const plumbing = @import("plumbing.zig");
const backend = @import("backend.zig");
const Env = backend.Env;
const EvalError = backend.eval.Error;
const ParseType = @import("frontend.zig").ParseType;

// this test ensures that all code is semantically analyzed
test {
    std.testing.refAllDeclsRecursive(@This());
}

/// put the `ep` in repl. reusable by `fluent run`.
fn execPrint(
    proj: Project,
    env: *Env,
    ref: FileRef,
    what: ParseType
) !void {
    switch (try plumbing.exec(proj, env, ref, what)) {
        .ok => |value| {
            defer value.deinit(env.ally);
            try kz.display(env.ally, env.*, value, stdout);
        },
        .err => |msg| {
            try kz.display(env.ally, proj, msg, stderr);
        },
    }
}

/// look for a `.fluentinit` script to run on repl startup
fn runFluentInit(proj: *Project, env: *Env) !void {
    const file = proj.load(".fluentinit") catch return;
    try execPrint(proj.*, env, file, .file);
}

fn repl(ally: Allocator, proj: *Project, env: *Env) !void {
    try runFluentInit(proj, env);

    if (builtin.mode == .Debug) {
        try stdout.writeAll("[Env]\n");
        try env.dump(ally, stdout);
        try stdout.writeByte('\n');
    }

    var ln = Linenoise.init(ally);
    defer ln.deinit();

    while (try ln.linenoise("> ")) |line| {
        try ln.history.add(line);

        const input = try proj.register("repl", line);
        try execPrint(proj.*, env, input, .expr);
    }
}

/// the output of argument parsing
const Command = union(enum) {
    const Self = @This();
    const Tag = @typeInfo(Self).Union.tag_type.?;

    help: void,
    repl: void,
    run: []const u8,

    const Meta = struct {
        desc: []const u8,
        params: []const []const u8 = &.{},
    };

    const meta = meta: {
        var map = std.enums.EnumArray(Tag, Meta).initUndefined();
        map.set(.help, .{ .desc = "display this prompt" });
        map.set(.repl, .{ .desc = "start interactive mode" });
        map.set(.run, .{
            .desc = "run a program dynamically",
            .params = &[_][]const u8{"file"},
        });

        break :meta map;
    };

    const tags = std.ComptimeStringMap(Tag, pairs: {
        const KV = struct {
            @"0": []const u8,
            @"1": Tag
        };

        const fields = @typeInfo(Tag).Enum.fields;
        var entries: [fields.len]KV = undefined;
        for (fields) |field, i| {
            entries[i] = KV{
                .@"0" = field.name,
                .@"1" = @intToEnum(Tag, field.value),
            };
        }

        break :pairs entries;
    });
};

fn printHelp() @TypeOf(stdout).Error!void {
    try stdout.print("commands:\n\n", .{});

    for (std.enums.values(Command.Tag)) |tag| {
        const meta = Command.meta.get(tag);
        try stdout.print("{s:<16}{s}\n", .{@tagName(tag), meta.desc});
    }
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

/// TODO I should probably reroll this into util
fn parseArgs(ally: Allocator) !Command {
    const args = try read_args(ally);
    defer {
        for (args) |arg| ally.free(arg);
        ally.free(args);
    }

    if (args.len < 2) return CommandError.BadArgs;

    const tag = Command.tags.get(args[1]) orelse {
        return CommandError.BadArgs;
    };
    const meta = Command.meta.get(tag);

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
    const ally = std.heap.page_allocator;

    var proj = util.Project.init(ally);
    defer proj.deinit();

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
        .repl => try repl(ally, &proj, &prelude),
        .run => |path| {
            defer ally.free(path);

            const ref = try proj.load(path);
            try execPrint(proj, &prelude, ref, .file);
        }
    }
}