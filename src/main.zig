const std = @import("std");
const Allocator = std.mem.Allocator;
const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();
const builtin = @import("builtin");
const kz = @import("kritzler");
const linenoize = @import("linenoize");
const Linenoise = linenoize.Linenoise;
const util = @import("util");
const cli = util.cli;
const FileRef = util.FileRef;
const Project = util.Project;
const plumbing = @import("plumbing.zig");
const backend = @import("backend.zig");
const Env = backend.Env;
const frontend = @import("frontend.zig");
const ParseType = frontend.ParseType;

const ZEN =
    \\(wip)
    \\
    \\programs should be:
    \\- easy to reason about.
    \\- well-understood by your tools.
    \\- more declarative, more explicit, and more beautiful.
    \\
;

// this test ensures that all code is semantically analyzed
test {
    std.testing.refAllDeclsRecursive(@This());
}

/// put the `ep` in repl. reusable by `fluent run`.
fn execPrint(
    env: *Env,
    proj: Project,
    ref: FileRef,
    what: ParseType
) !void {
    const ally = env.ally;
    const parse = @import("frontend.zig").parse;

    switch (try parse(ally, proj, ref, what)) {
        .ok => |rexpr| {
            defer rexpr.deinit(ally);
            try stdout.print("[parsed]\n", .{});
            try kz.display(ally, proj, rexpr, stdout);
        },
        .err => |msg| {
            try kz.display(ally, proj, msg, stderr);
        },
    }

    // switch (try plumbing.exec(proj, env, ref, what)) {
        // .ok => |value| {
            // defer value.deinit(env.ally);
            // try kz.display(env.ally, env.*, value, stdout);
        // },
        // .err => |msg| {
            // try kz.display(env.ally, proj, msg, stderr);
        // },
    // }
}

/// look for a `.fluentinit` script to run on repl startup
fn runFluentInit(env: *Env, proj: *Project) !void {
    const ally = env.ally;

    const file = proj.load(ally, ".fluentinit") catch return;
    try execPrint(env, proj.*, file, .file);
}

fn repl(env: *Env, proj: *Project) !void {
    const ally = env.ally;

    try runFluentInit(env, proj);

    if (builtin.mode == .Debug) {
        try stdout.writeAll("[Env]\n");
        try env.dump(ally, stdout);
        try stdout.writeByte('\n');
    }

    var ln = Linenoise.init(ally);
    defer ln.deinit();

    while (try ln.linenoise("> ")) |line| {
        try ln.history.add(line);

        const input = try proj.register(ally, "repl", line);
        try execPrint(env, proj.*, input, .expr);
    }
}

const LOG_FIELDS = @typeInfo(@TypeOf(util.options.log)).Struct.fields;

fn addLogFlags(parser: *cli.Parser) !void {
    inline for (LOG_FIELDS) |field| {
        const name = "log-" ++ field.name;
        const desc = "enable logging for the " ++ field.name ++ " stage";
        try parser.addArg(null, name, name, desc, .flag);
    }
}

fn checkLogFlags(options: *const cli.Output.Options) void {
    inline for (LOG_FIELDS) |field| {
        const name = "log-" ++ field.name;
        if (options.get(name) != null) {
            @field(util.options.log, field.name) = true;
        }
    }
}

fn dispatchArgs(
    parser: cli.Parser,
    output: cli.Output,
    env: *Env,
    proj: *Project,
) !void {
    try output.filterHelp(&parser);

    if (output.matches(&.{})) {
        try parser.usageExit();
    } else if (output.match(&.{"repl"})) |options| {
        checkLogFlags(options);

        try repl(env, proj);
    } else if (output.match(&.{"run"})) |options| {
        checkLogFlags(options);

        const filepath = options.get("file").?.string;
        const file = try proj.load(env.ally, filepath);
        try execPrint(env, proj.*, file, .file);
    } else if (output.match(&.{"zen"})) |_| {
        try stdout.writeAll(ZEN);
    } else {
        unreachable;
    }
}

pub fn main() !void {
    // allocator boilerplate
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .stack_trace_frames = 1000,
    }){};
    defer _ = gpa.deinit();
    // const ally = gpa.allocator();
    const ally = std.heap.page_allocator;

    // cli input
    var parser = try cli.Parser.init(ally, "fluent", "the fluent compiler");
    defer parser.deinit();

    try parser.addHelp();

    const repl_cmd = try parser.addSubcommand("repl", "start the fluent repl");
    try repl_cmd.addHelp();
    try addLogFlags(repl_cmd);

    const run_cmd = try parser.addSubcommand("run", "run a fluent program");
    try run_cmd.addHelp();
    try addLogFlags(run_cmd);
    try run_cmd.addPositional("file", "the file to run", .string);

    const zen_cmd = try parser.addSubcommand("zen", "display the fluent zen");
    try zen_cmd.addHelp();

    var output = try parser.parse();
    defer output.deinit(ally);

    // fluent globals
    try frontend.init(ally);
    defer frontend.deinit(ally);

    // fluent env
    var env = try backend.generatePrelude(ally);
    defer env.deinit();

    var proj = util.Project.init();
    defer proj.deinit(env.ally);

    try dispatchArgs(parser, output, &env, &proj);
}
