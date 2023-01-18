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

const LOG_FIELDS = @typeInfo(@TypeOf(util.options.log)).Struct.fields;

fn addLogArgs(parser: *cli.Parser) !void {
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
    ally: Allocator,
    parser: cli.Parser,
    output: cli.Output,
    proj: *Project,
    env: *Env
) !void {
    try output.filterHelp(&parser);

    if (output.matches(&.{})) {
        try parser.usageExit();
    } else if (output.match(&.{"repl"})) |options| {
        checkLogFlags(options);

        try repl(ally, proj, env);
    } else if (output.match(&.{"run"})) |options| {
        checkLogFlags(options);

        const filepath = options.get("file").?.string;
        const file = try proj.load(filepath);
        try execPrint(proj.*, env, file, .file);
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
    try addLogArgs(repl_cmd);

    const run_cmd = try parser.addSubcommand("run", "run a fluent program");
    try run_cmd.addHelp();
    try addLogArgs(run_cmd);
    try run_cmd.addPositional("file", "the file to run", .string);

    var output = try parser.parse();
    defer output.deinit(ally);

    // fluent env
    var proj = util.Project.init(ally);
    defer proj.deinit();

    var env = try backend.generatePrelude(ally);
    defer env.deinit();

    try dispatchArgs(ally, parser, output, &proj, &env);
}
