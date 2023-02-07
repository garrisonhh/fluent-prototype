const std = @import("std");
const Allocator = std.mem.Allocator;
const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();
const builtin = @import("builtin");
const kz = @import("kritzler");
const linenoize = @import("linenoize");
const Linenoise = linenoize.Linenoise;
const com = @import("common");
const cli = com.cli;
const FileRef = com.FileRef;
const Project = com.Project;
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

/// put the `ep` in repl
fn execPrint(
    env: *Env,
    proj: Project,
    ref: FileRef,
    what: ParseType,
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

        try stdout.writeAll("[Type Reprs]\n");
        var i: usize = 0;
        while (i < env.tw.types.items.len) : (i += 1) {
            var ctx = kz.Context.init(ally);
            defer ctx.deinit();

            const ty = backend.TypeId{ .index = i };
            switch (env.tw.get(ty).*) {
                .hole, .namespace, .builtin, .any, .set, .func => {
                    const tex = try ctx.slap(
                        try ty.render(&ctx, env.tw),
                        try ctx.print(.{ .fg = .red }, "no repr", .{}),
                        .right,
                        .{ .space = 1 },
                    );
                    try ctx.write(tex, stdout);

                    continue;
                },
                else => {},
            }

            if (try env.reprOf(ty)) |repr| {
                const sz = env.rw.get(repr).sizeOf(env.rw);
                const aln = env.rw.get(repr).alignOf(env.rw);

                const tex = try ctx.stack(
                    &.{
                        try ty.render(&ctx, env.tw),
                        try ctx.print(.{}, "->", .{}),
                        try repr.render(&ctx, env.rw),
                        try ctx.print(.{}, "(sz {}, aln {})", .{ sz, aln }),
                    },
                    .right,
                    .{ .space = 1 },
                );

                try ctx.write(tex, stdout);
            }
        }
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

const LOG_FIELDS = @typeInfo(@TypeOf(com.options.log)).Struct.fields;

fn addLogFlags(parser: *cli.Parser) !void {
    inline for (LOG_FIELDS) |field| {
        const name = "log-" ++ field.name;
        const desc = "enable logging for the " ++ field.name ++ " stage";
        try parser.addArg(null, name, name, desc, .flag);
    }
}

fn applyLogFlags(options: *const cli.Options) void {
    inline for (LOG_FIELDS) |field| {
        const name = "log-" ++ field.name;
        if (options.get(name) != null) {
            @field(com.options.log, field.name) = true;
        }
    }
}

fn replHook(opts: *const cli.Options) anyerror!void {
    applyLogFlags(opts);

    try repl(&ENV, &PROJ);
}

fn runHook(opts: *const cli.Options) anyerror!void {
    applyLogFlags(opts);

    const filepath = opts.get("file").?.string;
    const file = try PROJ.load(ENV.ally, filepath);
    try execPrint(&ENV, PROJ, file, .file);
}

fn zenHook(_: *const cli.Options) anyerror!void {
    try stdout.writeAll(ZEN);
}

var ENV: Env = undefined;
var PROJ: Project = undefined;

pub fn main() !void {
    // allocator boilerplate
    // var gpa = std.heap.GeneralPurposeAllocator(.{
    // .stack_trace_frames = 1000,
    // }){};
    // defer _ = gpa.deinit();
    // const ally = gpa.allocator();
    const ally = std.heap.page_allocator;

    // cli input
    var parser = try cli.Parser.init(
        ally,
        "fluent",
        "the fluent compiler",
        null,
        .exit_process,
    );
    defer parser.deinit();

    const repl_cmd = try parser.addSubcommand(
        "repl",
        "start the fluent repl",
        replHook,
    );
    try addLogFlags(repl_cmd);

    const run_cmd = try parser.addSubcommand(
        "run",
        "run a fluent program",
        runHook,
    );
    try addLogFlags(run_cmd);
    try run_cmd.addPositional("file", "the file to run", .string);

    _ = try parser.addSubcommand("zen", "display the fluent zen", null);

    // fluent globals
    try frontend.init(ally);
    defer frontend.deinit(ally);

    // fluent env
    ENV = try backend.generatePrelude(ally);
    defer ENV.deinit();

    PROJ = com.Project.init();
    defer PROJ.deinit(ENV.ally);

    try parser.run();
}
