//! the world that fluent revolves around

const std = @import("std");
const stdout = std.io.getStdOut().writer();
const builtin = @import("builtin");
const kz = @import("kritzler");
const util = @import("util");
const Name = util.Name;
const TExpr = @import("texpr.zig");
const SExpr = @import("sexpr.zig");
const Env = @import("env.zig");
const Type = @import("types.zig").Type;
const analyze = @import("sema.zig").analyze;
const lower = @import("lower.zig").lower;
const compile = @import("compile.zig").compile;
const run = @import("bytecode/vm.zig").run;

/// evaluate in the provided scope.
pub fn eval(env: *Env, scope: Name, sexpr: SExpr) !TExpr {
    const now = std.time.nanoTimestamp;
    const start = now();
    var render_time: i128 = 0;

    // analyze
    const any = try env.identify(Type{ .any = {} });
    const texpr = try analyze(env, scope, sexpr, any);
    defer texpr.deinit(env.ally);

    if (builtin.mode == .Debug) {
        const t = now();
        var ctx = kz.Context.init(env.ally);
        defer ctx.deinit();

        const tex = try texpr.render(&ctx, env.tw);

        try stdout.writeAll("[Analyzed AST]\n");
        try ctx.write(tex, stdout);
        try stdout.writeByte('\n');

        render_time += now() - t;
    }

    // lower to ssa ir
    var ssa = try lower(env.ally, env, texpr);
    defer ssa.deinit(env.ally);

    if (builtin.mode == .Debug) {
        const t = now();
        var ctx = kz.Context.init(env.ally);
        defer ctx.deinit();

        const tex = try ssa.render(&ctx, env);

        try stdout.writeAll("[SSA Program]\n");
        try ctx.write(tex, stdout);
        try stdout.writeByte('\n');

        render_time += now() - t;
    }

    // compile to bytecode
    const bc = try compile(env.ally, env.tw, ssa);
    defer bc.deinit(env.ally);

    if (builtin.mode == .Debug) {
        const t = now();
        var ctx = kz.Context.init(env.ally);
        defer ctx.deinit();

        const tex = try bc.render(&ctx);

        try stdout.writeAll("[Bytecode]\n");
        try ctx.write(tex, stdout);
        try stdout.writeByte('\n');

        render_time += now() - t;
    }

    // run compiled bytecode
    const return_ty = bc.returns;
    const value = try run(env.ally, &env.tw, bc);
    defer value.deinit(env.ally);

    const final = try value.resurrect(env.*, return_ty);

    // render final value
    if (builtin.mode == .Debug) {
        const t = now();

        var ctx = kz.Context.init(env.ally);
        defer ctx.deinit();

        const tex = try final.render(&ctx, env.tw);

        try stdout.writeAll("[Value]\n");
        try ctx.write(tex, stdout);
        try stdout.writeByte('\n');

        render_time += now() - t;
    }

    // time logging
    const stop = std.time.nanoTimestamp();
    const seconds = @intToFloat(f64, stop - start - render_time) * 1e-9;

    try stdout.print("eval finished in {d:.6}s.\n", .{seconds});
    if (render_time > 0) {
        const render_secs = @intToFloat(f64, render_time) * 1e-9;
        try stdout.print("render time {d:.6}s.\n", .{render_secs});
    }

    return final;
}