//! the world that fluent revolves around

const std = @import("std");
const stdout = std.io.getStdOut().writer();
const kz = @import("kritzler");
const com = @import("common");
const now = com.now;
const Name = com.Name;
const Message = com.Message;
const TExpr = @import("texpr.zig");
const SExpr = @import("sexpr.zig");
const Env = @import("env.zig");
const analyze = @import("sema.zig").analyze;
const lower = @import("lower.zig").lower;
const compile = @import("compile.zig").compile;
const run = @import("bytecode/vm.zig").run;
const canon = @import("canon.zig");
const Value = canon.Value;
const Type = canon.Type;
const TypeId = canon.TypeId;
const Repr = canon.Repr;
const ReprWelt = canon.ReprWelt;

pub const Error =
    std.mem.Allocator.Error ||
    canon.ResError ||
    Repr.Error ||
    @TypeOf(stdout).Error;

pub const Result = Message.Result(TExpr);

/// evaluate any dynamic value in the provided scope
pub fn eval(env: *Env, scope: Name, sexpr: SExpr) Error!Result {
    const any = try env.identify(.any);
    return try evalTyped(env, scope, sexpr, any);
}

/// evaluate in the provided scope, expecting a type
pub fn evalTyped(
    env: *Env,
    scope: Name,
    sexpr: SExpr,
    expected: TypeId,
) Error!Result {
    const start = now();
    var render_time: f64 = 0;

    // analyze
    const sema_res = try analyze(env, scope, sexpr, expected);
    const texpr = sema_res.get() orelse return sema_res;
    defer texpr.deinit(env.ally);

    if (com.options.log.sema) {
        const t = now();
        defer render_time += now() - t;

        try stdout.writeAll("[Analyzed AST]\n");
        try kz.display(env.ally, env.*, texpr, stdout);
        try stdout.writeByte('\n');
    }

    // analysis may produce a constant, otherwise the expr must be lowered,
    // compiled, and executed on the virtual machine
    const final = final: {
        // values don't need further execution
        if (texpr.known_const) {
            if (com.options.log.sema) {
                const msg_start = now();
                defer render_time += now() - msg_start;

                try stdout.writeAll("analyzed a constant.\n");
            }

            break :final try texpr.clone(env.ally);
        }

        // lower to ssa ir
        const ssa = try lower(env, scope, texpr);

        if (com.options.log.ssa) {
            const t = now();
            defer render_time += now() - t;

            try stdout.writeAll("[SSA Program]\n");
            try kz.display(env.ally, env.*, env.prog, stdout);
            try stdout.writeByte('\n');
        }

        // compile to bytecode
        _ = try compile(env, ssa);

        const prog = env.bc.build(ssa);

        if (com.options.log.bytecode) {
            const t = now();
            defer render_time += now() - t;

            try stdout.writeAll("[Bytecode]\n");
            try kz.display(env.ally, env.bc.comments, prog, stdout);
            try stdout.writeByte('\n');
        }

        // run compiled bytecode
        const final = try env.run(prog, texpr.loc, texpr.ty);

        // remove ssa expr
        try env.removeFunc(ssa);

        break :final final;
    };

    // render final value
    if (com.options.log.eval) {
        const t = now();
        defer render_time += now() - t;

        try stdout.writeAll("[Value]\n");
        try kz.display(env.ally, env.*, final, stdout);
        try stdout.writeByte('\n');
    }

    // time logging
    const duration = now() - start - render_time;

    try stdout.print("eval finished in {d:.6}ms", .{duration});
    if (render_time > 0) {
        try stdout.print(" (render time {d:.6}ms)", .{render_time});
    }
    try stdout.writeAll(".\n");

    return Result.ok(final);
}
