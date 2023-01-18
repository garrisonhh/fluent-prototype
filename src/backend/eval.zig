//! the world that fluent revolves around

const std = @import("std");
const stdout = std.io.getStdOut().writer();
const kz = @import("kritzler");
const util = @import("util");
const Name = util.Name;
const Project = util.Project;
const Message = util.Message;
const TExpr = @import("texpr.zig");
const SExpr = @import("sexpr.zig");
const Env = @import("env.zig");
const types = @import("types.zig");
const Type = types.Type;
const TypeId = types.TypeId;
const analyze = @import("sema.zig").analyze;
const lower = @import("lower.zig").lower;
const compile = @import("compile.zig").compile;
const run = @import("bytecode/vm.zig").run;
const canon = @import("canon.zig");
const Value = canon.Value;

pub const Error =
    std.mem.Allocator.Error
 || canon.ResError
 || @TypeOf(stdout).Error;

pub const Result = Message.Result(TExpr);

/// evaluate any dynamic value in the provided scope
pub fn eval(env: *Env, scope: Name, sexpr: SExpr) Error!Result {
    const any = try env.identify(Type{ .any = {} });
    return try evalTyped(env, scope, sexpr, any);
}

/// evaluate in the provided scope, expecting a type
pub fn evalTyped(
    env: *Env,
    scope: Name,
    sexpr: SExpr,
    expected: TypeId
) Error!Result {
    const now = std.time.nanoTimestamp;
    const start = now();
    var render_time: i128 = 0;

    // analyze
    const sema_res = try analyze(env, scope, sexpr, expected);
    const texpr = sema_res.get() orelse return sema_res;
    defer texpr.deinit(env.ally);

    if (util.options.log.sema) {
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
            if (util.options.log.sema) {
                const msg_start = now();
                try stdout.writeAll("analyzed a constant.\n");
                render_time += now() - msg_start;
            }

            break :final try texpr.clone(env.ally);
        }

        // lower to ssa ir
        const ssa = try lower(env, scope, texpr);

        if (util.options.log.ssa) {
            const t = now();
            defer render_time += now() - t;

            try stdout.writeAll("[SSA Program]\n");
            try kz.display(env.ally, env.*, env.prog, stdout);
            try stdout.writeByte('\n');
        }

        // compile to bytecode
        _ = try compile(env, ssa);

        const prog = env.bc.build(ssa);

        if (util.options.log.bytecode) {
            const t = now();
            defer render_time += now() - t;

            try stdout.writeAll("[Bytecode]\n");
            try kz.display(env.ally, env.bc.comments, prog, stdout);
            try stdout.writeByte('\n');
        }

        // run compiled bytecode
        const final = try env.run(prog, texpr.loc, env.getFunc(ssa).returns);

        // remove ssa expr
        try env.removeFunc(ssa);

        // when returning structured data, the vm may return a pointer to the
        // data I actually wanted
        const derefed = if (!final.ty.eql(texpr.ty)) deref: {
            const out_ty = env.tw.get(final.ty);
            std.debug.assert(out_ty.ptr.to.eql(texpr.ty));

            const child = final.data.ptr.*;
            env.ally.destroy(final.data.ptr);

            break :deref child;
        } else final;

        return Result.ok(derefed);
    };

    // render final value
    if (util.options.log.eval) {
        const t = now();
        defer render_time += now() - t;

        try stdout.writeAll("[Value]\n");
        try kz.display(env.ally, env.*, final, stdout);
        try stdout.writeByte('\n');
    }

    // time logging
    const stop = std.time.nanoTimestamp();
    const seconds = @intToFloat(f64, stop - start - render_time) * 1e-9;

    try stdout.print("eval finished in {d:.6}s.\n", .{seconds});
    if (render_time > 0) {
        const render_secs = @intToFloat(f64, render_time) * 1e-9;
        try stdout.print("render time {d:.6}s.\n", .{render_secs});
    }

    return Result.ok(final);
}