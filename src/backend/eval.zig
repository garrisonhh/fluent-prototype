//! the world that fluent revolves around

const std = @import("std");
const stdout = std.io.getStdOut().writer();
const kz = @import("kritzler");
const com = @import("common");
const now = com.now;
const Name = com.Name;
const Message = com.Message;
const SExpr = @import("sexpr.zig");
const Env = @import("env.zig");
const analyze = @import("sema.zig").analyze;
// const lower = @import("lower.zig").lower;
// const compile = @import("compile.zig").compile;
// const run = @import("bytecode/vm.zig").run;
const canon = @import("canon.zig");
const Value = canon.Value;
const Type = canon.Type;
const TypeId = canon.TypeId;
const Repr = canon.Repr;
const ReprWelt = canon.ReprWelt;
const Object = canon.Object;
const Expr = canon.Expr;
const Image = canon.Image;

pub const Error =
    std.mem.Allocator.Error ||
    ReprWelt.Error ||
    Image.AllocError ||
    @TypeOf(stdout).Error;

pub const Result = Message.Result(Expr);

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
    const expr = sema_res.get() orelse return sema_res;
    defer expr.deinit();

    if (com.options.log.sema) {
        const t = now();
        defer render_time += now() - t;

        try stdout.writeAll("[Analyzed AST]\n");
        try kz.display(env.ally, {}, expr, stdout);
        try stdout.writeByte('\n');
    }

    // time logging
    const duration = now() - start - render_time;

    try stdout.print("eval finished in {d:.6}ms", .{duration});
    if (render_time > 0) {
        try stdout.print(" (render time {d:.6}ms)", .{render_time});
    }
    try stdout.writeAll(".\n");

    // TODO the rest of the fucking eval
    return Result.ok(try expr.clone());
}
