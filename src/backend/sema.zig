const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");
const Name = com.Name;
const Loc = com.Loc;
const Env = @import("env.zig");
const canon = @import("canon.zig");
const Object = canon.Object;
const TypeId = canon.TypeId;
const SExpr = @import("sexpr.zig");

const Error = Object.InitError;
const Result = com.Message.Result(Object);

const ok = Result.ok;

fn err(
    ally: Allocator,
    loc: ?Loc,
    comptime fmt: []const u8,
    args: anytype,
) Result {
    return Result.err(try com.Message.print(ally, .@"error", loc, fmt, args));
}

pub fn analyze(
    env: *Env,
    scope: Name,
    sexpr: SExpr,
    outward: TypeId,
) Error!Result {
    _ = scope;
    _ = sexpr;
    _ = outward;

    return ok(try Object.fromUnit(env));
}
