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

fn holeError(env: Env, loc: ?Loc, ty: TypeId) Allocator.Error!Result {
    const ally = env.ally;

    const ty_text = try ty.toString(ally, env.tw);
    defer ally.free(ty_text);

    return try err(ally, loc, "this hole expects {s}", .{ty_text});
}

fn expectError(
    env: Env,
    loc: ?Loc,
    expected: TypeId,
    found: TypeId,
) Allocator.Error!Result {
    const ally = env.ally;

    const exp_text = try expected.toString(ally, env.tw);
    defer ally.free(exp_text);
    const found_text = try found.toString(ally, env.tw);
    defer ally.free(found_text);

    const fmt = "expected {s}, found {s}";
    return try err(ally, loc, fmt, .{ exp_text, found_text });
}

fn analyzeNumber(
    env: *Env,
    scope: Name,
    sexpr: SExpr,
    outward: TypeId,
) Error!Result {
    _ = scope;
    _ = outward;

    const number = try Object.fromNumber(env, sexpr.data.number);

    _ = number;

    @panic("TODO");
}

fn analyzeExpr(
    env: *Env,
    scope: Name,
    sexpr: SExpr,
    outward: TypeId,
) Error!Result {
    return switch (sexpr.data) {
        .number => analyzeNumber(env, scope, sexpr, outward),
        else => |tag| std.debug.panic("TODO analyze {}", .{tag}),
    };
}

/// given an Object, ensure that it will properly coerce to its expectation
///
/// this will either:
/// a) do nothing (return the same expr)
/// b) make an implicit cast explicit
/// c) find that an expectation was violated and produce a nice error message
fn coerce(env: *Env, obj: Object, outward: TypeId) Error!Result {
    // TypeId comparison is fastest
    if (obj.ty.eql(outward)) {
        return Result.ok(obj);
    }

    // check for an allowed implicit cast
    const inner = env.tw.get(obj.ty);
    const outer = env.tw.get(outward);

    const method = (try inner.coercesTo(env.ally, &env.tw, outer.*)) orelse {
        // no coercion :(
        return expectError(env.*, null, outward, obj.ty);
    };

    return switch (method) {
        .inbounds => in: {
            // nothing needs to be done
            break :in Result.ok(obj);
        },
        .natural => {
            // cast the expr
            @panic("TODO cast");
        },
        .array_ptr_to_slice => {
            // create a slice from the expr
            @panic("TODO aptr -> slice");
        },
    };
}

pub fn analyze(
    env: *Env,
    scope: Name,
    sexpr: SExpr,
    outward: TypeId,
) Error!Result {
    const res = try analyzeExpr(env, scope, sexpr, outward);

    // TODO postprocessing steps?

    return res;
}
