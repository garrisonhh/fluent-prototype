const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");
const Name = com.Name;
const Loc = com.Loc;
const Env = @import("env.zig");
const SExpr = @import("sexpr.zig");
const canon = @import("canon.zig");
const Object = canon.Object;
const TypeId = canon.TypeId;
const Type = canon.Type;
const Expr = canon.Expr;
const Basic = canon.Basic;
const Image = canon.Image;
const Ptr = canon.Ptr;

pub const Error = Object.InitError || Image.AllocError;
const Result = com.Message.Result(Expr);

const ok = Result.ok;

fn err(
    ally: Allocator,
    loc: ?Loc,
    comptime fmt: []const u8,
    args: anytype,
) Allocator.Error!Result {
    return Result.err(try com.Message.print(ally, .@"error", loc, fmt, args));
}

fn holeError(env: Env, loc: ?Loc, ty: TypeId) Allocator.Error!Result {
    return err(env.ally, loc, "this hole expects {}", .{ty});
}

fn expectError(
    env: Env,
    loc: ?Loc,
    expected: TypeId,
    found: TypeId,
) Allocator.Error!Result {
    const fmt = "expected {}, found {}";
    return err(env.ally, loc, fmt, .{ expected, found });
}

/// numbers become their fluent counterparts
fn analyzeNumber(
    env: *Env,
    sexpr: SExpr,
    outward: TypeId,
) Error!Result {
    const number = sexpr.data.number;

    const ty = try env.identify(Type{
        .number = .{ .bits = number.bits, .layout = number.data },
    });

    const expr = try Expr.init(env);
    expr.set(.type, ty);

    const data = expr.get(.data);
    switch (number.data) {
        inline else => |n, tag| {
            const field_tag = @field(@TypeOf(data).I.Tag, @tagName(tag));
            data.set(field_tag, n);
        },
    }

    return try coerce(env, expr, outward);
}

/// strings become arrays of bytes
fn analyzeString(
    env: *Env,
    sexpr: SExpr,
    outward: TypeId,
) Error!Result {
    const string = sexpr.data.string.str;

    // clone string to image
    const mem = try env.alloc(.heap, string.len * @sizeOf(u8));
    const owned = env.img.intoSlice(mem, u8, string.len);
    std.mem.copy(u8, owned, string);

    // create [N]u8 type
    const ty = try env.identify(Type{
        .array = .{ .size = string.len, .of = Basic.u8.get() },
    });

    // create expr
    const expr = try Expr.init(env);
    expr.set(.type, ty);

    const data = expr.get(.data);
    data.setInto(.string).setInto(mem, string.len);

    return try coerce(env, expr, outward);
}

fn analyzeExpr(
    env: *Env,
    scope: Name,
    sexpr: SExpr,
    outward: TypeId,
) Error!Result {
    _ = scope;

    return switch (sexpr.data) {
        .number => analyzeNumber(env, sexpr, outward),
        .string => analyzeString(env, sexpr, outward),
        else => |tag| std.debug.panic("TODO analyze {}", .{tag}),
    };
}

/// given an Object, ensure that it will properly coerce to its expectation
///
/// this will either:
/// a) do nothing (return the same expr)
/// b) make an implicit cast explicit
/// c) find that an expectation was violated and produce a nice error message
fn coerce(env: *Env, expr: Expr, outward: TypeId) Error!Result {
    const expr_ty = expr.get(.type);

    // TypeId comparison is fastest
    if (expr_ty.eql(outward)) {
        return Result.ok(expr);
    }

    // check for an allowed implicit cast
    const inner = env.tw.get(expr_ty);
    const outer = env.tw.get(outward);

    const method = (try inner.coercesTo(env.ally, &env.tw, outer.*)) orelse {
        // no coercion :(
        return expectError(env.*, null, outward, expr_ty);
    };

    return switch (method) {
        .inbounds => in: {
            // nothing needs to be done
            break :in Result.ok(expr);
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
