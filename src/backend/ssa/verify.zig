//! verification steps for ssa lowering. errors generated are not user-facing
//! errors but internal compiler errors, indicating something has gone wrong
//! in lowering TExprs to SSA. since this step has so much surface area, doing
//! sanity checks in debug makes sense

const std = @import("std");
const Allocator = std.mem.Allocator;
const ssa = @import("ssa.zig");
const Op = ssa.Op;
const Local = ssa.Local;
const Func = ssa.Func;
const Env = @import("../env.zig");
const canon = @import("../canon.zig");
const ReprWelt = canon.ReprWelt;
const ReprId = canon.ReprId;
const Repr = canon.Repr;

pub const Error = Allocator.Error || error{
    LargeLocal,
    WrongArgLen,
    MismatchedReprs,
    InvalidOffset,
};

/// ssa values should all clock in at up to 8 bytes
fn verifyReprSizes(rw: ReprWelt, func: *const Func) Error!void {
    for (func.locals.items) |local| {
        const size = rw.sizeOf(local);
        if (size > 8) {
            std.debug.print("local {} is {} bytes\n", .{ local, size });
            return Error.LargeLocal;
        }
    }
}

fn expectArgLen(args: []const Local, n: usize) Error!void {
    if (args.len != n) return Error.WrongArgLen;
}

fn expectEql(from: ReprId, to: ReprId) Error!void {
    if (!from.eql(to)) {
        return Error.MismatchedReprs;
    }
}

fn expectHomogenous(
    func: *const Func,
    to: Local,
    from: []const Local,
) Error!void {
    const to_repr = func.getLocal(to);
    for (from) |param| {
        try expectEql(to_repr, func.getLocal(param));
    }
}

fn verifyOp(env: *Env, func: *const Func, op: Op) Error!void {
    switch (op) {
        // unverifiable
        .br,
        .jmp,
        .phi,
        => {},

        // unique ops
        .ldc => |ldc| {
            const const_repr = try env.reprOf(func.getConst(ldc.a).ty);
            try expectEql(func.getLocal(ldc.to), const_repr);
        },
        .ret => |ret| {
            try expectArgLen(ret.params, 1);

            const ret_ty = env.tw.get(func.ty).func.returns;
            const ret_repr = try env.reprOf(ret_ty);

            const arg = func.getLocal(ret.params[0]);
            try expectEql(arg, ret_repr);
        },
        .store, .load => |eff| {
            const args = eff.params;
            try expectArgLen(args, 2);

            const ref = env.rw.get(func.getLocal(args[0]));
            const data = func.getLocal(args[1]);

            if (ref.* != .ptr or !ref.ptr.eql(data)) {
                return Error.MismatchedReprs;
            }
        },

        // homogenous pure binary ops
        .add,
        .sub,
        .mul,
        .div,
        .mod,
        .shl,
        .shr,
        .eq,
        .@"or",
        .@"and",
        .slice_ty,
        => |pure| {
            try expectArgLen(pure.params, 2);
            try expectHomogenous(func, pure.to, pure.params);
        },

        // homogenous pure unary ops
        .copy,
        .not,
        => |pure| {
            try expectArgLen(pure.params, 1);
            try expectHomogenous(func, pure.to, pure.params);
        },

        else => std.debug.panic("TODO verify ssa op {s}", .{@tagName(op)}),
    }
}

fn verifyOps(env: *Env, func: *const Func) Error!void {
    for (func.blocks.items) |block| {
        for (block.ops.items) |op| {
            verifyOp(env, func, op) catch |e| {
                std.debug.print("in {s}\n", .{op});
                return e;
            };
        }
    }
}

pub fn verify(env: *Env, func: *const Func) Error!void {
    try verifyReprSizes(env.rw, func);
    try verifyOps(env, func);
}
