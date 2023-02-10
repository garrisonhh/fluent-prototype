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
const TypeId = canon.TypeId;
const Type = canon.Type;

pub const Error =
    Allocator.Error ||
    Repr.AccessError ||
    ReprWelt.ConversionError ||
    ReprWelt.QualError ||
    error{
    LargeLocal,
    InvalidArgLen,
    InvalidOffset,
    InvalidCallArgs,
    MismatchedReprs,
    MismatchedAllocaSize,
};

/// ssa values should all clock in at up to 8 bytes
fn verifyReprSizes(rw: ReprWelt, func: *const Func) Error!void {
    for (func.locals.items) |local| {
        const size = try rw.sizeOf(local);
        if (size > 8) {
            std.debug.print("local {} is {} bytes\n", .{ local, size });
            return Error.LargeLocal;
        }
    }
}

fn expectArgLen(args: []const Local, n: usize) Error!void {
    if (args.len != n) return Error.InvalidArgLen;
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
        .vcall, .rcall => {
            // TODO verify calls
        },
        .ret => |ret| {
            const args = ret.params;
            try expectArgLen(args, 1);

            const ret_ty = env.tw.get(func.ty).func.returns;
            const ret_repr = try env.reprOf(ret_ty);

            const arg = func.getLocal(args[0]);
            try expectEql(arg, ret_repr);
        },
        .alloca => |all| {
            const repr = env.rw.get(func.getLocal(all.to));

            if (repr.* != .ptr) {
                return Error.MismatchedReprs;
            } else if (try env.rw.sizeOf(repr.ptr) != all.size) {
                return Error.MismatchedAllocaSize;
            }
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
        .store_el, .load_el => |access| {
            const obj = env.rw.get(func.getLocal(access.obj));
            const field = try obj.access(env.rw, access.index);
            const data = func.getLocal(access.data);

            try expectEql(field.of, data);
        },
        .fn_ty => |pure| {
            const args = pure.params;
            try expectArgLen(args, 2);
            const fn_params = func.getLocal(args[0]);
            const fn_returns = func.getLocal(args[1]);
            const to = func.getLocal(pure.to);

            const type_ty = try env.identify(.ty);
            const type_slice_ty = try env.identify(Type{
                .ptr = .{ .kind = .slice, .to = type_ty },
            });
            const type_repr = try env.reprOf(type_ty);
            const type_slice_repr = try env.reprOf(type_slice_ty);

            try expectEql(fn_params, type_slice_repr);
            try expectEql(fn_returns, type_repr);
            try expectEql(to, type_repr);
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

        // else => std.debug.panic("TODO verify ssa op {s}", .{@tagName(op)}),
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
