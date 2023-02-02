//! verification steps for ssa lowering. errors generated are not user-facing
//! errors but internal compiler errors, indicating something has gone wrong
//! in lowering TExprs to SSA. since this step has a ton of surface area,
//! doing sanity checks in debug makes sense.

const std = @import("std");
const ssa = @import("ssa.zig");
const Func = ssa.Func;
const Env = @import("../env.zig");

pub const Error = error{
    ConstTooLarge,
};

/// ssa values should all clock in at up to 8 bytes
fn verifyConstSizes(env: Env, func: *const Func) Error!void {
    for (func.consts.items) |expr| {
        if (env.sizeOf(expr.ty) > 8) {
            return Error.ConstTooLarge;
        }
    }
}

pub fn verify(env: Env, func: *const Func) Error!void {
    try verifyConstSizes(env, func);
}
