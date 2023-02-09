//! implements lowering the analyzed AST into SSA IR

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const com = @import("common");
const Name = com.Name;
const ssa = @import("ssa/ssa.zig");
const Local = ssa.Local;
const Op = ssa.Op;
const Label = ssa.Label;
const Func = ssa.Func;
const FuncRef = ssa.FuncRef;
const Env = @import("env.zig");
const TExpr = @import("texpr.zig");
const canon = @import("canon.zig");
const TypeId = canon.TypeId;
const Type = canon.Type;
const ReprWelt = canon.ReprWelt;
const ReprId = canon.ReprId;
const Repr = canon.Repr;
const Builtin = canon.Builtin;

// TODO read everything in this file again to ensure it's making use of repr
// properly

pub const Error = Allocator.Error;

fn lowerLoadConst(
    env: *Env,
    ref: FuncRef,
    block: Label,
    expr: TExpr,
) Error!Local {
    std.debug.assert(expr.known_const);

    const @"const" = try ref.addConst(env, expr);
    const out = try ref.addLocal(env, try env.reprOf(expr.ty));
    try ref.addOp(env, block, Op{ .ldc = .{ .a = @"const", .to = out } });

    return out;
}

/// this does all of the boilerplate of loading a const u64. this is
/// boilerplate I otherwise end up rewriting constantly.
fn lowerU64(env: *Env, ref: FuncRef, block: Label, n: u64) Error!Local {
    const @"u64" = try env.identify(Type{
        .number = .{ .bits = 64, .layout = .uint },
    });
    const expr = TExpr.init(null, true, @"u64", .{
        .number = TExpr.Number{
            .bits = 64,
            .data = .{ .uint = n },
        },
    });
    return try lowerLoadConst(env, ref, block, expr);
}

/// allocates some repr on the stack and returns a pointer to it
fn lowerAlloca(
    env: *Env,
    ref: FuncRef,
    block: Label,
    repr: ReprId,
) Error!Local {
    const size = env.rw.sizeOf(repr);

    const ptr_repr = try env.rw.intern(env.ally, Repr{ .ptr = repr });
    const ptr = try ref.addLocal(env, ptr_repr);

    // TODO use coll repr to model stack layout. this is a very dumb solution
    // to fix stack alignment issues
    const BOUNDARY = 8;
    const aln_diff = size % BOUNDARY;
    const aln_size = if (aln_diff > 0) size + (BOUNDARY - aln_diff) else size;

    try ref.addOp(env, block, Op{ .alloca = .{ .size = aln_size, .to = ptr } });

    return ptr;
}

fn lowerArray(env: *Env, ref: FuncRef, block: *Label, expr: TExpr) Error!Local {
    const ally = env.ally;
    const exprs = expr.data.array;

    // lower all elements
    const elements = try ally.alloc(Local, exprs.len);
    defer ally.free(elements);

    for (exprs) |child, i| {
        elements[i] = try lowerExpr(env, ref, block, child);
    }

    // alloca array
    const arr_repr = try env.reprOf(expr.ty);
    const arr_ptr = try lowerAlloca(env, ref, block.*, arr_repr);

    // store each element
    for (elements) |elem, i| {
        // store this element at the current array ptr
        try ref.addOp(env, block.*, Op{ .store_el = .{
            .index = i,
            .obj = arr_ptr,
            .data = elem,
        } });
    }

    return arr_ptr;
}

/// lowers the operation of taking the address of a subexpr
fn lowerAddrOf(
    env: *Env,
    ref: FuncRef,
    block: *Label,
    expr: TExpr,
) Error!Local {
    // lower subexpr
    const subexpr = expr.data.ptr.*;
    const subrepr = try env.reprOf(subexpr.ty);
    const sub = try lowerExpr(env, ref, block, subexpr);
    const repr = env.rw.get(ref.getLocal(env.*, sub));

    // with a structured data, data with automatically be placed on the stack
    // and taking its address is trivial
    if (repr.* == .ptr and repr.ptr.eql(subrepr)) {
        return sub;
    }

    @panic("TODO lowering taking the address of something not stack allocated");
}

/// functions get lowered as their own Func and then stored in the env. for
/// the current block, their FuncRef can then get lowered in.
fn lowerLambda(
    env: *Env,
    ref: FuncRef,
    block: *Label,
    expr: TExpr,
) Error!Local {
    // lower function expr
    std.debug.assert(env.tw.get(expr.ty).* == .func);

    const name = expr.data.func.name;
    const body = expr.data.func.body;
    const lambda = try lowerFunction(env, name, expr.ty, body.*);

    // load func ref as callable constant
    const final = TExpr.init(expr.loc, true, expr.ty, .{ .func_ref = lambda });
    return try lowerLoadConst(env, ref, block.*, final);
}

/// if exprs get lowered into two branching blocks
fn lowerIf(env: *Env, ref: FuncRef, block: *Label, expr: TExpr) Error!Local {
    const ally = env.ally;
    const exprs = expr.data.call;

    // lower branch op
    var branches = [2]Label{
        try ref.addBlock(env),
        try ref.addBlock(env),
    };
    const final_block = try ref.addBlock(env);

    const cond = try lowerExpr(env, ref, block, exprs[1]);
    try ref.addOp(env, block.*, Op{
        .br = .{ .cond = cond, .a = branches[0], .b = branches[1] },
    });

    // lower branches, jump to final block
    for (branches) |*branch, i| {
        const local = try lowerExpr(env, ref, branch, exprs[2 + i]);
        try ref.addOp(env, branch.*, Op{
            .jmp = .{ .data = local, .dst = final_block },
        });
    }

    // final block with phi
    const final = try ref.addLocal(env, try env.reprOf(expr.ty));
    try ref.addOp(env, final_block, try Op.initPhi(ally, final, &branches));

    block.* = final_block;

    return final;
}

fn lowerArrayPtrToSlice(
    env: *Env,
    ref: FuncRef,
    block: *Label,
    expr: TExpr,
) Error!Local {
    const child = expr.data.call[1];

    // get length and ptr
    const array_ptr_ty = env.tw.get(child.ty);
    const array_ty = env.tw.get(array_ptr_ty.ptr.to);
    const array_len = array_ty.array.size;

    const len = try lowerU64(env, ref, block.*, array_len);
    const ptr = try lowerExpr(env, ref, block, child);

    // alloca space for slice
    const slice_repr = try env.reprOf(expr.ty);
    const slice_ptr = try lowerAlloca(env, ref, block.*, slice_repr);

    try ref.addOp(env, block.*, Op{
        .store_el = .{ .index = 0, .obj = slice_ptr, .data = ptr },
    });
    try ref.addOp(env, block.*, Op{
        .store_el = .{ .index = 1, .obj = slice_ptr, .data = len },
    });

    return slice_ptr;
}

fn lowerBuiltin(
    env: *Env,
    ref: FuncRef,
    block: *Label,
    b: Builtin,
    expr: TExpr,
) Error!Local {
    return switch (b) {
        .recur => unreachable,
        .lambda => try lowerLambda(env, ref, block, expr),
        .@"if" => try lowerIf(env, ref, block, expr),
        .array_ptr_to_slice => try lowerArrayPtrToSlice(env, ref, block, expr),
        .cast => cast: {
            const ally = env.ally;
            const child = try lowerExpr(env, ref, block, expr.data.call[1]);
            const to = try ref.addLocal(env, try env.reprOf(expr.ty));

            const op = try Op.initPure(ally, .copy, to, &.{child});
            try ref.addOp(env, block.*, op);

            break :cast to;
        },
        // zig fmt: off
        .eq, .add, .sub, .mul, .div, .mod, .shl, .shr, .@"and", .@"or",
        .not, .slice_ty, .fn_ty
        // zig fmt: on
        => |tag| op: {
            // builtins that directly map to ssa ops
            const args = expr.data.call[1..];
            const locals = try env.ally.alloc(Local, args.len);
            for (args) |arg, i| {
                locals[i] = try lowerExpr(env, ref, block, arg);
            }

            const to = try ref.addLocal(env, try env.reprOf(expr.ty));

            // this convinces the zig compiler that Op will always have this tag
            const op = switch (tag) {
                inline else => |t| t: {
                    const name = @tagName(t);
                    if (!@hasField(Op, name)) unreachable;

                    break :t @unionInit(Op, name, Op.Pure{
                        .to = to,
                        .params = locals,
                    });
                },
            };

            try ref.addOp(env, block.*, op);

            break :op to;
        },
        else => std.debug.panic("TODO lower builtin {s}", .{@tagName(b)}),
    };
}

fn lowerCall(env: *Env, ref: FuncRef, block: *Label, expr: TExpr) Error!Local {
    const exprs = expr.data.call;
    std.debug.assert(exprs.len > 0);

    // get true head
    const raw_head = exprs[0];
    const head = switch (raw_head.data) {
        .name => |name| env.get(name),
        .builtin => |b| b: {
            if (b == .recur) {
                break :b TExpr.initFuncRef(raw_head.loc, raw_head.ty, ref);
            } else {
                break :b raw_head;
            }
        },
        else => raw_head,
    };

    // dispatch on builtins (except for recur)
    if (head.data == .builtin) {
        return try lowerBuiltin(env, ref, block, head.data.builtin, expr);
    }

    // lower all children
    var locals_buf: [256]Local = undefined;
    const locals = locals_buf[0..exprs.len];

    locals[0] = try lowerExpr(env, ref, block, head);
    for (exprs[1..]) |child, i| {
        locals[i + 1] = try lowerExpr(env, ref, block, child);
    }

    // lower call and return value
    const returns = env.tw.get(head.ty).func.returns;
    const dst = try ref.addLocal(env, try env.reprOf(returns));
    const call = try Op.initCall(env.ally, dst, locals[0], locals[1..]);
    try ref.addOp(env, block.*, call);

    return dst;
}

fn lowerExpr(env: *Env, ref: FuncRef, block: *Label, expr: TExpr) Error!Local {
    if (expr.known_const) {
        return try lowerLoadConst(env, ref, block.*, expr);
    }

    return switch (expr.data) {
        // always consts
        .@"bool", .number, .ty => unreachable,
        // has separate functionality
        .builtin => unreachable,
        // TODO named values should be cacheable and lowered only once, really.
        // afaik this will currently place the same named value multiple times
        // in the code.
        .name => |name| try lowerExpr(env, ref, block, env.get(name)),
        .array => try lowerArray(env, ref, block, expr),
        .call => try lowerCall(env, ref, block, expr),
        .ptr => try lowerAddrOf(env, ref, block, expr),
        .func => try lowerLambda(env, ref, block, expr),
        .param => |p| param: {
            // TODO analyze parameter references to ensure no closures are
            // created
            std.debug.assert(p.func.eql(env.getFunc(ref).name));
            break :param Local.of(p.index);
        },
        else => {
            const tag = @tagName(expr.data);
            std.debug.panic("TODO lower {s} exprs\n", .{tag});
        },
    };
}

/// everything in fluent ssa exists in the context of a function, so this is the
/// dispatcher of the rest of lowering
fn lowerFunction(
    env: *Env,
    name: Name,
    ty: TypeId,
    body: TExpr,
) Error!FuncRef {
    // create func and entry block
    var ref = try env.prog.add(env.ally, env, name, ty);
    var block = try ref.addBlock(env);

    // lowering expr will return the final value
    const final = try lowerExpr(env, ref, &block, body);
    try ref.addOp(env, block, try Op.initEffect(env.ally, .ret, &.{final}));

    return ref;
}

/// lowers expression as if it is the body of a function with no args
pub fn lower(env: *Env, scope: Name, expr: TExpr) Error!FuncRef {
    const ty = try env.identify(Type{ .func = .{
        .takes = &.{},
        .returns = expr.ty,
    } });

    const ref = try lowerFunction(env, scope, ty, expr);

    if (builtin.mode == .Debug) {
        const verify = @import("ssa/verify.zig").verify;

        verify(env, env.getFuncConst(ref)) catch |e| {
            std.debug.panic("SSA verify failed: {}", .{e});
        };
    }

    return ref;
}
