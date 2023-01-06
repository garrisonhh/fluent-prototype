//! implements lowering the analyzed AST into SSA IR

const std = @import("std");
const Allocator = std.mem.Allocator;
const kz = @import("kritzler");
const builtin = @import("builtin");
const util = @import("util");
const Symbol = util.Symbol;
const Name = util.Name;
const ssa = @import("ssa.zig");
const Const = ssa.Const;
const Local = ssa.Local;
const Op = ssa.Op;
const Label = ssa.Label;
const Func = ssa.Func;
const FuncRef = ssa.FuncRef;
const types = @import("types.zig");
const TypeId = types.TypeId;
const Type = types.Type;
const Env = @import("env.zig");
const TExpr = @import("texpr.zig");
const canon = @import("canon.zig");
const Builtin = canon.Builtin;

pub const Error = Allocator.Error;

fn lowerLoadConst(
    env: *Env,
    ref: FuncRef,
    block: Label,
    expr: TExpr
) Error!Local {
    std.debug.assert(expr.known_const);

    const @"const" = try ref.addConst(env, expr);
    const out = try ref.addLocal(env, expr.ty);
    try ref.addOp(env, block, Op{ .ldc = .{ .a = @"const", .to = out } });

    return out;
}

/// this does all of the boilerplate of loading a const u64. this is
/// boilerplate I otherwise end up rewriting constantly.
fn lowerU64(env: *Env, ref: FuncRef, block: Label, n: u64) Error!Local {
    const @"u64" = try env.identify(Type{
        .number = .{ .bits = 64, .layout = .uint }
    });
    const expr = TExpr.init(null, true, @"u64", .{
        .number = TExpr.Number{
            .bits = 64,
            .data = .{ .uint = n },
        },
    });
    return try lowerLoadConst(env, ref, block, expr);
}

/// lower builtins that connect directly to ssa IR ops
fn lowerOperator(
    env: *Env,
    ref: FuncRef,
    block: *Label,
    b: Builtin,
    args: []const TExpr,
    returns: TypeId
) Error!Local {
    const ally = env.ally;

    // lower args
    const locals = try ally.alloc(Local, args.len);
    for (args) |arg, i| {
        locals[i] = try lowerExpr(env, ref, block, arg);
    }

    // generate operation
    return switch (b) {
        // binary ops
        inline .add, .sub, .mul, .div, .mod, .@"and", .@"or", .fn_ty, .not,
        .cast, .slice_ty
            => |tag| op: {
            // TODO make sure this is checked in sema
            const to = try ref.addLocal(env, returns);
            const op = @unionInit(Op, @tagName(tag), Op.Pure{
                .to = to,
                .params = locals,
            });
            try ref.addOp(env, block.*, op);

            break :op to;
        },
        else => unreachable
    };
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
    const arr_ptr_ty = try env.identify(Type.initPtr(.single, expr.ty));
    const arr_ptr = try ref.addLocal(env, arr_ptr_ty);

    try ref.addOp(env, block.*, Op{
        .alloca = .{
            .size = env.sizeOf(expr.ty),
            .to = arr_ptr
        }
    });

    // for a zero-length array, everything is done
    if (elements.len == 0) return arr_ptr;

    // get ptr to first element
    const el_ty = env.tw.get(expr.ty).array.of;
    const el_ptr_ty = try env.identify(Type.initPtr(.single, el_ty));

    var cur_ptr = try ref.addLocal(env, el_ptr_ty);
    const cast_op = try Op.initPure(ally, .cast, cur_ptr, &.{arr_ptr});
    try ref.addOp(env, block.*, cast_op);

    if (elements.len == 1) {
        // single element arrays only need 1 store operation
        const sto = try Op.initImpure(ally, .store, &.{elements[0], cur_ptr});
        try ref.addOp(env, block.*, sto);

        return arr_ptr;
    }

    // get element size in the code as a constant
    const el_size = try lowerU64(env, ref, block.*, env.sizeOf(el_ty));

    // store each element
    for (elements) |elem, i| {
        // store this element at the current array ptr
        const sto = try Op.initImpure(ally, .store, &.{elem, cur_ptr});
        try ref.addOp(env, block.*, sto);

        // increment the current ptr, unless this is the last element
        if (i < elements.len - 1) {
            const next_ptr = try ref.addLocal(env, el_ptr_ty);
            const add =
                try Op.initPure(ally, .add, next_ptr, &.{cur_ptr, el_size});
            try ref.addOp(env, block.*, add);

            cur_ptr = next_ptr;
        }
    }

    return arr_ptr;
}

/// lowers the operation of taking the stack address of a subexpr
fn lowerAddrOf(env: *Env, ref: FuncRef, block: *Label, expr: TExpr) Error!Local {
    // lower subexpr
    const subexpr = expr.data.ptr.*;
    const sub = try lowerExpr(env, ref, block, subexpr);

    // with structured data, lowering automatically places the data on the stack
    const lty = env.tw.get(ref.getLocal(env.*, sub));
    if (lty.* == .ptr and lty.ptr.kind == .single
    and lty.ptr.to.eql(subexpr.ty)) {
        // super trivial!
        return sub;
    }

    @panic("TODO lowering taking the address of something not automatically "
        ++ "stack allocated");
}

/// functions get lowered as their own Func and then stored in the env. for
/// the current block, their FuncRef can then get lowered in.
fn lowerLambda(
    env: *Env,
    ref: FuncRef,
    block: *Label,
    expr: TExpr
) Error!Local {
    // lower function expr
    const ty = env.tw.get(expr.ty);
    std.debug.assert(ty.* == .func);

    const lambda = try lowerFunction(
        env,
        expr.data.func.name,
        ty.func.takes,
        expr.data.func.body.*,
    );

    // compile func ref
    // TODO this is done here to ensure the bytecode ordering is correct, but
    // that is pretty hacky ngl
    _ = try env.compileSsa(lambda);

    // load func ref as callable constant
    const final = TExpr.init(expr.loc, true, expr.ty, .{ .func_ref = lambda });
    return try lowerLoadConst(env, ref, block.*, final);
}

fn lowerCall(env: *Env, ref: FuncRef, block: *Label, expr: TExpr) Error!Local {
    const exprs = expr.data.call;
    std.debug.assert(exprs.len > 0);

    // get true head
    const raw_head = exprs[0];
    const head = switch (raw_head.data) {
        .name => |name| env.get(name),
        else => raw_head
    };

    const tail = exprs[1..];

    // builtins have their own logic
    if (head.data == .builtin) {
        return switch (head.data.builtin) {
            .add, .sub, .mul, .div, .mod, .@"and", .@"or", .not, .cast,
            .slice_ty, .fn_ty
                => |b| try lowerOperator(env, ref, block, b, tail, expr.ty),
            .@"fn" => try lowerLambda(env, ref, block, expr),
            else => |b| {
                std.debug.panic("TODO lower builtin {s}", .{@tagName(b)});
            }
        };
    }

    // lower all children
    var locals_buf: [256]Local = undefined;
    const locals = locals_buf[0..exprs.len];

    for (exprs) |child, i| {
        locals[i] = try lowerExpr(env, ref, block, child);
    }

    // lower call and return value
    const returns = env.tw.get(head.ty).func.returns;
    const dst = try ref.addLocal(env, returns);
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
        .builtin => |b| {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "attempted to lower builtin `{s}` through lowerExpr",
                    .{@tagName(b)}
                );
            } else unreachable;
        },
        else => {
            const tag = @tagName(expr.data);
            std.debug.panic("TODO lower {s} exprs\n", .{tag});
        }
    };
}

/// everything in fluent ssa exists in the context of a function, so this is the
/// dispatcher of the rest of lowering
fn lowerFunction(
    env: *Env,
    name: Name,
    takes: []const TypeId,
    body: TExpr
) Error!FuncRef {
    // create func and entry block
    var ref = try env.prog.add(env.ally, name, takes);
    var block = try ref.addBlock(env);

    // lowering expr will return the final value
    const final = try lowerExpr(env, ref, &block, body);
    try ref.addOp(env, block, try Op.initImpure(env.ally, .ret, &.{final}));

    // return value is unknown until this point
    env.getFunc(ref).returns = ref.getLocal(env.*, final);

    return ref;
}

/// lowers expression as if it is the body of a function with no args
pub fn lower(env: *Env, scope: Name, expr: TExpr) Error!FuncRef {
    return try lowerFunction(env, scope, &.{}, expr);
}
