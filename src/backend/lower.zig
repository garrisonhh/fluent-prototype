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
const Value = @import("value.zig");
const canon = @import("canon.zig");
const Builtin = canon.Builtin;

pub const Error = Allocator.Error;

fn lowerLoadConst(
    env: *Env,
    ref: FuncRef,
    block: Label,
    ty: TypeId,
    value: Value
) Error!Local {
    const @"const" = try ref.addConst(env, value);
    const out = try ref.addLocal(env, ty);
    try ref.addOp(env, block, Op{ .ldc = .{ .a = @"const", .to = out } });

    return out;
}

/// wrapper for loadConst which lowers a raw uint as some type
fn lowerCanonical(
    env: *Env,
    ref: FuncRef,
    block: Label,
    ty: TypeId,
    n: u64
) Error!Local {
    const value = try Value.init(env.ally, canon.fromCanonical(&n));
    return try lowerLoadConst(env, ref, block, ty, value);
}

/// this does all of the boilerplate of loading a const u64. this is
/// boilerplate I otherwise end up rewriting constantly.
fn lowerU64(env: *Env, ref: FuncRef, block: Label, n: u64) Error!Local {
    const @"u64" = try env.identify(Type{
        .number = .{ .bits = 64, .layout = .uint }
    });
    return try lowerCanonical(env, ref, block, @"u64", n);
}

fn lowerBool(env: *Env, ref: FuncRef, block: Label, expr: TExpr) Error!Local {
    const byte: u8 = if (expr.data.@"bool") 1 else 0;
    const value = try Value.init(env.ally, std.mem.asBytes(&byte));
    return lowerLoadConst(env, ref, block, expr.ty, value);
}

fn lowerNumber(env: *Env, ref: FuncRef, block: Label, expr: TExpr) Error!Local {
    const value = try expr.data.number.asValue(env.ally);
    return lowerLoadConst(env, ref, block, expr.ty, value);
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
    defer ally.free(locals);

    for (args) |arg, i| {
        locals[i] = try lowerExpr(env, ref, block, arg);
    }

    // generate operation
    return switch (b) {
        // binary ops
        inline .add, .sub, .mul, .div, .mod, .@"and", .@"or", .fn_ty
            => |tag| bin: {
            // TODO make sure this is checked in sema
            std.debug.assert(args.len == 2);

            const to = try ref.addLocal(env, returns);
            const op = @unionInit(Op, @tagName(tag), Op.Binary{
                .a = locals[0],
                .b = locals[1],
                .to = to
            });
            try ref.addOp(env, block.*, op);

            break :bin to;
        },
        // unary ops
        inline .not, .cast, .slice_ty => |tag| un: {
            // TODO make sure this is checked in sema
            std.debug.assert(args.len == 1);

            const to = try ref.addLocal(env, returns);
            const op = @unionInit(Op, @tagName(tag), Op.Unary{
                .a = locals[0],
                .to = to
            });
            try ref.addOp(env, block.*, op);

            break :un to;
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
    try ref.addOp(env, block.*, Op{
        .cast = .{ .a = arr_ptr, .to = cur_ptr }
    });

    if (elements.len == 1) {
        // single element arrays only need 1 store operation
        try ref.addOp(env, block.*, Op{
            .store = .{ .a = elements[0], .b = cur_ptr }
        });

        return arr_ptr;
    }

    // get element size in the code as a constant
    const el_size = try lowerU64(env, ref, block.*, env.sizeOf(el_ty));

    // store each element
    for (elements) |elem, i| {
        // store this element at the current array ptr
        try ref.addOp(env, block.*, Op{
            .store = .{ .a = elem, .b = cur_ptr }
        });

        // increment the current ptr, unless this is the last element
        if (i < elements.len - 1) {
            const next_ptr = try ref.addLocal(env, el_ptr_ty);
            try ref.addOp(env, block.*, Op{
                .add = .{ .a = cur_ptr, .b = el_size, .to = next_ptr }
            });

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
    return try lowerCanonical(env, ref, block.*, expr.ty, lambda.index);
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

    // lower args
    for (locals[1..]) |local, i| {
        try ref.addOp(env, block.*, Op{
            .arg = .{ .arg = i, .from = local }
        });
    }

    // lower call and return value
    const returns = env.tw.get(head.ty).func.returns;
    const dst = try ref.addLocal(env, returns);
    try ref.addOp(env, block.*, Op{ .call = .{ .a = locals[0], .to = dst } });

    return dst;
}

fn lowerExpr(env: *Env, ref: FuncRef, block: *Label, expr: TExpr) Error!Local {
    return switch (expr.data) {
        .@"bool" => try lowerBool(env, ref, block.*, expr),
        .number => try lowerNumber(env, ref, block.*, expr),
        // TODO named values should be cacheable and lowered only once, really.
        // afaik this will currently place the same named value multiple times
        // in the code.
        .name => |name| try lowerExpr(env, ref, block, env.get(name)),
        .array => try lowerArray(env, ref, block, expr),
        .call => try lowerCall(env, ref, block, expr),
        .ty => |ty| ty: {
            const tyty = try env.identify(Type{ .ty = {} });
            break :ty lowerCanonical(env, ref, block.*, tyty, ty.index);
        },
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
    try ref.addOp(env, block, Op{ .ret = .{ .a = final } });

    // return value won't be known until lowering figures it out
    env.getFunc(ref).returns = ref.getLocal(env.*, final);

    if (builtin.mode == .Debug) {
        const func = env.getFunc(ref);
        std.debug.assert(func.returns.eql(ref.getLocal(env.*, final)));
    }

    return ref;
}

/// lowers expression as if it is the body of a function with no args
pub fn lower(env: *Env, scope: Name, expr: TExpr) Error!FuncRef {
    return try lowerFunction(env, scope, &.{}, expr);
}
