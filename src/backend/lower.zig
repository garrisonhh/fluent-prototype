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
    ally: Allocator,
    func: *Func,
    block: Label,
    ty: TypeId,
    value: Value
) Error!Local {
    const @"const" = try func.addConst(ally, value);
    const out = try func.addLocal(ally, ty);
    try func.addOp(ally, block, Op{ .ldc = .{ .a = @"const", .to = out } });

    return out;
}

/// wrapper for loadConst which lowers a raw uint as some type
fn lowerCanonical(
    ally: Allocator,
    func: *Func,
    block: Label,
    ty: TypeId,
    n: u64
) Error!Local {
    const value = try Value.init(ally, canon.fromCanonical(&n));
    return try lowerLoadConst(ally, func, block, ty, value);
}

/// this does all of the boilerplate of loading a const u64. this is
/// boilerplate I otherwise end up rewriting constantly.
fn lowerU64(env: *Env, func: *Func, block: Label, n: u64) Error!Local {
    const @"u64" = try env.identify(Type{
        .number = .{ .bits = 64, .layout = .uint }
    });
    return try lowerCanonical(env.ally, func, block, @"u64", n);
}

fn lowerBool(
    ally: Allocator,
    func: *Func,
    block: Label,
    expr: TExpr
) Error!Local {
    const byte: u8 = if (expr.data.@"bool") 1 else 0;
    const value = try Value.init(ally, std.mem.asBytes(&byte));

    return lowerLoadConst(ally, func, block, expr.ty, value);
}

fn lowerNumber(
    ally: Allocator,
    func: *Func,
    block: Label,
    expr: TExpr
) Error!Local {
    const value = try expr.data.number.asValue(ally);
    return lowerLoadConst(ally, func, block, expr.ty, value);
}

/// lower builtins that connect directly to ssa IR ops
fn lowerOperator(
    env: *Env,
    func: *Func,
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
        locals[i] = try lowerExpr(env, func, block, arg);
    }

    // generate operation
    return switch (b) {
        // binary ops
        inline .add, .sub, .mul, .div, .mod, .@"and", .@"or", .fn_ty
            => |tag| bin: {
            // TODO make sure this is checked in sema
            std.debug.assert(args.len == 2);

            const to = try func.addLocal(ally, returns);
            const op = @unionInit(Op, @tagName(tag), Op.Binary{
                .a = locals[0],
                .b = locals[1],
                .to = to
            });
            try func.addOp(ally, block.*, op);

            break :bin to;
        },
        // unary ops
        inline .not, .cast, .slice_ty => |tag| un: {
            // TODO make sure this is checked in sema
            std.debug.assert(args.len == 1);

            const to = try func.addLocal(ally, returns);
            const op = @unionInit(Op, @tagName(tag), Op.Unary{
                .a = locals[0],
                .to = to
            });
            try func.addOp(ally, block.*, op);

            break :un to;
        },
        else => unreachable
    };
}

fn lowerArray(env: *Env, func: *Func, block: *Label, expr: TExpr) Error!Local {
    const ally = env.ally;
    const exprs = expr.data.array;

    // lower all elements
    const elements = try ally.alloc(Local, exprs.len);
    defer ally.free(elements);

    for (exprs) |child, i| {
        elements[i] = try lowerExpr(env, func, block, child);
    }

    // alloca array
    const arr_ptr_ty = try env.identify(Type.initPtr(.single, expr.ty));
    const arr_ptr = try func.addLocal(ally, arr_ptr_ty);

    try func.addOp(ally, block.*, Op{
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

    var cur_ptr = try func.addLocal(ally, el_ptr_ty);
    try func.addOp(ally, block.*, Op{
        .cast = .{ .a = arr_ptr, .to = cur_ptr }
    });

    if (elements.len == 1) {
        // single element arrays only need 1 store operation
        try func.addOp(ally, block.*, Op{
            .store = .{ .a = elements[0], .b = cur_ptr }
        });

        return arr_ptr;
    }

    // get element size in the code as a constant
    const el_size = try lowerU64(env, func, block.*, env.sizeOf(el_ty));

    // store each element
    for (elements) |elem, i| {
        // store this element at the current array ptr
        try func.addOp(ally, block.*, Op{
            .store = .{ .a = elem, .b = cur_ptr }
        });

        // increment the current ptr, unless this is the last element
        if (i < elements.len - 1) {
            const next_ptr = try func.addLocal(ally, el_ptr_ty);
            try func.addOp(ally, block.*, Op{
                .add = .{ .a = cur_ptr, .b = el_size, .to = next_ptr }
            });

            cur_ptr = next_ptr;
        }
    }

    return arr_ptr;
}

/// lowers the operation of taking the stack address of a subexpr
fn lowerAddrOf(env: *Env, func: *Func, block: *Label, expr: TExpr) Error!Local {
    // lower subexpr
    const subexpr = expr.data.ptr.*;
    const sub = try lowerExpr(env, func, block, subexpr);

    // with structured data, lowering automatically places the data on the stack
    const lty = env.tw.get(func.getLocal(sub));
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
fn lowerLambda(env: *Env, func: *Func, block: *Label, expr: TExpr) Error!Local {
    // lower function expr
    const ty = env.tw.get(expr.ty);
    std.debug.assert(ty.* == .func);

    const lambda = try lowerFunction(
        env,
        expr.data.func.name,
        ty.func.takes,
        ty.func.returns,
        expr.data.func.body.*,
    );

    // TODO remove vvv
    {
        var ctx = kz.Context.init(env.ally);
        defer ctx.deinit();

        const ref = lambda.render(&ctx, env.*) catch unreachable;

        const stdout = std.io.getStdOut().writer();
        stdout.writeAll("[lowered lambda]\n") catch {};
        ctx.write(ref, stdout) catch {};
    }

    // compile func ref
    // TODO this is done here to ensure the bytecode ordering is correct, but
    // that is pretty hacky ngl
    _ = try env.compileSsa(lambda);

    // load func ref as callable constant
    const index = lambda.ref.index;
    return try lowerCanonical(env.ally, func, block.*, expr.ty, index);
}

fn lowerCall(env: *Env, func: *Func, block: *Label, expr: TExpr) Error!Local {
    const ally = env.ally;
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
                => |b| try lowerOperator(env, func, block, b, tail, expr.ty),
            .@"fn" => try lowerLambda(env, func, block, expr),
            else => |b| {
                std.debug.panic("TODO lower builtin {s}", .{@tagName(b)});
            }
        };
    }

    // lower all children
    var locals_buf: [256]Local = undefined;
    const locals = locals_buf[0..exprs.len];

    for (exprs) |child, i| {
        locals[i] = try lowerExpr(env, func, block, child);
    }

    // lower args
    for (locals[1..]) |local, i| {
        try func.addOp(ally, block.*, Op{
            .arg = .{ .arg = i, .from = local }
        });
    }

    // lower call and return value
    const returns = env.tw.get(head.ty).func.returns;
    const dst = try func.addLocal(ally, returns);
    try func.addOp(ally, block.*, Op{ .call = .{ .a = locals[0], .to = dst } });

    return dst;
}

fn lowerExpr(env: *Env, func: *Func, block: *Label, expr: TExpr) Error!Local {
    return switch (expr.data) {
        .@"bool" => try lowerBool(env.ally, func, block.*, expr),
        .number => try lowerNumber(env.ally, func, block.*, expr),
        // TODO named values should be cacheable and lowered only once, really.
        // afaik this will currently place the same named value multiple times
        // in the code.
        .name => |name| try lowerExpr(env, func, block, env.get(name)),
        .array => try lowerArray(env, func, block, expr),
        .call => try lowerCall(env, func, block, expr),
        .ty => |ty| ty: {
            const tyty = try env.identify(Type{ .ty = {} });
            break :ty lowerCanonical(env.ally, func, block.*, tyty, ty.index);
        },
        .ptr => try lowerAddrOf(env, func, block, expr),
        .func => try lowerLambda(env, func, block, expr),
        .param => |p| param: {
            // TODO analyze parameter references to ensure no closures are
            // created
            std.debug.assert(p.func.eql(func.name));
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
    returns: TypeId,
    body: TExpr
) Error!Func {
    // create func and entry block
    var func = try env.prog.addFunc(env.ally, name, takes, returns);
    var block = try func.addBlock(env.ally);

    // lowering expr will return the final value
    const final = try lowerExpr(env, &func, &block, body);
    try func.addOp(env.ally, block, Op{ .ret = .{ .a = final } });
    // func.returns = func.getLocal(final); // TODO what was this for?

    std.debug.assert(func.returns.eql(func.getLocal(final)));

    return func;
}

/// lowers expression as if it is the body of a function with no args
pub fn lower(env: *Env, scope: Name, expr: TExpr) Error!Func {
    return try lowerFunction(env, scope, &.{}, expr.ty, expr);
}
