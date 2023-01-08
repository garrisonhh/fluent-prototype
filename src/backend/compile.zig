const std = @import("std");
const Allocator = std.mem.Allocator;
const kz = @import("kritzler");
const builtin = @import("builtin");
const Env = @import("env.zig");
const ssa = @import("ssa/ssa.zig");
const FuncRef = ssa.FuncRef;
const Label = ssa.Label;
const Local = ssa.Local;
const Pos = ssa.Pos;
const Op = ssa.Op;
const Vm = @import("bytecode/vm.zig");
const Register = Vm.Register;
const bytecode = @import("bytecode/bytecode.zig");
const Inst = bytecode.Inst;
const Bc = bytecode.Construct;
const building = @import("bytecode/building.zig");
const canon = @import("canon.zig");
const Value = canon.Value;
const RegisterMap = @import("bytecode/register_map.zig");
const TExpr = @import("texpr.zig");

pub const Builder = building.Builder;
pub const InstRef = bytecode.InstRef;
pub const Program = bytecode.Program;

// TODO compile each ssa function to its own Inst array and then appendSlice to
// the program, this will prevent ordering errors when it comes to compiling
// functions and allow me to compile functions here without any changes to
// the simple flat walk I'm doing here to lower SSA

// vm call convention:
// - callers must preserve any register values in use when calling another
//   function
// - function args are passed through (unreserved) registers 1-x, values are
//   returned through r0
//   - if return value fits in a register, it is returned by value
//   - otherwise, caller must provide memory for the callee to write to in r0
//
// other important notes
// - value pointers are stored as indices into the stack, and function pointers
//   are stored as instruction indices

pub const Error = Allocator.Error || canon.CrucifyError;

fn compileOp(
    env: *Env,
    b: *Builder,
    rmap: *RegisterMap,
    ref: FuncRef,
    op: Op,
) Error!void {
    const ally = env.ally;

    switch (op) {
        .ldc => |ldc| {
            // crucify and load a constant
            const dst = rmap.get(ldc.to);
            const texpr = ref.getConst(env.*, ldc.a);

            std.debug.assert(texpr.data != .func);

            if (texpr.data == .func_ref) {
                const call_ref = texpr.data.func_ref;
                const pos = Pos.ofEntry(call_ref);

                // func refs must be backreferenced
                try Bc.immBackref(b, ally, dst, pos);

                // ensure called func is compiled and resolved
                const bc_ref = try env.ensureCompiled(call_ref);
                try b.resolve(ally, pos, bc_ref);
            } else {
                // add value as an immediate
                const value = try canon.crucify(env.*, texpr);
                defer value.deinit(env.ally);

                try Bc.imm(b, ally, dst, value.buf);
            }
        },
        .cast => |pure| {
            const src = rmap.get(pure.params[0]);
            const dst = rmap.get(pure.to);

            // TODO cast requires more than bitcast in many cases
            try b.addInst(ally, Bc.mov(src, dst));
        },
        .alloca => |all| {
            // store stack pointer in a register and
            const dst = rmap.get(all.to);
            const size = rmap.getTemp();
            defer rmap.dropTemp(size);

            try b.addInst(ally, Bc.mov(Vm.SP, dst));
            try Bc.imm(b, ally, size, canon.from(&all.size));
            try b.addInst(ally, Bc.iadd(Vm.SP, size, Vm.SP));
        },
        .store => |imp| {
            const src = rmap.get(imp.params[0]);
            const dst = rmap.get(imp.params[1]);
            const size = env.sizeOf(ref.getLocal(env.*, imp.params[0]));
            const nbytes = @intCast(u8, size);

            try b.addInst(ally, Bc.store(nbytes, src, dst));
        },
        .ret => |imp| {
            const src = rmap.get(imp.params[0]);

            try b.addInst(ally, Bc.mov(src, Vm.RETURN));
            try b.addInst(ally, Bc.ret);
        },
        .call => |call| {
            // map params
            for (call.params) |param, i| {
                const reg = Register.param(@intCast(Register.Index, i));
                try rmap.mapExact(ally, b, param, reg);
            }

            // call func
            const func = rmap.get(call.func);
            const dst = rmap.get(call.to);

            try b.addInst(ally, Bc.call(func));
            try b.addInst(ally, Bc.mov(Vm.RETURN, dst));
        },
        .br => |br| {
            const cond = rmap.get(br.cond);

            try Bc.jump_if(b, ally, cond, Pos.of(ref, br.a, 0));
            try Bc.jump(b, ally, Pos.of(ref, br.b, 0));
        },
        .jmp => |jump| {
            const data = rmap.get(jump.data);

            try b.addInst(ally, Bc.mov(data, Vm.RETURN));
            try Bc.jump(b, ally, Pos.of(ref, jump.dst, 0));
        },
        .phi => |phi| {
            const dst = rmap.get(phi.to);

            try b.addInst(ally, Bc.mov(Vm.RETURN, dst));
        },
        inline .mod, .fn_ty => |pure, tag| {
            // binary ops
            const lhs = rmap.get(pure.params[0]);
            const rhs = rmap.get(pure.params[1]);
            const to = rmap.get(pure.to);

            const con = switch (comptime tag) {
                .mod => Bc.imod,
                .fn_ty => Bc.fn_ty,
                else => unreachable
            };

            try b.addInst(ally, con(lhs, rhs, to));
        },
        inline .add, .sub, .mul, .div => |pure, tag| {
            // binary ops with type awareness
            const lhs = rmap.get(pure.params[0]);
            const rhs = rmap.get(pure.params[1]);
            const to = rmap.get(pure.to);

            const ty = env.tw.get(ref.getLocal(env.*, pure.to));

            if (ty.* == .ptr
             or ty.* == .number and ty.number.layout != .float) {
                // integral math
                const con = switch (comptime tag) {
                    .add => Bc.iadd,
                    .sub => Bc.isub,
                    .mul => Bc.imul,
                    .div => Bc.idiv,
                    .mod => Bc.imod,
                    else => unreachable
                };

                try b.addInst(ally, con(lhs, rhs, to));
            } else if (ty.* == .number and ty.number.layout == .float) {
                // floats
                @panic("TODO compile float arithmetic");
            } else {
                unreachable;
            }
        },
        else => |tag| std.debug.panic("TODO compile op {}", .{tag})
    }
}

/// returns entry point of function
fn compileFunc(env: *Env, super: *Builder, ref: FuncRef) Error!void {
    const ally = env.ally;
    const func = env.getFunc(ref);

    var b = Builder{};
    defer b.deinit(env.ally);

    // mark start of region
    const start = b.here();

    // register allocation for ssa vars
    var rmap = try RegisterMap.init(ally, func);
    defer rmap.deinit(ally);

    for (func.blocks.items) |block, i| {
        // resolve block label
        const label = ssa.Label.of(i);
        try b.resolve(ally, Pos.of(ref, label, 0), b.here());

        // compile block ops
        for (block.ops.items) |op| {
            try compileOp(env, &b, &rmap, ref, op);
            rmap.next();
        }
    }

    // mark end of region
    try b.addRegion(ally, ref, start, b.here());
    try super.append(env.ally, b);
}

pub fn compile(env: *Env, ref: FuncRef) Error!void {
    std.debug.assert(env.sizeOf(env.getFunc(ref).returns) <= 8);
    try compileFunc(env, &env.bc, ref);
}
