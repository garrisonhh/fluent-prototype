const std = @import("std");
const Allocator = std.mem.Allocator;
const Env = @import("env.zig");
const ssa = @import("ssa/ssa.zig");
const FuncRef = ssa.FuncRef;
const Label = ssa.Label;
const Pos = ssa.Pos;
const Op = ssa.Op;
const Vm = @import("bytecode/vm.zig");
const Register = Vm.Register;
const bytecode = @import("bytecode/bytecode.zig");
const Inst = bytecode.Inst;
const Bc = bytecode.Construct;
const building = @import("bytecode/building.zig");
const canon = @import("canon.zig");
const RegisterMap = @import("bytecode/register_map.zig");

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

pub const Error =
    Allocator.Error ||
    canon.CrucifyError ||
    canon.Repr.Error;

fn compileAlloca(
    ally: Allocator,
    b: *Builder,
    rmap: *RegisterMap,
    size: usize,
    dst: Register,
) Error!void {
    const size_reg = rmap.getTemp();
    defer rmap.dropTemp(size_reg);

    try b.addInst(ally, Bc.mov(Vm.SP, dst));
    try Bc.imm(b, ally, size_reg, canon.from(&size));
    try b.addInst(ally, Bc.iadd(Vm.SP, size_reg, Vm.SP));
}

fn compileOp(
    env: *Env,
    b: *Builder,
    rmap: *RegisterMap,
    ref: FuncRef,
    op: Op,
) Error!void {
    const ally = env.ally;

    try b.addComment(ally, "{}", .{op});

    switch (op) {
        .ldc => |ldc| {
            // crucify and load a constant
            const dst = rmap.get(ldc.to);
            const texpr = ref.getConst(env.*, ldc.a);

            std.debug.assert(texpr.data != .func);

            if (texpr.data == .func_ref) {
                const call_ref = texpr.data.func_ref;
                const pos = Pos.ofEntry(call_ref);

                if (call_ref.eql(ref)) {
                    // recursive call
                    const bcref = b.getResolved(Pos.ofEntry(call_ref));
                    const bytes = canon.from(&@as(u64, bcref.index));
                    try Bc.imm(b, ally, dst, bytes);
                } else {
                    // func refs must be backreferenced
                    try Bc.immBackref(b, ally, dst, pos);

                    // ensure called func is compiled and resolved
                    const bc_ref = try env.ensureCompiled(call_ref);
                    try b.resolve(ally, pos, bc_ref);
                }
            } else {
                // add value as an immediate
                const value = try canon.crucify(env.*, texpr);
                defer value.deinit(env.ally);

                try Bc.imm(b, ally, dst, value.buf);
            }
        },
        .copy, .ptrcast => |copy| {
            const src = rmap.get(copy.params[0]);
            const dst = rmap.get(copy.to);

            try b.addInst(ally, Bc.mov(src, dst));
        },
        .alloca => |all| {
            // store stack pointer in a register and add to it
            const dst = rmap.get(all.to);
            try compileAlloca(ally, b, rmap, all.size, dst);
        },
        .store => |eff| {
            const dst = rmap.get(eff.params[0]);
            const src = rmap.get(eff.params[1]);
            const src_repr = ref.getLocal(env.*, eff.params[1]);

            const size = try env.rw.sizeOf(src_repr);
            const nbytes = @intCast(u8, size);

            try b.addInst(ally, Bc.store(nbytes, src, dst));
        },
        .gfp => |gfp| {
            const dst = rmap.get(gfp.to);
            const ptr = rmap.get(gfp.obj);

            const obj_ptr_repr = env.rw.get(ref.getLocal(env.*, gfp.obj));
            const obj_repr = env.rw.get(obj_ptr_repr.ptr);
            const offset = (try obj_repr.access(env.rw, gfp.index)).offset;

            try Bc.imm(b, ally, dst, canon.from(&offset));
            try b.addInst(ally, Bc.iadd(dst, ptr, dst));
        },
        .memcpy => |eff| {
            const dst = rmap.get(eff.params[0]);
            const src = rmap.get(eff.params[1]);

            const repr = env.rw.get(ref.getLocal(env.*, eff.params[0])).ptr;
            const nbytes = @intCast(u32, try env.rw.sizeOf(repr));

            try Bc.memcpy(b, ally, src, dst, nbytes);
        },
        .ret => |eff| {
            const src = rmap.get(eff.params[0]);

            if (src.n != Vm.RETURN.n) {
                try b.addInst(ally, Bc.mov(src, Vm.RETURN));
            }

            try b.addInst(ally, Bc.ret);
        },
        .vcall => |call| {
            // push all live ssa values that aren't dying or being born here
            var live = std.BoundedArray(Register, Register.COUNT){};
            var iter = rmap.iterator();
            while (iter.next()) |entry| {
                const lt = rmap.proph.get(entry.local);
                if (!lt.start.eql(rmap.pos) and !lt.stop.eql(rmap.pos)) {
                    live.appendAssumeCapacity(entry.reg);
                    try b.addInst(ally, Bc.push(8, entry.reg));
                }
            }

            // mov func to return register
            const func = rmap.get(call.func);

            try b.addInst(ally, Bc.mov(func, Vm.RETURN));

            // push/pop params to ensure no clobbering
            for (call.params) |param| {
                try b.addInst(ally, Bc.push(8, rmap.get(param)));
            }

            var i: usize = 0;
            while (i < call.params.len) : (i += 1) {
                const n = call.params.len - i - 1;
                const reg = Register.param(@intCast(Register.Index, n));
                try b.addInst(ally, Bc.pop(8, reg));
            }

            // do call op
            const dst = rmap.get(call.ret);

            try b.addInst(ally, Bc.call(Vm.RETURN));
            try b.addInst(ally, Bc.mov(Vm.RETURN, dst));

            // pop all live ssa values back into their mapped registers
            const slice = live.slice();
            std.mem.reverse(Register, slice);
            for (slice) |reg| {
                try b.addInst(ally, Bc.pop(8, reg));
            }
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
        inline .slice_ty => |pure, tag| {
            // unary ops
            const operand = rmap.get(pure.params[0]);
            const to = rmap.get(pure.to);

            const con = switch (comptime tag) {
                .slice_ty => Bc.slice_ty,
                else => unreachable,
            };

            try b.addInst(ally, con(operand, to));
        },
        .eq => |pure| {
            const lhs = rmap.get(pure.params[0]);
            const rhs = rmap.get(pure.params[1]);
            const to = rmap.get(pure.to);

            try b.addInst(ally, Bc.xor(lhs, rhs, to));
            try b.addInst(ally, Bc.lnot(to, to));
        },
        inline .mod, .fn_ty, .@"and", .@"or" => |pure, tag| {
            // binary ops
            const lhs = rmap.get(pure.params[0]);
            const rhs = rmap.get(pure.params[1]);
            const to = rmap.get(pure.to);

            const con = switch (comptime tag) {
                .mod => Bc.imod,
                .fn_ty => Bc.fn_ty,
                .@"and" => Bc.land,
                .@"or" => Bc.lor,
                else => unreachable,
            };

            try b.addInst(ally, con(lhs, rhs, to));
        },
        inline .add, .sub, .mul, .div, .shl, .shr => |pure, tag| {
            // binary ops with type awareness
            const lhs = rmap.get(pure.params[0]);
            const rhs = rmap.get(pure.params[1]);
            const to = rmap.get(pure.to);

            const repr = env.rw.get(ref.getLocal(env.*, pure.to));

            switch (repr.*) {
                .int, .uint => {
                    const con = switch (comptime tag) {
                        .add => Bc.iadd,
                        .sub => Bc.isub,
                        .mul => Bc.imul,
                        .div => Bc.idiv,
                        .mod => Bc.imod,
                        .shl => Bc.shl,
                        .shr => Bc.shr,
                        else => unreachable,
                    };

                    try b.addInst(ally, con(lhs, rhs, to));
                },
                .ptr => {
                    const con = switch (comptime tag) {
                        .add => Bc.iadd,
                        .sub => Bc.isub,
                        else => unreachable,
                    };

                    try b.addInst(ally, con(lhs, rhs, to));
                },
                .float => @panic("TODO non-integral arithmetic"),
                .unit, .array, .coll, .func => unreachable,
            }
        },
        else => |tag| std.debug.panic("TODO compile op {}", .{tag}),
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

    try b.addComment(ally, "[function {}]", .{func.name});

    // register allocation for ssa vars
    var rmap = try RegisterMap.init(ally, func);
    defer rmap.deinit(ally);

    for (func.blocks.items) |block, i| {
        // resolve block label
        const label = ssa.Label.of(i);
        try b.resolve(ally, Pos.of(ref, label, 0), b.here());

        // comment block
        try b.addComment(ally, "{} @{}", .{ func.name, label.index });

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
    try compileFunc(env, &env.bc, ref);
}
