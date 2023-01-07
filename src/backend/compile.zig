const std = @import("std");
const Allocator = std.mem.Allocator;
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

pub const Error = Allocator.Error;

fn compileOp(
    env: *Env,
    b: *Builder,
    rmap: *RegisterMap,
    ref: FuncRef,
    op: Op,
) Error!void {
    _ = env;
    _ = b;
    _ = rmap;
    _ = ref;
    _ = op;

    @panic("TODO reimplement compileOp");
}

/// returns entry point of function
fn compileFunc(env: *Env, b: *Builder, ref: FuncRef) Error!void {
    const ally = env.ally;
    const func = env.getFunc(ref);

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
            try compileOp(env, b, &rmap, ref, op);
        }
    }

    // mark end of region
    try b.addRegion(ally, ref, start, b.here());
}

/// compiles func to bytecode and returns instruction address of the function's
/// entry point.
pub fn compile(env: *Env, ref: FuncRef) Error!InstRef {
    std.debug.assert(env.sizeOf(env.getFunc(ref).returns) <= 8);

    var b = Builder{};
    defer b.deinit(env.ally);

    try compileFunc(env, &b, ref);

    // TODO try env.bc.append(env.ally, b);

    @panic("TODO compile; append builder and get final entry point");
}
