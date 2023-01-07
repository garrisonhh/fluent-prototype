const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const Env = @import("env.zig");
const ssa = @import("ssa.zig");
const FuncRef = ssa.FuncRef;
const Label = ssa.Label;
const Local = ssa.Local;
const Vm = @import("bytecode/vm.zig");
const Register = Vm.Register;
const bytecode = @import("bytecode/bytecode.zig");
const Inst = bytecode.Inst;
const building = @import("bytecode/builder.zig");
const SsaPos = building.SsaPos;
const canon = @import("canon.zig");
const Value = @import("value.zig");

pub const Builder = building.Builder;
pub const InstRef = bytecode.InstRef;
pub const Program = bytecode.Program;

// TODO the way I'm constructing ops is very loosely typed and it's easy to
// create bugs here

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
//   - TODO how to static pointers? maybe a `static` instruction which loads
//     static memory

pub const Error = Allocator.Error;

const RegisterMap = struct {
    const Self = @This();

    // TODO will at some point need to handle the case where there are more than
    // 256 locals
    unused: u8,
    temporaries: u8,
    map: []?Register,

    fn init(
        ally: Allocator,
        num_locals: usize,
        takes: usize
    ) Allocator.Error!Self {
        var self = Self{
            // r0 is used for large return parameters (see callconv)
            .unused = Vm.RESERVED + 1,
            .temporaries = 0,
            .map = try ally.alloc(?Register, num_locals),
        };

        std.mem.set(?Register, self.map, null);

        // map params
        var i: usize = 0;
        while (i < takes) : (i += 1) {
            self.map[i] = Register.of(@intCast(u8, i + Vm.RESERVED + 1));
        }

        return self;
    }

    fn deinit(self: Self, ally: Allocator) void {
        ally.free(self.map);
    }

    /// retrieves a new or old register for this local
    fn find(self: *Self, local: Local) Register {
        if (self.map[local.index]) |reg| return reg;

        // invalidate temporaries
        self.temporaries = 0;

        // alloc register
        const next = Register.of(self.unused);
        self.map[local.index] = next;
        self.unused += 1;

        return next;
    }

    /// retrieves an unused register for temporary usage. this register may be
    /// invalidated at the next `find` call.
    fn temporary(self: *Self) Register {
        const reg = Register.of(self.unused + self.temporaries);
        self.temporaries += 1;
        return reg;
    }
};

/// does whatever it needs to to get a constant into a register. this should
/// either load an immediate value or a ptr into static
fn compileLoadConst(env: *Env, reg: Register, value: Value) Error!void {
    const ally = env.ally;
    const bc = &env.bc;

    if (value.ptr.len <= 8) {
        // convert value to uint
        const n = canon.toCanonical(value.ptr);
        const bytes = @ptrCast(*const [8]u8, &n);

        // minimize stored bytes
        var to_store: usize = value.ptr.len;
        while (to_store > 0 and bytes[to_store - 1] == 0) {
            to_store -= 1;
        }

        comptime if (builtin.cpu.arch.endian() != .Little) {
            @compileError("storage code is deoptimized for bytecode space in "
                       ++ "a non-little-endian architecture");
        };

        // store with smallest imm op possible
        if (to_store <= 2) {
            try bc.addInst(ally, Inst.of(.imm2, reg.n, bytes[0], bytes[1]));
        } else if (to_store <= 4) {
            try bc.addInst(ally, Inst.of(.imm4, reg.n, 0, 0));
            try bc.addInst(ally, Inst.fromInt(@ptrCast(*const u32, bytes).*));
        } else {
            const data = @ptrCast(*const [2]u32, bytes);
            try bc.addInst(ally, Inst.of(.imm8, reg.n, 0, 0));
            try bc.addInst(ally, Inst.fromInt(data[0]));
            try bc.addInst(ally, Inst.fromInt(data[1]));
        }
    } else {
        @panic("TODO compile ldc -> static");
    }
}

/// load a u64 const
fn compileU64(env: *Env, n: u64, dst: Register) Error!void {
    var buf: [8]u8 align(16) = undefined;
    const data = canon.fromCanonical(&n);
    std.mem.copy(u8, &buf, data);

    const value = Value{ .ptr = buf[0..data.len] };
    try compileLoadConst(env, dst, value);
}

fn compileAlloca(env: *Env, size: Register, dst: Register) Error!void {
    try env.bc.addInst(env.ally, Inst.of(.mov, Vm.SP.n, dst.n, 0));
    try env.bc.addInst(env.ally, Inst.of(.iadd, Vm.SP.n, size.n, Vm.SP.n));
}

/// slices are a `struct { ptr: *T, len: usize }` which is stored in memory and
/// manipulated through a pointer to the struct
fn compileSliceOf(
    env: *Env,
    regmap: *RegisterMap,
    ptr: Register,
    len: Register,
    dst: Register
) Error!void {
    const ally = env.ally;
    const bc = &env.bc;

    // need 16 (struct size) and 8 (offset of length field) to be compiled
    const struct_size = regmap.temporary();
    const len_offset = regmap.temporary();
    const len_ptr = regmap.temporary();
    try compileU64(env, 16, struct_size);
    try compileU64(env, 8, len_offset);

    // alloc struct
    try compileAlloca(env, struct_size, dst);

    // store len + ptr fields
    try bc.addInst(ally, Inst.of(.store, 8, ptr.n, dst.n));
    try bc.addInst(ally, Inst.of(.iadd, dst.n, len_offset.n, len_ptr.n));
    try bc.addInst(ally, Inst.of(.store, 8, len.n, len_ptr.n));
}

fn todoCompileOp(op: ssa.Op) noreturn {
    std.debug.panic("TODO compile op {s}", .{@tagName(op)});
}

fn compileOp(
    env: *Env,
    ref: FuncRef,
    regmap: *RegisterMap,
    op: ssa.Op,
) Error!void {
    _ = env;
    _ = ref;
    _ = regmap;
    _ = op;

    @panic("TODO reimplement compileOp");
}

/// returns entry point of function
fn compileFunc(env: *Env, ref: FuncRef) Error!InstRef {
    const ally = env.ally;
    const bc = &env.bc;
    const func = env.getFunc(ref);

    // mark start of region
    const start = env.bc.here();

    // register allocation for ssa vars
    var rmap = try RegisterMap.init(ally, func.locals.items.len, func.takes);
    defer rmap.deinit(ally);

    for (func.blocks.items) |block, i| {
        // resolve block label
        const label = ssa.Label.of(i);
        try bc.resolve(ally, SsaPos.of(ref, label), bc.here());

        // compile block ops
        for (block.ops.items) |op| {
            try compileOp(env, ref, &rmap, op);
        }
    }

    // mark end of region
    const stop = env.bc.here();
    try env.bc.addRegion(ally, ref, start, stop);

    return start;
}

/// compiles func to bytecode and returns instruction address of the function's
/// entry point.
pub fn compile(env: *Env, ref: FuncRef) Error!InstRef {
    std.debug.assert(env.sizeOf(env.getFunc(ref).returns) <= 8);

    // TODO add ssa postprocessing here

    return try compileFunc(env, ref);
}
