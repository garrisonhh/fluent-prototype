const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const Env = @import("env.zig");
const ssa = @import("ssa.zig");
const types = @import("types.zig");
const TypeId = types.TypeId;
const TypeWelt = types.TypeWelt;
const Value = @import("value.zig");
const bytecode = @import("bytecode/bytecode.zig");
const Opcode = bytecode.Opcode;
const Inst = bytecode.Inst;
const Program = bytecode.Program;
const Vm = @import("bytecode/vm.zig");
const Register = Vm.Register;
const canon = @import("canon.zig");

// TODO the way I'm constructing ops is very loosely typed and it's easy to
// create bugs here

// TODO compile each ssa function to its own Inst array and then appendSlice to
// the program, this will prevent ordering errors when it comes to compiling
// functions and allow me to compile functions here without any changes to
// the simple flat walk I'm doing here to lower SSA

// TODO give builder its own file

pub const InstRef = packed struct {
    const Self = @This();

    index: u32,

    fn of(index: u32) Self {
        return Self{ .index = index };
    }
};

/// for backreferencing
const SsaPos = struct {
    const Self = @This();

    func: ssa.FuncRef,
    label: ssa.Label,

    fn of(func: ssa.FuncRef, label: ssa.Label) Self {
        return Self{
            .func = func,
            .label = label,
        };
    }

    /// gets ssa pos for a function's entry point
    fn ofFunc(func_ref: ssa.FuncRef) Self {
        return Self.of(func_ref, ssa.Label.of(0));
    }
};

pub const Builder = struct {
    const Self = @This();

    const BackRef = union(enum) {
        // what an SsaPos resolved to
        resolved: InstRef,
        // instructions to resolve once this SsaPos is resolved
        unresolved: std.ArrayListUnmanaged(InstRef),
    };

    const Region = struct {
        start: InstRef,
        stop: InstRef,
    };

    program: std.ArrayListAlignedUnmanaged(Inst, 16) = .{},
    // tracks backreferences while compiling
    refs: std.AutoHashMapUnmanaged(SsaPos, BackRef) = .{},
    // tracks slices of the program
    regions: std.AutoHashMapUnmanaged(ssa.FuncRef, Region) = .{},

    pub fn deinit(self: *Self, ally: Allocator) void {
        self.program.deinit(ally);
        self.refs.deinit(ally);
        self.regions.deinit(ally);
    }

    pub fn build(self: *Self, func: ssa.Func) Program {
        if (builtin.mode == .Debug) {
            // validate backrefs are completed
            var entries = self.refs.iterator();
            while (entries.next()) |entry| {
                if (entry.value_ptr.* == .unresolved) {
                    const sp = entry.key_ptr;
                    const label = sp.label.index;

                    std.debug.panic(
                        "bytecode compilation failed to resolve {}@{}",
                        .{func.name, label}
                    );
                }
            }
        }

        // get function backref
        const backref = self.refs.get(SsaPos.ofFunc(func.ref)).?;

        return Program{
            .entry = backref.resolved.index,
            .program = self.program.items,
        };
    }

    /// for jumps + backrefs
    fn here(self: Self) InstRef {
        return InstRef.of(@intCast(u32, self.program.items.len));
    }

    /// branching instructions expect a position to branch to. this effectively
    /// handles adding the position with any required backreferencing, and hides
    /// the internal backreference impl
    fn addBranch(
        self: *Self,
        ally: Allocator,
        sp: SsaPos
    ) Allocator.Error!void {
        const res = try self.refs.getOrPut(ally, sp);
        if (res.found_existing) {
            // retrieve resolved ref or add unresolved ref
            switch (res.value_ptr.*) {
                .resolved => |ref| {
                    try self.addInst(ally, Inst.fromInt(ref.index));
                },
                .unresolved => |*list| {
                    try list.append(ally, self.here());
                    try self.addInst(ally, Inst.fromInt(0));
                },
            }
        } else {
            // add unresolved backref
            res.value_ptr.* = BackRef{ .unresolved = .{} };
            try res.value_ptr.unresolved.append(ally, self.here());
            try self.addInst(ally, Inst.fromInt(0));
        }
    }

    /// resolve a backreference to a real instref
    fn resolve(
        self: *Self,
        ally: Allocator,
        sp: SsaPos,
        to: InstRef
    ) Allocator.Error!void {
        const res = try self.refs.getOrPut(ally, sp);
        if (res.found_existing) {
            // can't resolve the same SsaPos twice
            std.debug.assert(res.value_ptr.* == .unresolved);

            // resolve backrefs
            const list = &res.value_ptr.unresolved;
            defer list.deinit(ally);

            for (list.items) |ref| {
                self.program.items[ref.index] = Inst.fromInt(to.index);
            }
        }

        // keep resolved value
        res.value_ptr.* = BackRef{ .resolved = to };
    }

    fn addInst(self: *Self, ally: Allocator, inst: Inst) Allocator.Error!void {
        try self.program.append(ally, inst);
    }

    fn addRegion(
        self: *Self,
        ally: Allocator,
        func: ssa.FuncRef,
        start: InstRef,
        stop: InstRef
    ) Allocator.Error!void {
        std.debug.assert(start.index < stop.index);
        try self.regions.put(ally, func, Region{
            .start = start,
            .stop = stop
        });
    }

    /// removes instructions associated with a function. this is very useful
    /// for `eval` to not clutter the env's builder.
    pub fn removeFunc(
        self: *Self,
        ally: Allocator,
        func: ssa.FuncRef
    ) Allocator.Error!void {
        const region = self.regions.get(func).?;
        const start = region.start.index;
        const len = region.stop.index - start;

        try self.program.replaceRange(ally, start, len, &.{});
    }
};

// compilation =================================================================

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
    fn find(self: *Self, local: ssa.Local) Register {
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
    func: ssa.Func,
    regmap: *RegisterMap,
    op: ssa.Op,
) Error!void {
    const ally = env.ally;
    const bc = &env.bc;

    switch (op.classify()) {
        .ldc => |ldc| {
            const reg = regmap.find(ldc.to);
            const value = func.getConst(ldc.a);

            // acquire referenced function bytecode
            const ty = env.tw.get(func.getLocal(ldc.to));
            if (ty.* == .func) {
                // function value
                const ssa_ref = ssa.FuncRef.of(canon.toCanonical(value.ptr));
                const bc_ref = env.compiled.get(ssa_ref).?;
                const index: u64 = bc_ref.index;

                // back to value
                var buf: [8]u8 align(16) = undefined;
                const data = canon.fromCanonical(&index);
                std.mem.copy(u8, &buf, data);

                const processed = Value{ .ptr = buf[0..data.len] };
                try compileLoadConst(env, reg, processed);
            } else {
                // raw value
                try compileLoadConst(env, reg, value);
            }
        },
        .arg => |arg| {
            const src = regmap.find(arg.from);
            const dst = Register.of(@intCast(u8, Vm.RESERVED + 1 + arg.arg));
            try bc.addInst(ally, Inst.of(.mov, src.n, dst.n, 0));
        },
        .branch => |br| {
            const cond = regmap.find(br.cond);
            try bc.addInst(ally, Inst.of(.jump_if, cond.n, 0, 0));
            try bc.addBranch(ally, SsaPos.of(func.ref, br.a));
            try bc.addInst(ally, Inst.of(.jump, 0, 0, 0));
            try bc.addBranch(ally, SsaPos.of(func.ref, br.b));
        },
        .jump => |jmp| {
            try bc.addInst(ally, Inst.of(.jump, 0, 0, 0));
            try bc.addBranch(ally, SsaPos.of(func.ref, jmp.dst));
        },
        .alloca => |all| {
            // 1. store sp in output
            // 2. load size constant
            // 3. add size to sp
            const dst = regmap.find(all.to);
            const size = regmap.temporary();

            try compileU64(env, all.size, size);
            try compileAlloca(env, size, dst);
        },
        .unary => |un| switch (op) {
            .call => {
                const fn_reg = regmap.find(un.a);
                try bc.addInst(ally, Inst.of(.call, fn_reg.n, 0, 0));
                // move return value from RETURN to expected register
                const ret = regmap.find(un.to);
                try bc.addInst(ally, Inst.of(.mov, Vm.RETURN.n, ret.n, 0));
            },
            .load => {
                const dst_ty = func.getLocal(un.a);
                const dst_size = @intCast(u8, env.sizeOf(dst_ty));
                const arg = regmap.find(un.a);
                const to = regmap.find(un.to);
                try bc.addInst(ally, Inst.of(.load, dst_size, arg.n, to.n));
            },
            .not => {
                const ty = env.tw.get(func.getLocal(un.to));
                const opcode: Opcode = switch (ty.*) {
                    .@"bool" => .lnot,
                    .number => |num| switch (num.layout) {
                         .int, .uint => .bnot,
                         .float => unreachable
                    },
                    else => unreachable
                };

                const arg = regmap.find(un.a);
                const to = regmap.find(un.to);
                try bc.addInst(ally, Inst.of(opcode, arg.n, to.n, 0));
            },
            // TODO use different ops for bitcast and other casts
            .cast => {
                const dst = env.tw.get(func.getLocal(un.to));
                const src = env.tw.get(func.getLocal(un.a));

                const arg = regmap.find(un.a);
                const to = regmap.find(un.to);

                switch (dst.*) {
                    // bitcastable
                    .@"bool" => {
                        try bc.addInst(ally, Inst.of(.mov, arg.n, to.n, 0));
                    },
                    .ptr => |ptr| if (ptr.kind == .slice) {
                        if (src.* == .ptr and src.ptr.kind == .single) {
                            // cast `*[_]T` to `[]T`
                            const arr_ty = env.tw.get(src.ptr.to);
                            std.debug.assert(arr_ty.* == .array);

                            const len_reg = regmap.temporary();
                            try compileU64(env, arr_ty.array.size, len_reg);
                            try compileSliceOf(env, regmap, arg, len_reg, to);
                        } else {
                            @panic("TODO");
                        }
                    } else {
                        // single + many pointers can be bitcast
                        try bc.addInst(ally, Inst.of(.mov, arg.n, to.n, 0));
                    },
                    // numbers require more complex casting rules
                    .number => |num| switch (num.layout) {
                        .uint => {
                            try bc.addInst(ally, Inst.of(.mov, arg.n, to.n, 0));
                        },
                        else => @panic("TODO")
                    },
                    else => unreachable
                }
            },
            .slice_ty => {
                const arg = regmap.find(un.a);
                const to = regmap.find(un.to);
                try bc.addInst(ally, Inst.of(.slice_ty, arg.n, to.n, 0));
            },
            else => todoCompileOp(op)
        },
        .binary => |bin| {
            const ty = env.tw.get(func.getLocal(bin.to));
            const opcode: Opcode = switch (ty.*) {
                .@"bool" => switch (op) {
                    .@"or" => .lor,
                    .@"and" => .land,
                    else => todoCompileOp(op)
                },
                .number => |num| switch (num.layout) {
                    .int, .uint => switch (op) {
                        .add => .iadd,
                        .sub => .isub,
                        .mul => .imul,
                        .div => .idiv,
                        .mod => .imod,
                        .@"or" => .bor,
                        .@"and" => .band,
                        // .xor => .xor,
                        else => todoCompileOp(op)
                    },
                    .float => switch (op) {
                        else => todoCompileOp(op)
                    },
                },
                .ptr => switch (op) {
                    .add => .iadd,
                    .sub => .isub,
                    else => unreachable
                },
                .ty => switch (op) {
                    .fn_ty => .fn_ty,
                    else => unreachable
                },
                else => todoCompileOp(op)
            };

            const lhs = regmap.find(bin.a);
            const rhs = regmap.find(bin.b);
            const to = regmap.find(bin.to);
            try bc.addInst(ally, Inst.of(opcode, lhs.n, rhs.n, to.n));
        },
        .un_eff => |ue| switch (op) {
            .ret => {
                const value = regmap.find(ue.a);
                try bc.addInst(ally, Inst.of(.mov, value.n, Vm.RETURN.n, 0));
                try bc.addInst(ally, Inst.of(.ret, 0, 0, 0));
            },
            else => @panic("TODO")
        },
        .bin_eff => |be| switch (op) {
            .store => {
                const src_ty = func.getLocal(be.a);
                const bytes = switch (env.sizeOf(src_ty)) {
                    1, 2, 4, 8 => |n| @intCast(u8, n),
                    else => @panic("TODO store other sizes")
                };

                const src = regmap.find(be.a);
                const dst = regmap.find(be.b);
                try bc.addInst(ally, Inst.of(.store, bytes, src.n, dst.n));
            },
            else => @panic("TODO")
        },
        else => std.debug.panic("TODO compile ssa `{s}`", .{@tagName(op)})
    }
}

/// returns entry point of function
fn compileFunc(env: *Env, func: ssa.Func) Error!InstRef {
    const ally = env.ally;
    const bc = &env.bc;

    // mark start of region
    const start = env.bc.here();

    // register allocation for ssa vars
    var rmap = try RegisterMap.init(ally, func.locals.items.len, func.takes);
    defer rmap.deinit(ally);

    for (func.blocks.items) |block, i| {
        // resolve block label
        const label = ssa.Label.of(i);
        try bc.resolve(ally, SsaPos.of(func.ref, label), bc.here());

        // compile block ops
        for (block.ops.items) |op| {
            try compileOp(env, func, &rmap, op);
        }

        std.debug.print("compiled {d} ops for block {d}\n", .{block.ops.items.len, i});
    }

    std.debug.print("compiled {d} blocks\n", .{func.blocks.items.len});

    // mark end of region
    const stop = env.bc.here();

    std.debug.print(
        "compiling func {}; region {d} to {d}\n",
        .{func.name, start.index, stop.index}
    );

    try env.bc.addRegion(ally, func.ref, start, stop);

    return start;
}

/// compiles func to bytecode and returns instruction address of the function's
/// entry point.
pub fn compile(env: *Env, func: ssa.Func) Error!InstRef {
    std.debug.assert(env.sizeOf(func.returns) <= 8);

    // TODO add ssa postprocessing here

    return try compileFunc(env, func);
}
