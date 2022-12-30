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

// TODO give builder its own file

const InstRef = packed struct {
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

    static: std.ArrayListUnmanaged(u8) = .{},
    program: std.ArrayListAlignedUnmanaged(Inst, 16) = .{},
    // tracks backreferences while compiling
    refs: std.AutoHashMapUnmanaged(SsaPos, BackRef) = .{},
    // tracks slices of the program
    regions: std.AutoHashMapUnmanaged(ssa.FuncRef, Region) = .{},

    pub fn deinit(self: *Self, ally: Allocator) void {
        self.static.deinit(ally);
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
            .static = self.static.items,
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

    fn addStatic(
        self: *Self,
        ally: Allocator,
        data: []const u8
    ) Allocator.Error!usize {
        const index = self.static.items.len;
        try self.static.appendSlice(ally, data);
        return index;
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
    map: []?Register,

    fn init(
        ally: Allocator,
        num_locals: usize,
        takes: usize
    ) Allocator.Error!Self {
        var self = Self{
            // r0 is used for large return parameters (see callconv)
            .unused = Vm.RESERVED + 1,
            .map = try ally.alloc(?Register, num_locals),
        };

        std.mem.set(?Register, self.map, null);

        // map params
        var i: usize = 0;
        while (i < takes) : (i += 1) {
            self.map[i] = Register.of(@intCast(u8, i));
        }

        return self;
    }

    fn deinit(self: Self, ally: Allocator) void {
        ally.free(self.map);
    }

    /// retrieves a new or old register for this local
    fn find(self: *Self, local: ssa.Local) Register {
        if (self.map[local.index]) |reg| return reg;

        // alloc register
        const next = Register.of(self.unused);
        self.map[local.index] = next;
        self.unused += 1;

        return next;
    }

    /// retrieves an unused register for temporary usage. this register may be
    /// invalidated at the next `find` call.
    fn temporary(self: Self, index: u8) Register {
        return Register.of(self.unused + index);
    }
};

/// does whatever it needs to to get a constant into a register. this should
/// either load an immediate value or a ptr into static
fn compileLoadConst(env: *Env, reg: Register, value: Value) Error!void {
    const ally = env.ally;
    const bc = &env.bc;

    if (value.ptr.len <= 8) {
        // convert value to uint
        // TODO I should probably do this with a union, this is already a source
        // of bugs
        const n = canon.toCanonical(value.ptr);
        const bytes = @ptrCast(*const [8]u8, &n);

        // minimize stored bytes
        var to_store: usize = 8;
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

fn todoCompileOp(op: ssa.Op) noreturn {
    std.debug.panic("TODO compile op {s}", .{@tagName(op)});
}

fn compileOp(
    env: *Env,
    func: ssa.Func,
    registers: *RegisterMap,
    op: ssa.Op,
) Error!void {
    const ally = env.ally;
    const bc = &env.bc;

    switch (op.classify()) {
        .ldc => |ldc| {
            const reg = registers.find(ldc.to);
            try compileLoadConst(env, reg, func.getConst(ldc.a));
        },
        .branch => |br| {
            const cond = registers.find(br.cond);
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
            const reg = registers.find(all.to);

            const size = @intCast(u64, all.size);
            var buf: [8]u8 align(16) = undefined;
            std.mem.copy(u8, &buf, canon.fromCanonical(&size));
            const size_val = Value{ .ptr = &buf };
            const size_reg = registers.temporary(0);

            try bc.addInst(ally, Inst.of(.mov, Vm.SP.n, reg.n, 0));
            try compileLoadConst(env, size_reg, size_val);
            try bc.addInst(ally, Inst.of(.iadd, Vm.SP.n, size_reg.n, Vm.SP.n));
        },
        .unary => |un| switch (op) {
            .load => {
                const dst_ty = func.getLocal(un.a);
                const dst_size = @intCast(u8, env.sizeOf(dst_ty));
                const arg = registers.find(un.a);
                const to = registers.find(un.to);
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

                const arg = registers.find(un.a);
                const to = registers.find(un.to);
                try bc.addInst(ally, Inst.of(opcode, arg.n, to.n, 0));
            },
            .cast => {
                const dst = env.tw.get(func.getLocal(un.to));

                const arg = registers.find(un.a);
                const to = registers.find(un.to);

                switch (dst.*) {
                    // bool and ptr can bitcast
                    .@"bool", .ptr => {
                        try bc.addInst(ally, Inst.of(.mov, arg.n, to.n, 0));
                    },
                    // numbers require more complex casting rules
                    // TODO use different ops for bitcast and other casts
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
                const arg = registers.find(un.a);
                const to = registers.find(un.to);
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
                else => todoCompileOp(op)
            };

            const lhs = registers.find(bin.a);
            const rhs = registers.find(bin.b);
            const to = registers.find(bin.to);
            try bc.addInst(ally, Inst.of(opcode, lhs.n, rhs.n, to.n));
        },
        .un_eff => |ue| switch (op) {
            .ret => {
                const value = registers.find(ue.a);
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

                const src = registers.find(be.a);
                const dst = registers.find(be.b);
                try bc.addInst(ally, Inst.of(.store, bytes, src.n, dst.n));
            },
            else => @panic("TODO")
        },
        else => std.debug.panic("TODO compile ssa `{s}`", .{@tagName(op)})
    }
}

fn compileFunc(env: *Env, func: ssa.Func) Error!void {
    const ally = env.ally;
    const bc = &env.bc;

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
    }

    // add region
    const stop = env.bc.here();
    try env.bc.addRegion(ally, func.ref, start, stop);
}

pub fn compile(env: *Env, func: ssa.Func) Error!void {
    if (env.sizeOf(func.returns) > 8) {
        @panic("TODO allocate for program return value");
    }

    try compileFunc(env, func);
}
