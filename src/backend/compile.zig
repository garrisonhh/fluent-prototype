const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
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
const canon = @import("bytecode/canon.zig");

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
    fn ofFunc(ssa_prog: ssa.Program, func_ref: ssa.FuncRef) Self {
        const func = ssa_prog.funcs[func_ref.index];
        return of(func_ref, func.entry);
    }
};

const BackRef = union(enum) {
    // what an SsaPos resolved to
    resolved: InstRef,
    // instructions to resolve once this SsaPos is resolved
    unresolved: std.ArrayListUnmanaged(InstRef),
};

const Builder = struct {
    const Self = @This();

    ally: Allocator,
    typewelt: *const TypeWelt,
    static: std.ArrayListUnmanaged(u8) = .{},
    program: std.ArrayListAlignedUnmanaged(Inst, 16) = .{},
    refs: std.AutoHashMapUnmanaged(SsaPos, BackRef) = .{},

    fn init(ally: Allocator, typewelt: *const TypeWelt) Self {
        return Self{
            .ally = ally,
            .typewelt = typewelt,
        };
    }

    /// invalidates this builder
    fn build(self: *Self, ssa_prog: ssa.Program) Program {
        if (builtin.mode == .Debug) {
            // validate backrefs are done
            var entries = self.refs.iterator();
            while (entries.next()) |entry| {
                if (entry.value_ptr.* == .unresolved) {
                    const sp = entry.key_ptr;
                    const func = ssa_prog.funcs[sp.func.index].label;
                    const label = sp.label.index;

                    std.debug.panic(
                        "bytecode compilation failed to resolve {}:{}",
                        .{func, label}
                    );
                }
            }
        }
        self.refs.deinit(self.ally);

        return Program{
            .returns = ssa_prog.funcs[ssa_prog.entry.index].returns,
            .static = self.static.toOwnedSlice(self.ally),
            .program = self.program.toOwnedSlice(self.ally),
        };
    }


    fn sizeOf(self: Self, ty: TypeId) usize {
        return self.typewelt.get(ty).sizeOf(self.typewelt.*);
    }

    /// for jumps + backrefs
    fn here(self: Self) InstRef {
        return InstRef.of(@intCast(u32, self.program.items.len));
    }

    /// branching instructions expect a position to branch to. this effectively
    /// handles adding the position with any required backreferencing, and hides
    /// the internal backreference impl
    fn addBranch(self: *Self, sp: SsaPos) Allocator.Error!void {
        const res = try self.refs.getOrPut(self.ally, sp);
        if (res.found_existing) {
            // retrieve resolved ref or add unresolved ref
            switch (res.value_ptr.*) {
                .resolved => |ref| try self.addInst(Inst.fromInt(ref.index)),
                .unresolved => |*list| {
                    try list.append(self.ally, self.here());
                    try self.addInst(Inst.fromInt(0));
                },
            }
        } else {
            // add unresolved backref
            res.value_ptr.* = BackRef{ .unresolved = .{} };
            try res.value_ptr.unresolved.append(self.ally, self.here());
            try self.addInst(Inst.fromInt(0));
        }
    }

    /// resolve a backreference to a real instref
    fn resolve(self: *Self, sp: SsaPos, to: InstRef) Allocator.Error!void {
        const res = try self.refs.getOrPut(self.ally, sp);
        if (res.found_existing) {
            // can't resolve the same SsaPos twice
            std.debug.assert(res.value_ptr.* == .unresolved);

            // resolve backrefs
            const list = &res.value_ptr.unresolved;
            defer list.deinit(self.ally);

            for (list.items) |ref| {
                self.program.items[ref.index] = Inst.fromInt(to.index);
            }
        }

        // keep resolved value
        res.value_ptr.* = BackRef{ .resolved = to };
    }

    fn addInst(self: *Self, inst: Inst) Allocator.Error!void {
        try self.program.append(self.ally, inst);
    }

    fn addStatic(self: *Self, data: []const u8) Allocator.Error!usize {
        const index = self.static.items.len;
        try self.static.appendSlice(self.ally, data);
        return index;
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

pub const CompileError =
    Allocator.Error;

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
fn compileLoadConst(
    b: *Builder,
    reg: Register,
    value: Value
) CompileError!void {
    if (value.ptr.len <= 8) {
        // convert value to uint
        // TODO I should probably do this with a union, this is already a source
        // of bugs
        const n = canon.toCanonical(value.ptr);
        const zeroes = @clz(n);

        // store with smallest imm op possible
        if (zeroes >= 0x30) {
            const hi = @intCast(u8, n >> 8);
            const lo = @truncate(u8, n);
            try b.addInst(Inst.of(.imm2, reg.n, hi, lo));
        } else if (zeroes >= 0x20) {
            try b.addInst(Inst.of(.imm4, reg.n, 0, 0));
            try b.addInst(Inst.fromInt(@intCast(u32, n)));
        } else {
            try b.addInst(Inst.of(.imm8, reg.n, 0, 0));
            try b.addInst(Inst.fromInt(@intCast(u32, n >> 32)));
            try b.addInst(Inst.fromInt(@truncate(u32, n)));
        }
    } else {
        @panic("TODO compile ldc -> static");
    }
}

fn todoCompileOp(op: ssa.Op) noreturn {
    std.debug.panic("TODO compile op {s}", .{@tagName(op)});
}

fn compileOp(
    b: *Builder,
    func_ref: ssa.FuncRef,
    func: ssa.Func,
    registers: *RegisterMap,
    op: ssa.Op,
) CompileError!void {
    switch (op.classify()) {
        .ldc => |ldc| {
            const reg = registers.find(ldc.to);
            try compileLoadConst(b, reg, func.consts[ldc.a.index]);
        },
        .branch => |br| {
            const cond = registers.find(br.cond);
            try b.addInst(Inst.of(.jump_if, cond.n, 0, 0));
            try b.addBranch(SsaPos.of(func_ref, br.a));
            try b.addInst(Inst.of(.jump, 0, 0, 0));
            try b.addBranch(SsaPos.of(func_ref, br.b));
        },
        .jump => |jmp| {
            try b.addInst(Inst.of(.jump, 0, 0, 0));
            try b.addBranch(SsaPos.of(func_ref, jmp.dst));
        },
        .alloca => |all| {
            // 1. store sp in output
            // 2. load size constant
            // 3. add size to sp
            const reg = registers.find(all.to);
            const size_val = try Value.init(b.ally, std.mem.asBytes(&all.size));
            defer size_val.deinit(b.ally);
            const size_reg = registers.temporary(0);

            try b.addInst(Inst.of(.mov, Vm.SP.n, reg.n, 0));
            try compileLoadConst(b, size_reg, size_val);
            try b.addInst(Inst.of(.iadd, Vm.SP.n, size_reg.n, Vm.SP.n));
        },
        .unary => |un| switch (op) {
            .load => {
                const dst_ty = b.typewelt.get(func.locals[un.a.index]);
                const dst_size = @intCast(u8, dst_ty.sizeOf(b.typewelt.*));
                const arg = registers.find(un.a);
                const to = registers.find(un.to);
                try b.addInst(Inst.of(.load, dst_size, arg.n, to.n));
            },
            else => {
                // most unary opcodes result in a unary instruction
                const ty = b.typewelt.get(func.locals[un.to.index]);
                const opcode: Opcode = switch (ty.*) {
                    .@"bool" => switch (op) {
                        .@"not" => .lnot,
                        else => todoCompileOp(op)
                    },
                    .number => |num| switch (num.layout) {
                        .int, .uint => switch (op) {
                            .@"not" => .bnot,
                            else => todoCompileOp(op)
                        },
                        .float => switch (op) {
                            else => todoCompileOp(op)
                        },
                    },
                    else => {
                        std.debug.panic(
                            "TODO compile unary op for type tag {s}",
                            .{@tagName(ty.*)}
                        );
                    }
                };

                const arg = registers.find(un.a);
                const to = registers.find(un.to);
                try b.addInst(Inst.of(opcode, arg.n, to.n, 0));
            }
        },
        .binary => |bin| {
            const ty = b.typewelt.get(func.locals[bin.to.index]);
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
                else => todoCompileOp(op)
            };

            const lhs = registers.find(bin.a);
            const rhs = registers.find(bin.b);
            const to = registers.find(bin.to);
            try b.addInst(Inst.of(opcode, lhs.n, rhs.n, to.n));
        },
        .unary_eff => |ue| switch (op) {
            .ret => {
                const value = registers.find(ue.a);
                try b.addInst(Inst.of(.mov, value.n, Vm.RETURN.n, 0));
                try b.addInst(Inst.of(.ret, 0, 0, 0));
            },
            else => @panic("TODO")
        },
        .binary_eff => |be| switch (op) {
            .store => {
                const src_ty = b.typewelt.get(func.locals[be.a.index]);
                const bytes = switch (src_ty.sizeOf(b.typewelt.*)) {
                    1, 2, 4, 8 => |n| @intCast(u8, n),
                    else => @panic("TODO store other sizes")
                };

                const src = registers.find(be.a);
                const dst = registers.find(be.b);
                try b.addInst(Inst.of(.store, bytes, src.n, dst.n));
            },
            else => @panic("TODO")
        },
        else => std.debug.panic("TODO compile ssa `{s}`", .{@tagName(op)})
    }
}

fn compileFunc(
    b: *Builder,
    func_ref: ssa.FuncRef,
    func: ssa.Func
) CompileError!void {
    // label mapping for backreference resolution
    var labels = try func.mapLabels(b.ally);
    defer labels.deinit(b.ally);

    // register allocation for ssa vars
    var registers = try RegisterMap.init(b.ally, func.locals.len, func.takes);
    defer registers.deinit(b.ally);

    for (func.ops) |op, i| {
        // resolve any labels once found
        if (labels.get(i)) |label| {
            try b.resolve(SsaPos.of(func_ref, label), b.here());
        }

        try compileOp(b, func_ref, func, &registers, op);
    }
}

pub fn compile(
    ally: Allocator,
    typewelt: *const TypeWelt,
    ssa_prog: ssa.Program
) CompileError!Program {
    var b = Builder.init(ally, typewelt);

    const entry = ssa_prog.funcs[ssa_prog.entry.index];

    if (b.sizeOf(entry.returns) > 8) {
        @panic("TODO allocate for program return value");
    }

    // call entry and exit
    try b.addInst(Inst.of(.call, 0, 0, 0));
    try b.addBranch(SsaPos.ofFunc(ssa_prog, ssa_prog.entry));
    try b.addInst(Inst.of(.exit, 0, 0, 0));

    // compile all functions
    for (ssa_prog.funcs) |func, i| {
        try compileFunc(&b, ssa.FuncRef.of(i), func);
    }

    return b.build(ssa_prog);
}