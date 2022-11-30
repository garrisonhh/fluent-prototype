const std = @import("std");
const stdout = std.io.getStdOut().writer();
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const kz = @import("kritzler");
const ssa = @import("ssa.zig");
const types = @import("types.zig");
const TypeId = types.TypeId;
const TypeWelt = types.TypeWelt;
const Value = @import("value.zig");

/// a single operation
const BcOpcode = enum(u8) {
    exit, // no args
    mov, // mov %src %dst

    imm2, // imm %dst $hi $lo
    imm4, // imm %dst ; load next opcode as u32
    imm8, // imm %dst ; load next two opcodes as u64

    // control flow
    jump, // load next opcode as u32 index, and jump to that instruction
    jump_if, // jump_if %cond ; same as jump, but only if %test != 0
    ret, // no args. mov fp -> sp, pop fp, pop ip
    call, // call %dst ; push ip, push fp, mov sp -> fp, mov dst -> ip

    // stack manipulation
    pop, // pop $bytes %dst
    push, // push $bytes %src

    // memory manipulation
    load, // load $bytes %src %dst ; read from a pointer
    store, // store $bytes %src %dst
    static, // static $bytes %src %dst ; load but from static

    // basic operations
    // take form: op %a %b %dst
    iadd,
    isub,
    imul,
    idiv,
    imod,
    lor,
    land,
    lnot,
    bor,
    band,
    bnot,
    xor,

    // special
    debug, // debug %src ; prints a register for debugging
};

/// all operations take up to 3 operands, which typically represent VM registers
const BcInst = packed struct(u32) {
    const Self = @This();

    op: BcOpcode,
    a: u8,
    b: u8,
    c: u8,

    fn of(op: BcOpcode, a: u8, b: u8, c: u8) Self {
        return Self{
            .op = op,
            .a = a,
            .b = b,
            .c = c,
        };
    }

    fn fromInt(n: u32) Self {
        return @bitCast(Self, n);
    }

    fn toInt(self: Self) u32 {
        return @bitCast(u32, self);
    }

    fn getArgs(self: *const Self) *const [3]u8 {
        return @ptrCast(*const [3]u8, &@ptrCast(*const [4]u8, self)[1]);
    }
};

const BcProgram = struct {
    const Self = @This();

    returns: TypeId,
    static: []u8,
    program: []align(16) BcInst,

    pub fn deinit(self: Self, ally: Allocator) void {
        ally.free(self.static);
        ally.free(self.program);
    }

    fn renderOpcode(ctx: *kz.Context, opcode: BcOpcode) !kz.Ref {
        // for adding buffered spaces
        const width = comptime width: {
            var max: usize = 0;
            for (std.meta.fieldNames(@TypeOf(opcode))) |field| {
                max = @max(field.len, max);
            }

            break :width max;
        };
        const width_fmt = comptime std.fmt.comptimePrint("{}", .{width});
        const tag = @tagName(opcode);

        return try ctx.print(.{}, "{s:<" ++ width_fmt ++ "}", .{tag});
    }

    fn renderImm(ctx: *kz.Context, imm: u64) !kz.Ref {
        const sym = try ctx.print(.{}, "$", .{});
        const num = try ctx.print(.{ .fg = .magenta }, "{X}", .{imm});
        return try ctx.slap(sym, num, .right, .{});
    }

    fn renderReg(ctx: *kz.Context, reg: usize) !kz.Ref {
        const sym = try ctx.print(.{}, "%", .{});
        const id = try ctx.print(.{ .fg = .red }, "{}", .{reg});
        return try ctx.slap(sym, id, .right, .{});
    }

    fn renderPos(ctx: *kz.Context, pos: usize) !kz.Ref {
        const sym = try ctx.print(.{}, "@", .{});
        const tex = try ctx.print(.{ .fg = .cyan }, "{}", .{pos});
        return try ctx.slap(sym, tex, .right, .{});
    }

    pub fn render(self: Self, ctx: *kz.Context) !kz.Ref {
        var lines = std.ArrayList(kz.Ref).init(ctx.ally);
        defer lines.deinit();

        var line = try std.ArrayList(kz.Ref).initCapacity(ctx.ally, 8);
        defer line.deinit();

        // render each inst
        const insts = self.program;
        var i: usize = 0;
        while (i < insts.len) : (i += 1) {
            const inst = insts[i];
            defer line.shrinkRetainingCapacity(0);

            try line.append(try ctx.print(.{ .fg = .cyan }, "{:>4}", .{i}));
            try line.append(try renderOpcode(ctx, inst.op));

            // args
            const args = inst.getArgs();
            switch (inst.op) {
                .exit, .ret => {},
                .mov => {
                    try line.append(try renderReg(ctx, args[0]));
                    try line.append(try renderReg(ctx, args[1]));
                },
                .imm2 => {
                    const n = (@intCast(u16, args[1]) << 8) | args[2];
                    try line.append(try renderReg(ctx, args[0]));
                    try line.append(try renderImm(ctx, n));
                },
                .imm4 => {
                    const n = insts[i + 1].toInt();
                    i += 1;

                    try line.append(try renderReg(ctx, args[0]));
                    try line.append(try renderImm(ctx, n));
                },
                .imm8 => {
                    const hi = insts[i + 1].toInt();
                    const lo = insts[i + 2].toInt();
                    const n = (@intCast(u64, hi) << 32) | lo;
                    i += 2;

                    try line.append(try renderReg(ctx, args[0]));
                    try line.append(try renderImm(ctx, n));
                },
                .jump, .call => {
                    const n = insts[i + 1].toInt();
                    i += 1;

                    try line.append(try renderPos(ctx, n));
                },
                .jump_if => {
                    const n = insts[i + 1].toInt();
                    i += 1;

                    try line.append(try renderReg(ctx, args[0]));
                    try line.append(try renderPos(ctx, n));
                },
                .iadd, .isub, .imul, .idiv, .imod, .lor, .land, .bor, .band,
                .xor => {
                    for (args) |arg| {
                        try line.append(try renderReg(ctx, arg));
                    }
                },
                .lnot, .bnot => {
                    for (args[0..2]) |arg| {
                        try line.append(try renderReg(ctx, arg));
                    }
                },
                .debug => try line.append(try renderReg(ctx, args[0])),
                else => std.debug.panic(
                    "cannot render op `{s}` yet\n",
                    .{@tagName(inst.op)}
                )
            }

            // stack the line
            const tex = try ctx.stack(line.items, .right, .{ .space = 1 });
            try lines.append(tex);
        }

        // stack the lines
        return try ctx.stack(lines.items, .bottom, .{});
    }
};

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

const BcBuilder = struct {
    const Self = @This();

    ally: Allocator,
    typewelt: *const TypeWelt,
    static: std.ArrayListUnmanaged(u8) = .{},
    program: std.ArrayListAlignedUnmanaged(BcInst, 16) = .{},
    refs: std.AutoHashMapUnmanaged(SsaPos, BackRef) = .{},

    fn init(ally: Allocator, typewelt: *const TypeWelt) Self {
        return Self{
            .ally = ally,
            .typewelt = typewelt,
        };
    }

    /// invalidates this builder
    fn build(self: *Self, ssa_prog: ssa.Program) BcProgram {
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

        return BcProgram{
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
                .resolved => |ref| try self.addInst(BcInst.fromInt(ref.index)),
                .unresolved => |*list| {
                    try list.append(self.ally, self.here());
                    try self.addInst(BcInst.fromInt(0));
                },
            }
        } else {
            // add unresolved backref
            res.value_ptr.* = BackRef{ .unresolved = .{} };
            try res.value_ptr.unresolved.append(self.ally, self.here());
            try self.addInst(BcInst.fromInt(0));
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
                self.program.items[ref.index] = BcInst.fromInt(to.index);
            }
        }

        // keep resolved value
        res.value_ptr.* = BackRef{ .resolved = to };
    }

    fn addInst(self: *Self, inst: BcInst) Allocator.Error!void {
        try self.program.append(self.ally, inst);
    }

    fn addStatic(self: *Self, data: []const u8) Allocator.Error!usize {
        const index = self.static.items.len;
        try self.static.appendSlice(self.ally, data);
        return index;
    }
};

const RuntimeError = Allocator.Error;

const Vm = struct {
    const Self = @This();

    // reserved registers
    // TODO do I want to remove these and just have them as locals in `run()`?
    const IP = Register.of(0); // instruction pointer
    const SP = Register.of(1); // stack pointer (top of frame)
    const FP = Register.of(2); // frame pointer (bottom of frame)

    const RESERVED = 8; // number of reserved registers

    ally: Allocator,
    // registers for instructions to use
    scratch: [256]u64 = undefined,
    // the call stack, used for temporary storage and function memory
    stack: []align(16) u8,

    fn init(ally: Allocator, stack_size: usize) Allocator.Error!Self {
        return Self{
            .ally = ally,
            .stack = try ally.alignedAlloc(u8, 16, stack_size),
        };
    }

    fn deinit(self: Self) void {
        self.ally.free(self.stack);
    }

    fn get(self: Self, reg: Register) u64 {
        return self.scratch[reg.n];
    }

    fn set(self: *Self, reg: Register, value: u64) void {
        self.scratch[reg.n] = value;
    }

    fn mov(self: *Self, src: Register, dst: Register) void {
        self.scratch[dst.n] = self.scratch[src.n];
    }

    fn push(self: *Self, bytes: usize, src: Register) void {
        const dst = self.stack[self.scratch[SP.n]..];
        const src_bytes = @ptrCast([*]u8, &self.scratch[src.n])[0..bytes];
        std.mem.copy(u8, dst, src_bytes);
        self.scratch[SP.n] += bytes;
    }

    fn pop(self: *Self, bytes: usize, dst: Register) void {
        self.scratch[SP.n] -= bytes;
        const sp = self.scratch[SP.n];
        const src = self.stack[sp..sp + bytes];
        const dst_bytes = @ptrCast([*]u8, &self.scratch[dst.n])[0..bytes];
        std.mem.copy(u8, dst_bytes, src);
    }

    const Now = if (builtin.mode == .Debug) f64 else void;

    fn now() Now {
        if (builtin.mode == .Debug) {
            return @intToFloat(f64, std.time.nanoTimestamp()) * 1e-6;
        } else {
            return {};
        }
    }

    fn run(self: *Self, program: BcProgram) RuntimeError!void {
        const start = now();

        std.mem.set(u64, self.scratch[0..Vm.RESERVED], 0);

        const insts = program.program;
        const ip = &self.scratch[IP.n];

        while (ip.* < insts.len) {
            const inst = insts[ip.*];
            const op = inst.op;
            const args = inst.getArgs();

            switch (op) {
                .exit => {
                    if (builtin.mode == .Debug) {
                        const t = now() - start;
                        std.debug.print("vm.run took {d:.6}ms\n", .{t});
                    }

                    return;
                },
                .mov => {
                    const src = Register.of(args[0]);
                    const dst = Register.of(args[1]);
                    self.mov(src, dst);
                },
                .imm2 => {
                    const dst = Register.of(args[0]);
                    const hi = args[1];
                    const lo = args[2];
                    self.set(dst, (@intCast(u16, hi) << 8) | lo);
                },
                .imm4 => {
                    const dst = Register.of(args[0]);
                    const n = insts[ip.* + 1].toInt();
                    ip.* += 1;
                    self.set(dst, n);
                },
                .imm8 => {
                    const dst = Register.of(args[0]);
                    const hi = insts[ip.* + 1].toInt();
                    const lo = insts[ip.* + 2].toInt();
                    ip.* += 2;
                    self.set(dst, (@intCast(u64, hi) << 32) | lo);
                },
                .jump => {
                    const to = insts[ip.* + 1].toInt();
                    self.set(IP, to);
                    continue;
                },
                .jump_if => {
                    const cond = Register.of(args[0]);
                    const to = insts[ip.* + 1].toInt();
                    ip.* += 1;
                    if (self.get(cond) != 0) {
                        self.set(IP, to);
                        continue;
                    }
                },
                .call => {
                    const n = insts[ip.* + 1].toInt();
                    ip.* += 1;
                    self.push(8, IP);
                    self.mov(SP, FP);
                    self.set(IP, n);
                    continue;
                },
                .ret => {
                    self.mov(FP, SP);
                    self.pop(8, IP);
                },
                inline .lnot, .bnot => |v| {
                    const arg = self.get(Register.of(args[0]));
                    const to = Register.of(args[1]);

                    const value: u64 = comptime switch (v) {
                        .lnot => @boolToInt(arg == 0),
                        .bnot => ~arg,
                        else => unreachable
                    };

                    self.set(to, value);
                },
                inline .iadd, .isub, .imul, .idiv, .imod, .lor, .land, .bor,
                .band, .xor => |v| {
                    const lhs = self.get(Register.of(args[0]));
                    const rhs = self.get(Register.of(args[1]));
                    const to = Register.of(args[2]);

                    const value: u64 = comptime switch (v) {
                        .iadd => lhs +% rhs,
                        .isub => lhs -% rhs,
                        .imul => lhs * rhs,
                        .idiv => @divFloor(lhs, rhs),
                        .imod => lhs % rhs,
                        .lor => @boolToInt(lhs != 0 or rhs != 0),
                        .land => @boolToInt(lhs != 0 and rhs != 0),
                        .bor => lhs | rhs,
                        .band => lhs & rhs,
                        .xor => lhs ^ rhs,
                        else => unreachable
                    };

                    self.set(to, value);
                },
                .debug => {
                    const src = Register.of(args[0]);
                    std.debug.print("debug: {d}\n", .{self.get(src)});
                },
                else => std.debug.panic(
                    "instruction {s} not implemented",
                    .{@tagName(op)}
                )
            }

            // iterate ip
            ip.* += 1;
        }
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

pub const CompileError =
    Allocator.Error;

const Register = packed struct {
    const Self = @This();

    const RETURN = of(Vm.RESERVED);

    n: u8,

    fn of(n: u8) Self {
        return Self{ .n = n };
    }
};

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
    b: *BcBuilder,
    reg: Register,
    value: Value
) CompileError!void {
    if (value.ptr.len <= 8) {
        // convert value to uint
        // TODO I should probably do this with a union, this is already a source
        // of bugs
        var bytes: [8]u8 align(8) = undefined;
        std.mem.set(u8, &bytes, 0);

        var i: usize = 1;
        while (i <= value.ptr.len) : (i += 1) {
            bytes[bytes.len - i] = value.ptr[value.ptr.len - i];
        }

        var n = @ptrCast(*const u64, &bytes).*;
        const zeroes = @clz(n);

        // store with smallest imm op possible
        if (zeroes >= 0x30) {
            const hi = @intCast(u8, n >> 8);
            const lo = @truncate(u8, n);
            try b.addInst(BcInst.of(.imm2, reg.n, hi, lo));
        } else if (zeroes >= 0x20) {
            try b.addInst(BcInst.of(.imm4, reg.n, 0, 0));
            try b.addInst(BcInst.fromInt(@intCast(u32, n)));
        } else {
            try b.addInst(BcInst.of(.imm8, reg.n, 0, 0));
            try b.addInst(BcInst.fromInt(@intCast(u32, n >> 32)));
            try b.addInst(BcInst.fromInt(@truncate(u32, n)));
        }
    } else {
        @panic("TODO compile ldc -> static");
    }
}

fn compileOp(
    b: *BcBuilder,
    func: ssa.Func,
    registers: *RegisterMap,
    op: ssa.Op,
) CompileError!void {
    switch (op.classify()) {
        .ldc => |ldc| {
            const reg = registers.find(ldc.to);
            try compileLoadConst(b, reg, func.consts[ldc.a.index]);
        },
        .alloca => |all| {
            // 1. store sp in output
            // 2. load size constant
            // 3. add size to sp
            const reg = registers.find(all.to);
            const size_val = try Value.init(b.ally, std.mem.asBytes(&all.size));
            defer size_val.deinit(b.ally);
            const size_reg = registers.temporary(0);

            try b.addInst(BcInst.of(.mov, Vm.SP.n, reg.n, 0));
            try compileLoadConst(b, size_reg, size_val);
            try b.addInst(BcInst.of(.iadd, Vm.SP.n, size_reg.n, Vm.SP.n));
        },
        .unary => |un| {
            const ty = b.typewelt.get(func.locals[un.to.index]);
            const opcode: BcOpcode = switch (ty.*) {
                .@"bool" => switch (op) {
                    .@"not" => .lnot,
                    else => @panic("TODO")
                },
                .number => |num| switch (num.layout) {
                    .int, .uint => switch (op) {
                        .@"not" => .bnot,
                        else => @panic("TODO")
                    },
                    .float => @panic("TODO")
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
            try b.addInst(BcInst.of(opcode, arg.n, to.n, 0));
        },
        .binary => |bin| {
            const ty = b.typewelt.get(func.locals[bin.to.index]);
            const opcode: BcOpcode = switch (ty.*) {
                .@"bool" => switch (op) {
                    .@"or" => .lor,
                    .@"and" => .land,
                    else => @panic("TODO")
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
                        else => @panic("TODO")
                    },
                    .float => switch (op) {
                        else => @panic("TODO")
                    },
                },
                else => @panic("TODO")
            };

            const lhs = registers.find(bin.a);
            const rhs = registers.find(bin.b);
            const to = registers.find(bin.to);
            try b.addInst(BcInst.of(opcode, lhs.n, rhs.n, to.n));
        },
        .unary_eff => |ue| switch (op) {
            .ret => {
                const value = registers.find(ue.a);
                try b.addInst(BcInst.of(.mov, value.n, Register.RETURN.n, 0));
                try b.addInst(BcInst.of(.ret, 0, 0, 0));
            },
            else => @panic("TODO")
        },
        else => std.debug.panic("TODO compile ssa `{s}`", .{@tagName(op)})
    }
}

fn compileFunc(
    b: *BcBuilder,
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

        try compileOp(b, func, &registers, op);
    }
}

pub fn compile(
    ally: Allocator,
    typewelt: *const TypeWelt,
    ssa_prog: ssa.Program
) CompileError!BcProgram {
    var b = BcBuilder.init(ally, typewelt);

    const entry = ssa_prog.funcs[ssa_prog.entry.index];

    if (b.sizeOf(entry.returns) > 8) {
        @panic("TODO allocate for program return value");
    }

    // call entry and exit
    try b.addInst(BcInst.of(.call, 0, 0, 0));
    try b.addBranch(SsaPos.ofFunc(ssa_prog, ssa_prog.entry));
    try b.addInst(BcInst.of(.exit, 0, 0, 0));

    // compile all functions
    for (ssa_prog.funcs) |func, i| {
        try compileFunc(&b, ssa.FuncRef.of(i), func);
    }

    return b.build(ssa_prog);
}

/// returns a value allocated on ally
pub fn run(
    ally: Allocator,
    typewelt: *TypeWelt,
    program: BcProgram
) RuntimeError!Value {
    // run program
    var vm = try Vm.init(ally, 32 * 1024);
    defer vm.deinit();

    try vm.run(program);

    // copy return value
    const ret_size = typewelt.get(program.returns).sizeOf(typewelt.*);
    const value = try Value.initEmpty(ally, ret_size);

    const out: u64 = vm.scratch[Vm.RESERVED];
    if (ret_size > 8) {
        // deref r0 and read
        const data = @intToPtr([*]const u8, out)[0..ret_size];
        std.mem.copy(u8, value.ptr, data);
    } else {
        // copy lowest bytes of r0
        const data = @ptrCast(*const [8]u8, &out)[8 - ret_size..];
        std.mem.copy(u8, value.ptr, data);
    }

    return value;
}