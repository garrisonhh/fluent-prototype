//! the thing that executes bytecode programs.

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const bytecode = @import("bytecode.zig");
const Program = bytecode.Program;
const canon = @import("canon.zig");
const TypeWelt = @import("../types.zig").TypeWelt;
const Value = @import("../value.zig");

const Self = @This();

pub const Register = packed struct {
    n: u8,

    pub fn of(n: u8) Register {
        return Register{ .n = n };
    }
};

// reserved registers
// TODO do I want to remove these and just have them as locals in `run()`?
const IP = Register.of(0); // instruction pointer
pub const SP = Register.of(1); // stack pointer (top of frame)
pub const FP = Register.of(2); // frame pointer (bottom of frame)

pub const RESERVED = 8; // number of reserved registers
pub const RETURN = Register.of(RESERVED); // the return register by callconv

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

const RuntimeError = Allocator.Error;

fn execute(self: *Self, program: Program) RuntimeError!void {
    const start = now();

    std.mem.set(u64, self.scratch[0..RESERVED], 0);

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

                // TODO remove
                if (src.n == SP.n) {
                    std.debug.print("read sp at {} into %{}\n", .{self.get(dst), dst.n});
                }
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
            .ret => {
                self.mov(FP, SP);
                self.pop(8, IP);
            },
            .call => {
                const n = insts[ip.* + 1].toInt();
                ip.* += 1;
                self.push(8, IP);
                self.mov(SP, FP);
                self.set(IP, n);
                continue;
            },
            .pop => {
                const nbytes = args[0];
                const dst = Register.of(args[1]);
                self.stack[SP.n] -= nbytes;
                const addr = self.get(SP);
                const data = self.stack[addr..addr + nbytes];
                self.set(dst, canon.toCanonical(data));
            },
            .push => {
                const nbytes = args[0];
                const src = Register.of(args[0]);
                var buf: [8]u8 = undefined;
                const data = buf[0..nbytes];
                canon.fromCanonical(data, self.get(src));
                const addr = self.get(SP);
                std.mem.copy(u8, self.stack[addr..addr + nbytes], data);
            },
            .load => {
                const nbytes = args[0];
                const src = Register.of(args[1]);
                const dst = Register.of(args[2]);
                // read canonical data
                const addr = self.get(src);
                const data = self.stack[addr..addr + nbytes];
                self.set(dst, canon.toCanonical(data));
            },
            .store => {
                const nbytes = args[0];
                const src = Register.of(args[1]);
                const dst = Register.of(args[2]);
                // format register canonically
                var buf: [8]u8 = undefined;
                const data = buf[0..nbytes];
                canon.fromCanonical(data, self.get(src));
                // write
                const addr = self.get(dst);
                std.mem.copy(u8, self.stack[addr..addr + nbytes], data);
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
            .band, .xor, .shl, .shr => |v| {
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
                    .shl => lhs << @intCast(u6, rhs),
                    .shr => lhs >> @intCast(u6, rhs),
                    else => unreachable
                };

                self.set(to, value);
            },
            .debug => {
                const src = Register.of(args[0]);
                std.debug.print("debug: {d}\n", .{self.get(src)});
            },
            // else => std.debug.panic(
                // "instruction {s} not implemented",
                // .{@tagName(op)}
            // )
        }

        // iterate ip
        ip.* += 1;
    }
}

/// returns a value allocated on ally
pub fn run(
    ally: Allocator,
    typewelt: *TypeWelt,
    program: Program
) RuntimeError!Value {
    // run program
    var vm = try Self.init(ally, 32 * 1024);
    defer vm.deinit();

    try vm.execute(program);

    // copy return value
    const ret_size = typewelt.get(program.returns).sizeOf(typewelt.*);
    const value = try Value.initEmpty(ally, ret_size);

    const out: u64 = vm.scratch[RESERVED];
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