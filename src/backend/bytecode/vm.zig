//! the thing that executes bytecode programs.

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const Program = @import("bytecode.zig").Program;
const canon = @import("../canon.zig");

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

// registers for instructions to use
scratch: [256]u64 = undefined,
// the call stack, used for temporary storage and function memory
stack: []align(16) u8,

pub fn init(ally: Allocator, stack_size: usize) Allocator.Error!Self {
    return Self{
        .stack = try ally.alignedAlloc(u8, 16, stack_size),
    };
}

pub fn deinit(self: Self, ally: Allocator) void {
    ally.free(self.stack);
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

fn mov_imm(self: *Self, src: []const u8, dst: Register) void {
    std.debug.assert(src.len <= 8);
    comptime if (builtin.cpu.arch.endian() != .Little) {
        @compileError("this code is optimized by space for little-endian arch");
    };

    const reg = @ptrCast(*[8]u8, &self.scratch[dst.n]);
    self.set(dst, 0);
    std.mem.copy(u8, reg, src);
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

    const insts = program.program;
    const ip = &self.scratch[IP.n];

    // initialization
    std.mem.set(u64, self.scratch[0..RESERVED], 0);
    ip.* = program.entry;

    // execution loop
    loop: while (ip.* < insts.len) {
        const inst = insts[ip.*];
        const op = inst.op;
        const args = inst.getArgs();

        switch (op) {
            .mov => {
                const src = Register.of(args[0]);
                const dst = Register.of(args[1]);
                self.mov(src, dst);
            },
            .imm2 => {
                const dst = Register.of(args[0]);
                self.mov_imm(args[1..3], dst);
            },
            .imm4 => {
                const dst = Register.of(args[0]);
                ip.* += 1;
                self.mov_imm(@ptrCast(*const [4]u8, &insts[ip.*]), dst);
            },
            .imm8 => {
                const dst = Register.of(args[0]);
                const slice = insts[ip.*..ip.* + 2];
                ip.* += 2;
                self.mov_imm(@ptrCast(*const [8]u8, slice), dst);
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
                // return from the first frame
                if (self.get(FP) == 0) {
                    break :loop;
                }

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

        // iterate ip. this cannot be in the second while loop stmt because I
        // don't want this to run on `continue` when I manipulate the IP
        // otherwise
        ip.* += 1;
    }

    // end of execution
    if (builtin.mode == .Debug) {
        const stdout = std.io.getStdOut().writer();
        const t = now() - start;
        stdout.print("vm.run took {d:.6}ms\n", .{t}) catch {};
    }
}

/// expects ret_buf to have a size equivalent to the program's return size.
/// this can be determined by calling Env.sizeOf on the ssa func's return type
pub fn run(vm: *Self, ret: []u8, program: Program) RuntimeError!void {
    // run program
    try vm.execute(program);

    // copy return value
    const out: u64 = vm.scratch[RESERVED];
    if (ret.len > 8) {
        // deref r0 and read
        const data = @intToPtr([*]const u8, out)[0..ret.len];
        std.mem.copy(u8, ret, data);
    } else {
        canon.fromCanonical(ret, out);
    }
}