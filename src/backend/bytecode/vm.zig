//! the thing that executes bytecode programs.

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const Program = @import("bytecode.zig").Program;
const types = @import("../types.zig");
const TypeWelt = types.TypeWelt;
const TypeId = types.TypeId;
const Type = types.Type;
const Env = @import("../env.zig");
const canon = @import("../canon.zig");

const Self = @This();

pub const Register = packed struct {
    pub const Index = u8;
    pub const COUNT = std.math.maxInt(Index);

    n: Index,

    pub fn of(n: Index) Register {
        return Register{ .n = n };
    }

    /// parameter register by callconv
    pub fn param(n: Index) Register {
        return Register.of(n + RESERVED + 1);
    }
};

// reserved registers
pub const IP = Register.of(0); // instruction pointer
pub const SP = Register.of(1); // stack pointer (top of frame)
pub const FP = Register.of(2); // frame pointer (bottom of frame)

pub const RESERVED = 8; // number of reserved registers
pub const RETURN = Register.of(RESERVED); // the return register by callconv

// registers for instructions to use
scratch: [Register.COUNT]u64 = undefined,
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

pub const RuntimeError = Allocator.Error || canon.ResError;

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
    self.set(dst, canon.to(src));
}

fn push(self: *Self, nbytes: usize, src: Register) void {
    const addr = self.get(SP);
    const dst = self.stack[addr..addr + nbytes];
    const from = @ptrCast(*const [8]u8, &self.get(src));
    std.mem.copy(u8, dst, from);
    self.scratch[SP.n] += nbytes;
}

fn pop(self: *Self, nbytes: usize, dst: Register) void {
    self.scratch[SP.n] -= nbytes;
    const addr = self.get(SP);
    const data = self.stack[addr..addr + nbytes];
    self.set(dst, canon.to(data));
}

fn makeFnType(
    self: *Self,
    env: *Env,
    param_slice: u64,
    return_id: u64
) RuntimeError!TypeId {
    // resurrect parameters (*[2]TypeId)
    var slice_ptr = param_slice;
    const params_val = canon.intoValue(&slice_ptr);

    const tyty = try env.identify(Type{ .ty = {} });
    const ty_slice_ty = try env.identify(Type{
        .ptr = .{ .kind = .slice, .to = tyty }
    });
    const expr = try canon.resurrect(
        env.*,
        params_val,
        self.stack,
        null,
        ty_slice_ty
    );
    defer expr.deinit(env.ally);

    const children = expr.data.slice;

    // manipulate parameters into type slice
    var params_buf: [256]TypeId = undefined;
    const params = params_buf[0..children.len];

    for (children) |child, i| {
        params[i] = child.data.ty;
    }

    // id type
    return try env.identify(Type{
        .func = Type.Func{
            .takes = params,
            .returns = TypeId{ .index = return_id },
        }
    });
}

fn now() f64 {
    return @intToFloat(f64, std.time.nanoTimestamp()) * 1e-6;
}

/// debug prints state of vm
fn debug(self: Self, program: Program) void {
    const kz = @import("kritzler");

    const ip = self.get(IP);
    const inst = program.program[ip];

    std.debug.print(
        "[sp: {} fp: {} ip: {}]\n",
        .{self.get(SP), self.get(FP), ip}
    );

    const sl = @ptrCast([*]const u64, self.stack.ptr)[0..self.get(SP) / 8];
    std.debug.print("stack: {d}\n", .{sl});

    std.debug.print("[", .{});
    for (self.scratch[RESERVED..RESERVED + 10]) |n, i| {
        if (i > 0) std.debug.print(" ", .{});
        const index = i + RESERVED;
        std.debug.print("%{}={}", .{index, n});
    }
    std.debug.print("]\n", .{});

    std.debug.print("{}{s}{} {} {} {}\n\n", .{
        kz.Style{ .fg = .red },
        @tagName(inst.op),
        kz.Style{},
        inst.a,
        inst.b,
        inst.c,
    });

    // const stdin = std.io.getStdIn().reader();
    // _ = stdin.readByte() catch {};
}

pub fn execute(
    self: *Self,
    env: *Env,
    program: Program
) RuntimeError!void {
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

        // self.debug(program);

        switch (op) {
            .nop => unreachable,
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
                self.debug(program);

                const cond = Register.of(args[0]);
                ip.* += 1;
                const to = insts[ip.*].toInt();
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
                self.pop(8, FP);
                self.pop(8, IP);
            },
            .call => {
                const dst = Register.of(args[0]);

                self.push(8, IP);
                self.push(8, FP);
                self.mov(SP, FP);
                self.set(IP, self.get(dst));
                continue;
            },
            .pop => {
                const nbytes = args[0];
                const dst = Register.of(args[1]);

                self.pop(nbytes, dst);
            },
            .push => {
                const nbytes = args[0];
                const src = Register.of(args[1]);

                self.push(nbytes, src);
            },
            .load => {
                const nbytes = args[0];
                const src = Register.of(args[1]);
                const dst = Register.of(args[2]);

                // read canonical data
                const addr = self.get(src);
                const data = self.stack[addr..addr + nbytes];
                self.set(dst, canon.to(data));
            },
            .store => {
                const nbytes = args[0];
                const src = Register.of(args[1]);
                const dst = Register.of(args[2]);

                // format register canonically
                const value = self.get(src);
                const data = canon.from(&value);
                // write
                const addr = self.get(dst);
                const slice = self.stack[addr..addr + nbytes];
                std.mem.set(u8, slice, 0);
                std.mem.copy(u8, slice, data);
            },
            inline .lnot, .bnot, .slice_ty => |v| {
                const arg = self.get(Register.of(args[0]));
                const to = Register.of(args[1]);

                const value: u64 = switch (comptime v) {
                    .lnot => @boolToInt(arg == 0),
                    .bnot => ~arg,
                    .slice_ty => slice: {
                        const el_ty = TypeId{ .index = arg };
                        const ty = Type.initPtr(.slice, el_ty);
                        const tid = try env.identify(ty);

                        break :slice tid.index;
                    },
                    else => unreachable
                };

                self.set(to, value);
            },
            inline .iadd, .isub, .imul, .idiv, .imod, .lor, .land, .bor,
            .band, .xor, .shl, .shr, .fn_ty => |v| {
                const lhs = self.get(Register.of(args[0]));
                const rhs = self.get(Register.of(args[1]));
                const to = Register.of(args[2]);

                const value: u64 = switch (comptime v) {
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
                    .fn_ty => (try self.makeFnType(env, lhs, rhs)).index,
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
    const stdout = std.io.getStdOut().writer();
    const t = now() - start;
    stdout.print("vm.run took {d:.6}ms\n", .{t}) catch {};
}