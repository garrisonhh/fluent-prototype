//! bytecode program representation

const std = @import("std");
const Allocator = std.mem.Allocator;
const kz = @import("kritzler");
const canon = @import("../canon.zig");
const Vm = @import("vm.zig");
const Builder = @import("building.zig").Builder;
const ssa = @import("../ssa/ssa.zig");
const Pos = ssa.Pos;

/// a single operation
pub const Opcode = enum(u8) {
    // NOTE '$bytes' here expects 1, 2, 4, or 8

    nop,

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
    load, // load $bytes %src %dst ; read pointer %src into value %dst
    store, // store $bytes %src %dst ; write value %src into pointer %dst

    // basic operations
    // take one of two forms:
    // unary: op %a %dst
    // binary: op %a %b %dst
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
    shl,
    shr,

    // type operations
    fn_ty, // binary
    slice_ty, // unary
};

/// all operations take up to 3 operands, which typically represent VM registers
/// see comments for each opcode for the operand usage
pub const Inst = packed struct(u32) {
    const Self = @This();

    op: Opcode,
    a: u8,
    b: u8,
    c: u8,

    pub fn fromInt(n: u32) Self {
        return @bitCast(Self, n);
    }

    pub fn toInt(self: Self) u32 {
        return @bitCast(u32, self);
    }

    pub fn getArgs(self: *const Self) *const [3]u8 {
        comptime std.debug.assert(@sizeOf(Self) == 4);
        return @ptrCast(*const [3]u8, &@ptrCast(*const [4]u8, self)[1]);
    }
};

pub const InstRef = packed struct {
    const Self = @This();

    index: u32,

    pub fn of(index: u32) Self {
        return Self{ .index = index };
    }
};

/// program is an executable 'view' into the env's bytecode builder.
///
/// in order to execute, the vm can call the entry point and then observe the
/// return value.
pub const Program = struct {
    const Self = @This();

    entry: u64,
    program: []const align(16) Inst,

    fn renderOpcode(ctx: *kz.Context, opcode: Opcode) !kz.Ref {
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
        const num = try ctx.print(.{ .fg = .magenta }, "0x{X}", .{imm});
        return try ctx.slap(sym, num, .right, .{});
    }

    fn renderReg(ctx: *kz.Context, reg: usize) !kz.Ref {
        const cyan = kz.Style{ .fg = .cyan };
        const red = kz.Style{ .fg = .red };

        const sym = try ctx.print(.{}, "%", .{});
        const id = switch (reg) {
            Vm.IP.n => try ctx.print(cyan, "ip", .{}),
            Vm.FP.n => try ctx.print(cyan, "fp", .{}),
            Vm.SP.n => try ctx.print(cyan, "sp", .{}),
            Vm.RETURN.n => try ctx.print(cyan, "{}", .{reg}),
            else => try ctx.print(red, "{}", .{reg}),
        };

        return try ctx.slap(sym, id, .right, .{});
    }

    fn renderPos(ctx: *kz.Context, pos: usize) !kz.Ref {
        const sym = try ctx.print(.{}, "@", .{});
        const tex = try ctx.print(.{ .fg = .cyan }, "{}", .{pos});
        return try ctx.slap(sym, tex, .right, .{});
    }

    pub fn render(
        self: Self,
        ctx: *kz.Context,
        comments: Builder.Comments
    ) !kz.Ref {
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

            // check for a comment
            const iref = InstRef.of(@intCast(u32, i));
            if (comments.get(iref)) |list| {
                for (list.items) |comment| {
                    const gray = kz.Style{ .special = .faint };
                    try lines.append(try ctx.print(gray, "{s}", .{comment}));
                }
            }

            try line.append(try ctx.print(.{ .fg = .cyan }, "{:>4}", .{i}));
            try line.append(try renderOpcode(ctx, inst.op));

            // args
            const args = inst.getArgs();
            switch (inst.op) {
                .nop, .ret => {},
                .mov => {
                    try line.append(try renderReg(ctx, args[0]));
                    try line.append(try renderReg(ctx, args[1]));
                },
                .imm2 => {
                    const n = canon.to(args[1..3]);
                    try line.append(try renderReg(ctx, args[0]));
                    try line.append(try renderImm(ctx, n));
                },
                .imm4 => {
                    const bytes = @ptrCast(*const [4]u8, &insts[i + 1]);
                    const n = canon.to(bytes);
                    i += 1;

                    try line.append(try renderReg(ctx, args[0]));
                    try line.append(try renderImm(ctx, n));
                },
                .imm8 => {
                    const bytes = @ptrCast(*const [8]u8, insts[i + 1..i + 3]);
                    const n = canon.to(bytes);
                    i += 2;

                    try line.append(try renderReg(ctx, args[0]));
                    try line.append(try renderImm(ctx, n));
                },
                .push, .pop => {
                    try line.append(try renderImm(ctx, args[0]));
                    try line.append(try renderReg(ctx, args[1]));
                },
                .jump => {
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
                .load, .store => {
                    try line.append(try renderImm(ctx, args[0]));
                    try line.append(try renderReg(ctx, args[1]));
                    try line.append(try renderReg(ctx, args[2]));
                },
                .iadd, .isub, .imul, .idiv, .imod, .lor, .land, .bor, .band,
                .xor, .fn_ty => {
                    for (args) |arg| {
                        try line.append(try renderReg(ctx, arg));
                    }
                },
                .lnot, .bnot, .slice_ty => {
                    for (args[0..2]) |arg| {
                        try line.append(try renderReg(ctx, arg));
                    }
                },
                .call => try line.append(try renderReg(ctx, args[0])),
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

/// constructors for Insts
pub const Construct = struct {
    const Reg = Vm.Register;

    fn make(op: Opcode, a: u8, b: u8, c: u8) Inst {
        return Inst{
            .op = op,
            .a = a,
            .b = b,
            .c = c,
        };
    }

    fn conR(comptime op: Opcode) fn(Reg) Inst {
        return struct {
            fn f(a: Reg) Inst {
                return make(op, a.n, 0, 0);
            }
        }.f;
    }

    fn conRR(comptime op: Opcode) fn(Reg, Reg) Inst {
        return struct {
            fn f(a: Reg, b: Reg) Inst {
                return make(op, a.n, b.n, 0);
            }
        }.f;
    }

    fn conBR(comptime op: Opcode) fn(u8, Reg) Inst {
        return struct {
            fn f(a: u8, b: Reg) Inst {
                return make(op, a, b.n, 0);
            }
        }.f;
    }

    fn conRRR(comptime op: Opcode) fn(Reg, Reg, Reg) Inst {
        return struct {
            fn f(a: Reg, b: Reg, c: Reg) Inst {
                return make(op, a.n, b.n, c.n);
            }
        }.f;
    }

    fn conBRR(comptime op: Opcode) fn(u8, Reg, Reg) Inst {
        return struct {
            fn f(a: u8, b: Reg, c: Reg) Inst {
                return make(op, a, b.n, c.n);
            }
        }.f;
    }

    /// constructs one of the immediate ops to load these bytes into a register
    pub fn imm(
        b: *Builder,
        ally: Allocator,
        dst: Reg,
        bytes: []const u8
    ) Allocator.Error!void {
        std.debug.assert(bytes.len <= 8);

        var buf: [8]u8 align(8) = [1]u8{0} ** 8;
        std.mem.copy(u8, &buf, bytes);

        // find nbytes
        var nbytes: usize = 8;
        while (nbytes > 0) : (nbytes -= 1) {
            if (buf[nbytes - 1] != 0) {
                break;
            }
        }

        if (nbytes < 2) {
            try b.addInst(ally, make(.imm2, dst.n, buf[0], buf[1]));
        } else if (nbytes < 4) {
            const n = @ptrCast(*const u32, &buf).*;
            try b.addInst(ally, make(.imm4, dst.n, 0, 0));
            try b.addInst(ally, Inst.fromInt(n));
        } else {
            const ns = @ptrCast(*const [2]u32, &buf);
            try b.addInst(ally, make(.imm8, dst.n, 0, 0));
            try b.addInst(ally, Inst.fromInt(ns[0]));
            try b.addInst(ally, Inst.fromInt(ns[1]));
        }
    }

    pub fn immBackref(
        b: *Builder,
        ally: Allocator,
        dst: Reg,
        pos: Pos
    ) Allocator.Error!void {
        try b.addInst(ally, make(.imm4, dst.n, 0, 0));
        try b.addBranch(ally, pos);
    }

    pub fn jump(b: *Builder, ally: Allocator, dst: Pos) Allocator.Error!void {
        try b.addInst(ally, make(.jump, 0, 0, 0));
        try b.addBranch(ally, dst);
    }

    pub fn jump_if(
        b: *Builder,
        ally: Allocator,
        cond: Reg,
        dst: Pos
    ) Allocator.Error!void {
        try b.addInst(ally, make(.jump_if, cond.n, 0, 0));
        try b.addBranch(ally, dst);
    }

    pub const nop = make(.nop, 0, 0, 0);
    pub const mov = conRR(.mov);
    pub const ret = make(.ret, 0, 0, 0);
    pub const call = conR(.call);

    pub const pop = conBR(.pop);
    pub const push = conBR(.push);

    pub const load = conBRR(.load);
    pub const store = conBRR(.store);

    pub const iadd = conRRR(.iadd);
    pub const isub = conRRR(.isub);
    pub const imul = conRRR(.imul);
    pub const idiv = conRRR(.idiv);
    pub const imod = conRRR(.imod);

    pub const land = conRRR(.land);
    pub const lor = conRRR(.lor);
    pub const lnot = conRR(.lnot);

    pub const bnot = conRR(.bnot);
    pub const xor = conRRR(.xor);

    pub const fn_ty = conRRR(.fn_ty);
    pub const slice_ty = conRR(.slice_ty);
};