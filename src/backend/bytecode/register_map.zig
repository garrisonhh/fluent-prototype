//! during compilation, every live SSA temporary needs to be mapped to register
//! storage in the bytecode vm. this data structure manages those mappings.
//!
//! RegisterMap expects to be updated as you iteratively move through the
//! ssa function. this allows

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const Env = @import("../env.zig");
const bytecode = @import("bytecode.zig");
const Bc = bytecode.Construct;
const building = @import("building.zig");
const Builder = building.Builder;
const ssa = @import("../ssa/ssa.zig");
const Local = ssa.Local;
const Func = ssa.Func;
const FuncRef = ssa.FuncRef;
const Pos = ssa.Pos;
const Prophecy = ssa.Prophecy;
const Vm = @import("vm.zig");
const Register = Vm.Register;

pub const Error = Allocator.Error;

const Self = @This();

const RegList = std.BoundedArray(Register, Register.COUNT);

func: *const Func,
proph: Prophecy,
pos: Pos,

// tracks unmapped registers as a stack
free: RegList,
// list of registers currently allocated as temporaries
temps: RegList = .{},
// local -> register
map: []?Register,
// register -> local
back: [Register.COUNT]?Local,

pub fn init(ally: Allocator, func: *const Func) Error!Self {
    // make free list
    var free = RegList{};
    var r = Register.of(Register.COUNT - 1);
    const last_reg = Register.param(0);
    std.debug.assert(last_reg.n > 0);
    while (r.n >= last_reg.n) : (r.n -= 1) {
        free.appendAssumeCapacity(r);
    }

    // make map
    const nlocals = func.locals.items.len;
    const lmap = try ally.alloc(?Register, nlocals);
    std.mem.set(?Register, lmap, Register.of(Register.COUNT - 1));

    var self = Self{
        .func = func,
        .proph = try Prophecy.init(ally, func),
        .pos = Pos.ofEntry(func.ref),
        .free = free,
        .map = lmap,
        .back = [1]?Local{null} ** Register.COUNT,
    };

    // map function parameters
    var param = Local.of(0);
    while (param.index < func.takes) : (param.index += 1) {
        const reg = Register.param(@intCast(Register.Index, param.index));
        self.tie(param, reg);
    }

    // animate any value that lives at the entry point
    for (self.proph.map) |lt, i| {
        if (lt.start.eql(self.pos)) {
            self.animate(Local.of(i));
        }
    }

    return self;
}

pub fn deinit(self: *Self, ally: Allocator) void {
    self.proph.deinit(ally);
    ally.free(self.map);
}

fn tie(self: *Self, local: Local, reg: Register) void {
    self.map[local.index] = reg;
    self.back[reg.n] = local;
}

fn popFree(self: *Self) Register {
    return self.free.popOrNull() orelse {
        @panic("TODO out of animable registers; store data on stack");
    };
}

/// allocates a register for a local
fn animate(self: *Self, local: Local) void {
    self.tie(local, self.popFree());
}

/// frees the register for a local
fn murder(self: *Self, local: Local) void {
    const reg = self.get(local);
    self.map[local.index] = null;
    self.back[reg.n] = null;
    self.free.appendAssumeCapacity(reg);
}

pub fn get(self: Self, local: Local) Register {
    return self.map[local.index].?;
}

/// ensures that a register is available for a local, without losing
/// allocations for other locals
pub fn mapExact(
    self: *Self,
    ally: Allocator,
    b: *Builder,
    local: Local,
    reg: Register
) Allocator.Error!void {
    const old_reg = self.get(local);

    // ensure the register is available
    if (self.back[reg.n]) |prev| {
        // swap registers
        const swap_reg = self.popFree();
        try b.addInst(ally, Bc.mov(reg, swap_reg));

        self.murder(prev);
        self.tie(prev, swap_reg);
        self.murder(local);
        self.tie(local, reg);
    } else {
        // no need to remap, just tie this one
        self.tie(local, reg);
    }

    try b.addInst(ally, Bc.mov(old_reg, reg));
}

/// temporary must be dropped before next iteration
pub fn getTemp(self: *Self) Register {
    return self.popFree();
}

pub fn dropTemp(self: *Self, reg: Register) void {
    self.free.appendAssumeCapacity(reg);
}

/// iterate the lifetimes to the next ssa position
pub fn next(self: *Self) void {
    if (builtin.mode == .Debug) {
        // check temporaries
        if (self.temps.len > 0) {
            @panic("called RegisterMap.next() with temporaries allocated");
        }
    }

    // iterate if possible
    self.pos = self.pos.next(self.func) orelse return;

    // TODO I should probably modify prophecy to prevent this amount of repeated
    // O(n) iterations every ssa op, use a more optimal data structure now that
    // I see how I want to use it

    // murder everything (in reverse order cause it keeps registers ordered)
    var death_note = std.BoundedArray(Local, 8){};
    for (self.proph.map) |lt, i| {
        if (self.pos.order(lt.stop) == .gt and self.map[i] != null) {
            death_note.appendAssumeCapacity(Local.of(i));
        }
    }

    while (death_note.popOrNull()) |local| {
        self.murder(local);
    }

    // animate anything
    for (self.proph.map) |lt, i| {
        if (self.pos.eql(lt.start)) {
            self.animate(Local.of(i));
        }
    }
}
