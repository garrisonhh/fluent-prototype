//! during compilation, every live SSA temporary needs to be mapped to register
//! storage in the bytecode vm. this data structure manages those mappings.
//!
//! RegisterMap expects to be updated as you iteratively move through the
//! ssa function. this allows

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const ssa = @import("../ssa/ssa.zig");
const Local = ssa.Local;
const Func = ssa.Func;
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
    const last_reg = Register.param(@intCast(Register.Index, func.takes));
    while (r.n >= last_reg.n) : (r.n -= 1) {
        free.appendAssumeCapacity(r);
    }

    // make map
    const nlocals = func.locals.items.len;
    const lmap = try ally.alloc(?Register, nlocals);
    std.mem.set(?Register, lmap, null);

    var self = Self{
        .func = func,
        .proph = try Prophecy.init(ally, func),
        .pos = Pos.ofEntry(func.ref),
        .free = free,
        .map = lmap,
        .back = [1]?Local{null} ** Register.COUNT,
    };

    // map function parameters directly to callconv registers
    var param = Local.of(0);
    while (param.index < func.takes) : (param.index += 1) {
        const reg = Register.param(@intCast(Register.Index, param.index));
        self.tie(param, reg);
    }

    // animate any non-parameter value that lives at the entry point
    var local = Local.of(func.takes);
    while (local.index < self.proph.map.len) : (local.index += 1) {
        const lt = self.proph.get(local);
        if (!lt.start.eql(self.pos)) {
            break;
        }

        self.animate(local);
    }

    return self;
}

pub fn deinit(self: *Self, ally: Allocator) void {
    self.proph.deinit(ally);
    ally.free(self.map);
}

fn tie(self: *Self, local: Local, reg: Register) void {
    std.debug.assert(self.map[local.index] == null);
    std.debug.assert(self.back[reg.n] == null);
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
    if (builtin.mode == .Debug) {
        if (self.map[local.index] == null) {
            std.debug.print(
                "at pos {} tried to get dead {}\n",
                .{ self.pos, local },
            );
        }
    }

    return self.map[local.index].?;
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

pub const Entry = struct {
    local: Local,
    reg: Register,
};

pub const Iterator = struct {
    map: []const ?Register,
    index: usize = 0,

    pub fn next(self: *Iterator) ?Entry {
        while (self.index < self.map.len) {
            defer self.index += 1;

            if (self.map[self.index]) |reg| {
                return Entry{
                    .local = Local.of(self.index),
                    .reg = reg,
                };
            }
        }

        return null;
    }
};

/// iterate through live locals
pub fn iterator(self: Self) Iterator {
    return Iterator{ .map = self.map };
}
