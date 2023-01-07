//! during compilation, every live SSA temporary needs to be mapped to register
//! storage in the bytecode vm. this data structure manages those mappings.
//!
//! RegisterMap expects to be updated as you iteratively move through the
//! ssa function. this allows

const std = @import("std");
const Allocator = std.mem.Allocator;
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

const FreeList = std.BoundedArray(Register, Register.COUNT);

func: *const Func,
proph: Prophecy,
pos: Pos,

// tracks unmapped registers as a stack
free: FreeList,
// local -> register
map: []?Register,
// register -> local
back: [Register.COUNT]?Local,

pub fn init(ally: Allocator, func: *const Func) Error!Self {
    // make free list
    var free = FreeList{};
    var r = Register.of(Register.COUNT - 1);
    const last_reg = Register.param(@intCast(Register.Index, func.takes));
    std.debug.assert(last_reg.n > 0);
    while (r.n >= last_reg.n) : (r.n -= 1) {
        free.appendAssumeCapacity(r);
    }

    // make map
    const lmap = try ally.alloc(?Register, func.locals.items.len);
    std.mem.set(?Register, lmap, null);

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

/// allocates a register for a local
fn animate(self: *Self, local: Local) Error!void {
    std.debug.assert(!self.isAlive(local));
    if (self.free.popOrNull()) |reg| {
        self.tie(local, reg);
    } else {
        @panic("TODO out of animatable registers; store data on stack");
    }
}

/// frees the register for a local
fn murder(self: *Self, local: Local) void {
    const reg = self.get(local);
    self.map[local.index] = null;
    self.back[reg.n] = null;
    self.free.appendAssumeCapacity(reg);
}

pub fn get(self: Self, local: Local) Register {
    std.debug.assert(self.isAlive(local));
    return self.map[local.index].?;
}

fn isAlive(self: Self, local: Local) bool {
    return self.map[local.index] != null;
}

/// iterate the lifetimes to the next ssa position
pub fn next(self: *Self) Error!void {
    self.pos = self.pos.next(self.func) orelse {
        // if next is called at the end of the function, do nothing
        return;
    };

    for (self.proph.map) |lt, i| {
        const local = Local.of(i);
        if (self.isAlive(local)) {
            // check for death
            if (self.pos.order(lt.stop) == .gt) {
                self.murder(local);
            }
        } else {
            // check for life
            // TODO the prophecy needs to understand function call convention
            // for parameters and return values
            if (self.pos.eql(lt.start)) {
                try self.animate(local);
            }
        }
    }
}
