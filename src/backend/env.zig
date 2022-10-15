//! env is the fluent namespace abstraction.

const std = @import("std");
const util = @import("util");
const types = @import("types.zig");

const Allocator = std.mem.Allocator;
const Symbol = util.Symbol;
const TypeSet = types.TypeSet;
const TypeId = types.TypeId;
const Type = types.Type;

const Self = @This();

pub const DefError = error { SymbolRedef };

const Bind = union(enum) {
    ty: TypeId,
};

ally: Allocator,
typeset: TypeSet = .{},
ns: Symbol.HashMapUnmanaged(Bind) = .{},

pub fn init(ally: Allocator) Self {
    return Self{ .ally = ally };
}

pub fn deinit(self: *Self) void {
    self.typeset.deinit(self.ally);
    self.ns.deinit(self.ally);
}

pub fn def(
    self: *Self,
    sym: Symbol,
    to: Bind
) (DefError || Allocator.Error)!void {
    const res = try self.ns.getOrPut(self.ally, sym);
    if (res.found_existing) {
        return error.SymbolRedef;
    } else {
        res.value_ptr.* = to;
    }
}

pub fn typeUnify(self: *Self, ty: Type) Allocator.Error!TypeId {
    return try self.typeset.unify(self.ally, ty);
}

pub fn typeDef(
    self: *Self,
    sym: Symbol,
    ty: Type
) (DefError || Allocator.Error)!void {
    return try self.def(sym, Bind{ .ty = try self.typeUnify(ty) });
}

pub fn initGlobal(ally: Allocator) Allocator.Error!Self {
    const sym = Symbol.init;

    var global = Self.init(ally);

    // TODO errors here

    global.typeDef(sym("unit"), Type{ .unit = {} }) catch {};
    global.typeDef(sym("i64"), Type{
        .number = .{ .layout = .int, .bits = 64 }
    }) catch {};

    return global;
}