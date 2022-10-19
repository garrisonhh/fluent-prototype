//! env is the fluent namespace abstraction.

const std = @import("std");
const util = @import("util");
const types = @import("types.zig");

const Allocator = std.mem.Allocator;
const Symbol = util.Symbol;
const TypeWelt = types.TypeWelt;
const TypeId = types.TypeId;
const Type = types.Type;

const Self = @This();

pub const DefError =
    Allocator.Error
 || error { SymbolRedef };

pub const Bound = union(enum) {
    unimpl,
    ty: TypeId,
};

const Binding = struct {
    ty: TypeId,
    value: Bound
};

ally: Allocator,
typewelt: TypeWelt,
ns: Symbol.HashMapUnmanaged(Binding) = .{},

pub fn init(ally: Allocator) Self {
    return Self{
        .ally = ally,
        .typewelt = TypeWelt.init(ally),
    };
}

pub fn deinit(self: *Self) void {
    self.typewelt.deinit();
    self.ns.deinit(self.ally);
}

pub fn def(self: *Self, sym: Symbol, to: Binding) DefError!void {
    const res = try self.ns.getOrPut(self.ally, sym);
    if (res.found_existing) {
        return error.SymbolRedef;
    } else {
        res.value_ptr.* = to;
    }
}

pub fn contains(self: Self, sym: Symbol) bool {
    return self.ns.contains(sym);
}

pub fn getType(self: Self, sym: Symbol) ?TypeId {
    return if (self.ns.get(sym)) |binding| binding.ty else null;
}

pub fn getValue(self: Self, sym: Symbol) ?*const Bound {
    return if (self.ns.getPtr(sym)) |binding| &binding.value else null;
}

// type helpers ================================================================

pub fn typeIdentify(self: *Self, ty: Type) Allocator.Error!TypeId {
    return try self.typewelt.identify(ty);
}

pub fn typeUnify(
    self: *Self,
    outward: TypeId,
    inward: TypeId
) Allocator.Error!?TypeId {
    return try types.unify(&self.typewelt, outward, inward);
}

pub fn typeGet(self: Self, ty: TypeId) *const Type {
    return self.typewelt.get(ty);
}

pub fn typeDef(self: *Self, sym: Symbol, ty: Type) DefError!TypeId {
    const id = try self.typeIdentify(ty);
    try self.def(sym, Binding{
        .ty = try self.typeIdentify(Type{ .ty = {} }),
        .value = .{ .ty = id }
    });
    return id;
}
