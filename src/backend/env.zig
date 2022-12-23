//! env manages the state of fluent's dynamic backend runtime.
//!
//! fluent's backend runtime consists of two components:
//! - the TypeWelt
//!   - this is the context for managing fluent types. there are two canonical
//!     representations a fluent type can take, the Type and the TypeId.
//!     - Type is a tagged union as you would expect, a structured type repr
//!     - TypeId is a handle which acts like a `*const Type` when paired with
//!       the TypeWelt
//!   - TypeWelt is the entire set of Types that have and will ever exist in a
//!     fluent program
//!     - it also stores some metadata, like typenames
//! - the NameMap
//!   - this is an associative map including all of the names that have and will
//!     ever exist in a fluent program

const std = @import("std");
const Allocator = std.mem.Allocator;
const util = @import("util");
const Symbol = util.Symbol;
const Name = util.Name;
const NameMap = util.NameMap;
const kz = @import("kritzler");
const types = @import("types.zig");
const TypeWelt = types.TypeWelt;
const TypeId = types.TypeId;
const Type = types.Type;
const TExpr = @import("texpr.zig");

const Self = @This();

pub const root = Name.root;

const Binding = struct {
    ty: TypeId,
    value: TExpr,
};

const Bindings = NameMap(Binding);

ally: Allocator,
tw: TypeWelt = .{},
// env owns everything in every binding
nmap: Bindings = .{},

pub fn init(ally: Allocator) Allocator.Error!Self {
    return Self{
        .ally = ally,
    };
}

pub fn deinit(self: *Self) void {
    self.tw.deinit(self.ally);
    self.nmap.deinit(self.ally);
}

pub fn identify(self: *Self, ty: Type) Allocator.Error!TypeId {
    return self.tw.identify(self.ally, ty);
}

// accessors ===================================================================

pub fn getValue(self: *Self, name: Name) TExpr {
    return self.nmap.get(name).value;
}

pub fn getType(self: *Self, name: Name) TypeId {
    return self.nmap.get(name).ty;
}

// definitions =================================================================

pub const DefError = Allocator.Error || Bindings.PutError;

pub fn def(
    self: *Self,
    scope: Name,
    sym: Symbol,
    ty: TypeId,
    value: TExpr
) DefError!Name {
    const binding = Binding{
        .ty = ty,
        .value = try value.clone(self.ally),
    };

    return self.nmap.put(self.ally, scope, sym, binding);
}

pub fn defNamespace(self: *Self, scope: Name, sym: Symbol) DefError!Name {
    const nsty = try self.identify(Type{ .namespace = {} });
    return self.def(scope, sym, nsty, TExpr{ .ty = .namespace });
}

pub fn defType(
    self: *Self,
    scope: Name,
    sym: Symbol,
    value: TypeId
) DefError!Name {
    const tyty = try self.identify(Type{ .ty = {} });
    const name = self.def(scope, sym, tyty, TExpr{ .ty = value });
    try self.tw.setName(self.ally, value, name);

    return name;
}