//! storage for types and the context for type handles.
//!
//! using a Type, you can call `identify()` which gives you the unique id of
//! this type. TypeIds can be 'dereferenced' with `get`.
//!
//! the actual unique Type objects are just stored in the internal allocator,
//! but hashing and storing an arraylist of pointers allows the handle system
//! to function, and the memory/speed tradeoff is worth it given the vast amount
//! of these objects I'm creating and messing around with.
//!
//! TODO I should cache all of the prelude types somehow so I can make fewer
//! calls to identify!

const std = @import("std");
const Allocator = std.mem.Allocator;
const Wyhash = std.hash.Wyhash;
const kz = @import("kritzler");
const com = @import("common");
const Name = com.Name;
const Type = @import("type.zig").Type;

const Self = @This();

pub const TypeId = packed struct {
    index: usize,

    pub fn eql(self: @This(), id: @This()) bool {
        return self.index == id.index;
    }

    pub fn render(self: @This(), ctx: *kz.Context, tw: Self) !kz.Ref {
        return try tw.get(self).render(ctx, tw);
    }

    pub fn toString(
        self: @This(),
        ally: Allocator,
        tw: Self,
    ) Allocator.Error![]u8 {
        return try tw.get(self).toString(ally, tw);
    }
};

const TypeMapContext = struct {
    const K = *const Type;

    pub fn hash(_: @This(), key: K) u64 {
        var wyhash = Wyhash.init(0);
        key.hash(&wyhash);

        return wyhash.final();
    }

    pub fn eql(_: @This(), a: K, b: K) bool {
        return a.eql(b.*);
    }
};

const TypeMap = std.HashMapUnmanaged(
    *const Type,
    TypeId,
    TypeMapContext,
    std.hash_map.default_max_load_percentage,
);

// maps TypeId -> Type
// types are individually allocated to allow the typewelt to reuse the data
// as the key to the other map
types: std.ArrayListUnmanaged(*Type) = .{},
// maps Type -> TypeId
map: TypeMap = .{},
// maps TypeId -> Name
// names are owned by the environment
names: std.ArrayListUnmanaged(?Name) = .{},

pub fn deinit(self: *Self, ally: Allocator) void {
    for (self.types.items) |ptr| {
        ptr.deinit(ally);
        ally.destroy(ptr);
    }
    self.types.deinit(ally);
    self.names.deinit(ally);
    self.map.deinit(ally);
}

pub fn get(self: Self, id: TypeId) *const Type {
    return self.types.items[id.index];
}

pub const RenameError = error{RenamedType};

/// used in conjunction with Env's namemap
pub fn setName(
    self: *Self,
    ally: Allocator,
    id: TypeId,
    name: Name,
) (Allocator.Error || RenameError)!void {
    // expand names array
    if (self.names.items.len <= id.index) {
        const diff = 1 + id.index - self.names.items.len;
        try self.names.appendNTimes(ally, null, diff);
    }

    if (self.names.items[id.index] != null) {
        return error.RenamedType;
    } else {
        self.names.items[id.index] = name;
    }
}

pub fn getName(self: Self, id: TypeId) ?Name {
    if (id.index >= self.names.items.len) return null;
    return self.names.items[id.index];
}

/// retrieves an established ID or creates a new one.
pub fn identify(
    self: *Self,
    ally: Allocator,
    ty: Type,
) Allocator.Error!TypeId {
    const res = try self.map.getOrPut(ally, &ty);
    if (!res.found_existing) {
        // find id
        const id = TypeId{ .index = self.types.items.len };

        // allocate for type and clone
        const cloned = try com.placeOn(ally, try ty.clone(ally));

        // store in internal data structures
        res.key_ptr.* = cloned;
        res.value_ptr.* = id;

        try self.types.append(ally, cloned);
    }

    return res.value_ptr.*;
}
