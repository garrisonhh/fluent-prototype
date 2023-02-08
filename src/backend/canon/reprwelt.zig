//! manages handles for fluent reprs.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Wyhash = std.hash.Wyhash;
const kz = @import("kritzler");
const Repr = @import("repr.zig").Repr;
const Type = @import("type.zig").Type;
const TypeWelt = @import("typewelt.zig");
const TypeId = TypeWelt.TypeId;

const Self = @This();

pub const ReprId = packed struct {
    index: usize,

    pub fn eql(self: @This(), id: @This()) bool {
        return self.index == id.index;
    }

    pub fn render(self: @This(), ctx: *kz.Context, rw: Self) !kz.Ref {
        return try rw.get(self).render(ctx, rw);
    }
};

const ReprMapContext = struct {
    const K = Repr;

    pub fn hash(_: @This(), key: K) u64 {
        var wyhash = Wyhash.init(0);
        key.hash(&wyhash);

        return wyhash.final();
    }

    pub fn eql(_: @This(), a: K, b: K) bool {
        return a.eql(b);
    }
};

const ReprMap = std.HashMapUnmanaged(
    Repr,
    ReprId,
    ReprMapContext,
    std.hash_map.default_max_load_percentage,
);

/// maps ReprId -> Repr
map: std.ArrayListUnmanaged(Repr) = .{},
/// maps Repr -> ReprId
reprs: ReprMap = .{},
/// maps TypeId -> ReprId
converts: std.AutoHashMapUnmanaged(TypeId, ReprId) = .{},

pub fn deinit(self: *Self, ally: Allocator) void {
    for (self.map.items) |repr| repr.deinit(ally);
    self.map.deinit(ally);
    self.reprs.deinit(ally);
    self.converts.deinit(ally);
}

pub fn get(self: Self, id: ReprId) *const Repr {
    return &self.map.items[id.index];
}

pub const ConvertError =
    Allocator.Error || error{ AnalysisType, UnknownConversion };

pub fn intern(self: *Self, ally: Allocator, repr: Repr) Allocator.Error!ReprId {
    const id = ReprId{ .index = self.map.items.len };
    try self.map.append(ally, repr);
    try self.reprs.put(ally, repr, id);

    return id;
}

/// get ReprId for any type. may convert or retrieve a cached id
pub fn reprOf(
    self: *Self,
    ally: Allocator,
    tw: TypeWelt,
    ty: TypeId,
) ConvertError!ReprId {
    if (self.converts.get(ty)) |id| {
        return id;
    }

    // convert
    const repr = try Repr.ofType(ally, self, tw, ty);
    const id = try self.intern(ally, repr);
    try self.converts.put(ally, ty, id);

    return id;
}

pub fn sizeOf(self: Self, id: ReprId) usize {
    return self.get(id).sizeOf(self);
}

pub fn alignOf(self: Self, id: ReprId) usize {
    return self.get(id).alignOf(self);
}
