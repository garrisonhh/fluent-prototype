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

/// stores repr and cached computations
const Slot = struct {
    repr: Repr,
    aln: ?usize = null,
    sz: ?usize = null,
};

/// maps ReprId -> Repr
map: std.MultiArrayList(Slot) = .{},
/// maps Repr -> ReprId
reprs: ReprMap = .{},
/// maps TypeId -> ReprId
converts: std.AutoHashMapUnmanaged(TypeId, ReprId) = .{},

pub fn deinit(self: *Self, ally: Allocator) void {
    for (self.map.items(.repr)) |repr| repr.deinit(ally);
    self.map.deinit(ally);
    self.reprs.deinit(ally);
    self.converts.deinit(ally);
}

/// invalidated on next intern() call
pub fn get(self: Self, id: ReprId) *const Repr {
    return &self.map.get(id.index).repr;
}

pub fn intern(self: *Self, ally: Allocator, repr: Repr) Allocator.Error!ReprId {
    const res = try self.reprs.getOrPut(ally, repr);
    if (!res.found_existing) {
        const id = ReprId{ .index = self.map.len };
        try self.map.append(ally, Slot{ .repr = repr });

        res.value_ptr.* = id;
        res.key_ptr.* = repr;
    }

    return res.value_ptr.*;
}

/// get ReprId for any type. may convert or retrieve a cached id
pub fn reprOf(
    self: *Self,
    ally: Allocator,
    tw: TypeWelt,
    ty: TypeId,
) Repr.ConversionError!ReprId {
    if (self.converts.get(ty)) |id| {
        return id;
    }

    // convert
    const repr = try Repr.ofType(ally, self, tw, ty);
    const id = try self.intern(ally, repr);
    try self.converts.put(ally, ty, id);

    return id;
}

pub fn alignOf(self: Self, id: ReprId) Repr.QualError!usize {
    const aln = &self.map.items(.aln)[id.index];
    if (aln.* == null) {
        aln.* = try self.get(id).alignOf(self);
    }

    return aln.*.?;
}

pub fn sizeOf(self: Self, id: ReprId) Repr.QualError!usize {
    const sz = &self.map.items(.sz)[id.index];
    if (sz.* == null) {
        sz.* = try self.get(id).sizeOf(self);
    }

    return sz.*.?;
}

pub fn sizeOfAligned(self: Self, id: ReprId, aln: usize) Repr.QualError!usize {
    return try self.get(id).sizeOfAligned(self, aln);
}

pub fn getConv(self: Self, id: ReprId) Repr.Conv {
    return self.get(id).getConv();
}
