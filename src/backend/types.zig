//! provides fluent type operations using handles to minimize the amount
//! of in-memory objects being used.

const std = @import("std");
const util = @import("util");

const Allocator = std.mem.Allocator;
const Wyhash = std.hash.Wyhash;

pub const TypeId = packed struct { index: usize };

/// where types are stored.
pub const TypeSet = struct {
    const Self = @This();

    const TypeMapContext = struct {
        pub fn hash(ctx: @This(), key: Type) u64 {
            _ = ctx;
            var wyhash = Wyhash.init(0);
            key.hash(&wyhash);

            return wyhash.final();
        }

        pub fn eql(ctx: @This(), a: Type, b: Type) bool {
            _ = ctx;
            return a.eql(b);
        }
    };
    const load = std.hash_map.default_max_load_percentage;
    const TypeMap = std.HashMapUnmanaged(Type, TypeId, TypeMapContext, load);

    types: std.ArrayListUnmanaged(Type) = .{},
    map: TypeMap = .{},

    pub fn deinit(self: *Self, ally: Allocator) void {
        self.types.deinit(ally);
        self.map.deinit(ally);
    }

    pub fn get(self: Self, id: TypeId) void {
        return &self.types.items[id.index];
    }

    pub fn unify(
        self: *Self,
        ally: Allocator,
        ty: Type
    ) Allocator.Error!TypeId {
        const res = try self.map.getOrPut(ally, ty);
        if (!res.found_existing) {
            const id = TypeId{ .index = self.types.items.len };
            var slot = try self.types.addOne(ally);

            slot.* = try ty.clone(ally);

            // TODO key ownership???

            res.value_ptr.* = id;
        }

        return res.value_ptr.*;
    }
};

pub const Type = union(enum) {
    const Self = @This();

    pub const Tag = std.meta.Tag(Self);

    pub const Number = struct {
        layout: util.Number.Layout,
        bits: u8,
    };

    unit,
    symbol: util.Symbol,
    number: Number,

    pub fn hash(self: Self, wyhash: *Wyhash) void {
        const asBytes = std.mem.asBytes;
        wyhash.update(asBytes(&std.meta.activeTag(self)));

        switch (self) {
            .unit => {},
            .symbol => |sym| wyhash.update(asBytes(&sym.hash)),
            .number => |num| {
                wyhash.update(asBytes(&num.layout));
                wyhash.update(asBytes(&num.bits));
            },
        }
    }

    pub fn eql(self: Self, ty: Self) bool {
        if (@as(Tag, self) != @as(Tag, ty)) return false;

        return switch (self) {
            .unit => true,
            .symbol => |sym| sym.eql(ty.symbol),
            .number => |num|
                num.layout == ty.number.layout and num.bits == ty.number.bits,
        };
    }

    pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
        return switch (self) {
            .unit, .number => self,
            .symbol => |sym| Self{ .symbol = try sym.clone(ally) },
        } ;
    }
};

pub const Pattern = union(enum) {
    const Self = @This();

    const Set = std.AutoHashMapUnmanaged(TypeId, void);

    any,
    set: Set,
    ty: TypeId,
};