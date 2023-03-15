//! manages handles for fluent reverse.

const std = @import("std");
const Allocator = std.mem.Allocator;
const Wyhash = std.hash.Wyhash;
const com = @import("common");
const kz = @import("kritzler");
const Repr = @import("repr.zig").Repr;
const Param = Repr.Param;
const Type = @import("type.zig").Type;
const TypeWelt = @import("typewelt.zig");
const TypeId = TypeWelt.TypeId;

const Self = @This();

pub const ConversionError = Allocator.Error || error{NoRepr};
pub const QualError = Repr.QualError;
pub const Error = ConversionError || Repr.AccessError;

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

const ReverseMap = std.HashMapUnmanaged(
    Repr,
    ReprId,
    ReprMapContext,
    std.hash_map.default_max_load_percentage,
);

/// stores repr and cached computations
const Slot = struct {
    repr: Repr,
    aln: ?usize,
    sz: ?usize,

    /// for ReprMap to automatically deinit repr
    pub fn deinit(self: *@This(), ally: Allocator) void {
        self.repr.deinit(ally);
    }
};

pub const ReprId = com.UId(.repr);
const ReprMap = com.IdMap(ReprId, Slot);

/// maps ReprId -> Repr
map: ReprMap = .{},
/// maps Repr -> ReprId
reverse: ReverseMap = .{},
/// maps TypeId -> ReprId
converts: std.AutoHashMapUnmanaged(TypeId, ReprId) = .{},

pub fn deinit(self: *Self, ally: Allocator) void {
    self.map.deinit(ally);
    self.reverse.deinit(ally);
    self.converts.deinit(ally);
}

pub fn get(self: Self, id: ReprId) *const Repr {
    return &self.map.get(id).repr;
}

pub fn intern(self: *Self, ally: Allocator, repr: Repr) Allocator.Error!ReprId {
    return self.internExtra(ally, Slot{
        .repr = repr,
        .sz = null,
        .aln = null,
    });
}

/// when size and alignment are already known (e.g. when creating a collection)
/// this will store any metadata along with the repr.
pub fn internExtra(
    self: *Self,
    ally: Allocator,
    slot: Slot,
) Allocator.Error!ReprId {
    if (self.reverse.get(slot.repr)) |id| {
        // repr is known
        return id;
    }

    // repr is unknown, clone and store it
    const cloned = try slot.repr.clone(ally);
    const id = try self.map.new(ally, Slot{
        .repr = cloned,
        .sz = slot.sz,
        .aln = slot.aln,
    });

    try self.reverse.put(ally, cloned, id);

    return id;
}

/// get ReprId for any type. may convert or retrieve a cached id
pub fn reprOf(
    self: *Self,
    ally: Allocator,
    tw: TypeWelt,
    ty: TypeId,
) Error!ReprId {
    if (self.converts.get(ty)) |id| {
        return id;
    }

    // convert
    const id = try self.ofType(ally, tw, ty);
    try self.converts.put(ally, ty, id);

    return id;
}

/// the necessary alignment of the Repr
pub fn alignOf(self: Self, id: ReprId) QualError!usize {
    const slot = self.map.get(id);
    if (slot.aln == null) {
        slot.aln = try slot.repr.alignOf(self);
    }

    return slot.aln.?;
}

/// the aligned size of the Repr
pub fn sizeOf(self: Self, id: ReprId) QualError!usize {
    const slot = self.map.get(id);
    if (slot.sz == null) {
        slot.sz = try slot.repr.sizeOf(self);
    }

    return slot.sz.?;
}

/// what size this Repr takes in memory, given an alignment
pub fn sizeOfAligned(self: Self, id: ReprId, aln: usize) QualError!usize {
    return com.padAlignment(try self.sizeOf(id), aln);
}

pub fn getConv(self: Self, id: ReprId) Repr.Conv {
    return self.get(id).getConv();
}

pub fn ofType(
    self: *Self,
    ally: Allocator,
    tw: TypeWelt,
    id: TypeId,
) Error!ReprId {
    // check for cached repr
    if (self.converts.get(id)) |rid| {
        return rid;
    }

    // map a new rid
    const rid = try self.map.newId(ally);
    try self.converts.put(ally, id, rid);

    const ty = tw.get(id);

    // special cases
    // some cases handle putting themselves into the idmap
    switch (ty.*) {
        .ptr => |ptr| switch (ptr.kind) {
            .slice => {
                try self.genSlice(ally, rid, try self.reprOf(ally, tw, ptr.to));
                return rid;
            },
            else => {},
        },
        inline .@"struct", .variant => |fields, tag| {
            const field_reprs = try ally.alloc(ReprId, fields.len);
            defer ally.free(field_reprs);

            for (fields) |field, i| {
                field_reprs[i] = try self.reprOf(ally, tw, field.of);
            }

            switch (comptime tag) {
                .@"struct" => try self.genStruct(ally, rid, field_reprs),
                .variant => try self.genVariant(ally, rid, field_reprs),
                else => unreachable,
            }

            return rid;
        },
        else => {},
    }

    // create repr
    const repr: Repr = switch (ty.*) {
        // no repr
        .hole,
        .any,
        .set,
        => return error.NoRepr,

        .unit => .unit,
        .@"bool" => .{ .uint = 1 },
        .ty, .builtin => .{ .uint = 8 },
        .array => |arr| .{
            .array = .{
                .size = arr.size,
                .of = try self.reprOf(ally, tw, arr.of),
            },
        },
        .number => |num| num: {
            const nbytes = if (num.bits) |bits| @divExact(bits, 8) else 8;
            break :num switch (num.layout) {
                inline else => |tag| @unionInit(
                    Repr,
                    @tagName(tag),
                    @intCast(u4, nbytes),
                ),
            };
        },
        .ptr => |ptr| switch (ptr.kind) {
            .slice => unreachable,
            // the difference between single and many pointers is purely
            // symbolic
            .single, .many => Repr{
                .ptr = try self.reprOf(ally, tw, ptr.to),
            },
        },
        .func => |func| func: {
            // NOTE this implements the base truth for the fluent callconv
            const takes = try ally.alloc(Param, func.takes.len);
            for (func.takes) |param, i| {
                takes[i] = try Param.ofType(ally, self, tw, param);
            }

            const unit = try self.intern(ally, .unit);
            const ctx = Param{
                .conv = self.get(unit).getConv(),
                .of = unit,
            };

            const returns = try Param.ofType(ally, self, tw, func.returns);

            break :func Repr{
                .func = .{
                    .ctx = ctx,
                    .takes = takes,
                    .returns = returns,
                },
            };
        },
        .@"struct", .variant => unreachable,
        else => |tag| std.debug.panic("TODO convert repr of {}", .{tag}),
    };

    try self.map.set(ally, rid, Slot{
        .repr = repr,
        .sz = null,
        .aln = null,
    });

    return rid;
}

/// helper for ofType
/// create a slice repr
fn genSlice(
    self: *Self,
    ally: Allocator,
    id: ReprId,
    of: ReprId,
) Error!void {
    const ptr = try self.intern(ally, Repr{ .ptr = of });
    const len = try self.intern(ally, Repr{ .uint = 8 });

    const fields = [_]Repr.Field{
        .{ .offset = 0, .of = ptr },
        .{ .offset = 8, .of = len },
    };

    try self.map.set(ally, id, Slot{
        .repr = Repr{ .coll = &fields },
        .sz = 16,
        .aln = 8,
    });
}

/// helper for creating collections
const FieldSlot = struct {
    index: usize,
    repr: ReprId,
    sz: usize,

    /// reverse order by size
    fn structSort(_: void, a: @This(), b: @This()) bool {
        return a.sz > b.sz;
    }
};

/// helper for ofType
/// create a struct repr. all fields will have the same index as their repr.
fn genStruct(
    self: *Self,
    ally: Allocator,
    id: ReprId,
    field_reprs: []const ReprId,
) Error!void {
    // construct field metadata and sort
    const slots = try ally.alloc(FieldSlot, field_reprs.len);
    defer ally.free(slots);

    for (field_reprs) |repr, i| {
        slots[i] = FieldSlot{
            .index = i,
            .repr = repr,
            .sz = try self.sizeOf(repr),
        };
    }

    std.sort.sort(FieldSlot, slots, {}, FieldSlot.structSort);

    // manipulate into fields, calculating struct aln and sz along the way
    const fields = try ally.alloc(Repr.Field, slots.len);

    var offset: usize = 0;
    var struct_aln: usize = 1;
    for (slots) |slot| {
        const aln = try self.alignOf(slot.repr);

        // find struct aln
        struct_aln = @max(struct_aln, aln);

        // calc field offset and find struct sz
        const field_offset = com.padAlignment(offset, aln);
        offset = field_offset + slot.sz;

        fields[slot.index] = Repr.Field{
            .offset = field_offset,
            .of = slot.repr,
        };
    }

    // intern
    const struct_sz = com.padAlignment(offset, struct_aln);
    const repr = Repr{ .coll = fields };

    try self.map.set(ally, id, Slot{
        .repr = repr,
        .sz = struct_sz,
        .aln = struct_aln,
    });
}

/// helper for ofType
/// variant == enum or tagged union
/// all variants have a u64 tag at offset zero, which will become index 0. all
/// field reprs will then be indexable at their passed index + 1
fn genVariant(
    self: *Self,
    ally: Allocator,
    id: ReprId,
    field_reprs: []const ReprId,
) Error!void {
    const fields = try ally.alloc(Repr.Field, field_reprs.len + 1);

    // tag
    const tag = try self.intern(ally, Repr{ .uint = 8 });
    fields[0] = Repr.Field{
        .offset = 0,
        .of = tag,
    };

    // find max aln, which will be the offset of all union fields
    var max_aln: usize = try self.alignOf(tag);
    for (field_reprs) |repr| {
        max_aln = @max(max_aln, try self.alignOf(repr));
    }

    // get fields and find max field size
    var fields_sz: usize = 0;
    for (field_reprs) |repr, i| {
        fields_sz = @max(fields_sz, try self.sizeOf(repr));
        fields[i + 1] = Repr.Field{
            .offset = max_aln,
            .of = repr,
        };
    }

    // intern
    const variant_sz = com.padAlignment(max_aln + fields_sz, max_aln);
    const repr = Repr{ .coll = fields };

    try self.map.set(ally, id, Slot{
        .repr = repr,
        .sz = variant_sz,
        .aln = max_aln,
    });
}
