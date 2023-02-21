//! manages handles for fluent reprs.

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

pub const ReprId = packed struct(u64) {
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
    aln: ?usize,
    sz: ?usize,
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
    return self.internExtra(ally, repr, null, null);
}

/// when size and alignment are already known (e.g. when creating a collection)
/// this will store any metadata along with the repr.
pub fn internExtra(
    self: *Self,
    ally: Allocator,
    repr: Repr,
    sz: ?usize,
    aln: ?usize,
) Allocator.Error!ReprId {
    if (self.reprs.get(repr)) |id| {
        // repr is known
        return id;
    } else {
        // repr is unknown, clone and store it
        const id = ReprId{ .index = self.map.len };
        const cloned = try repr.clone(ally);

        try self.map.append(ally, Slot{
            .repr = cloned,
            .sz = sz,
            .aln = aln,
        });

        try self.reprs.put(ally, cloned, id);

        return id;
    }
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
    const aln = &self.map.items(.aln)[id.index];
    if (aln.* == null) {
        aln.* = try self.get(id).alignOf(self);
    }

    return aln.*.?;
}

/// the aligned size of the Repr
pub fn sizeOf(self: Self, id: ReprId) QualError!usize {
    const sz = &self.map.items(.sz)[id.index];
    if (sz.* == null) {
        sz.* = try self.get(id).sizeOf(self);
    }

    return sz.*.?;
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
    const ty = tw.get(id);
    return switch (ty.*) {
        // no repr
        .hole,
        .any,
        .set,
        => return error.NoRepr,

        .unit => try self.intern(ally, .unit),
        .@"bool" => try self.intern(ally, .{ .uint = 1 }),
        .ty, .builtin => try self.intern(ally, .{ .uint = 8 }),
        .array => |arr| try self.intern(ally, .{
            .array = .{
                .size = arr.size,
                .of = try self.reprOf(ally, tw, arr.of),
            },
        }),
        .number => |num| num: {
            const nbytes = if (num.bits) |bits| @divExact(bits, 8) else 8;
            const repr = switch (num.layout) {
                inline else => |tag| @unionInit(
                    Repr,
                    @tagName(tag),
                    @intCast(u4, nbytes),
                ),
            };

            break :num try self.intern(ally, repr);
        },
        .ptr => |ptr| switch (ptr.kind) {
            .slice => sl: {
                const of = try self.reprOf(ally, tw, ptr.to);
                break :sl try self.makeSlice(ally, of);
            },
            // the difference between single and many pointers is purely
            // symbolic
            .single, .many => try self.intern(ally, Repr{
                .ptr = try self.reprOf(ally, tw, ptr.to),
            }),
        },
        inline .@"struct", .variant => |fields, tag| st: {
            const field_reprs = try ally.alloc(ReprId, fields.len);
            defer ally.free(field_reprs);

            for (fields) |field, i| {
                field_reprs[i] = try self.reprOf(ally, tw, field.of);
            }

            break :st switch (comptime tag) {
                .@"struct" => try self.makeStruct(ally, field_reprs),
                .variant => try self.makeVariant(ally, field_reprs),
                else => unreachable,
            };
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

            break :func try self.intern(ally, Repr{
                .func = .{
                    .ctx = ctx,
                    .takes = takes,
                    .returns = returns,
                },
            });
        },
        else => |tag| std.debug.panic("TODO convert repr of {}", .{tag}),
    };
}

/// dupes parameters
pub fn makeFunc(
    self: *Self,
    ally: Allocator,
    ctx: Param,
    takes: []const Param,
    returns: Param,
) Allocator.Error!Self {
    return try self.intern(ally, Repr{
        .func = Repr.Func{
            .ctx = ctx,
            .takes = takes,
            .returns = returns,
        },
    });
}

/// create a slice repr
pub fn makeSlice(
    self: *Self,
    ally: Allocator,
    of: ReprId,
) Error!ReprId {
    const ptr = try self.intern(ally, Repr{ .ptr = of });
    const len = try self.intern(ally, Repr{ .uint = 8 });

    const fields = [_]Repr.Field{
        .{ .offset = 0, .of = ptr },
        .{ .offset = 8, .of = len },
    };

    return try self.intern(ally, Repr{ .coll = &fields });
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

/// create a struct repr. all fields will have the same index as their repr.
pub fn makeStruct(
    self: *Self,
    ally: Allocator,
    field_reprs: []const ReprId,
) Error!ReprId {
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
    defer ally.free(fields);

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

    return try self.internExtra(ally, repr, struct_sz, struct_aln);
}

/// variant == enum or tagged union
/// all variants have a u64 tag at offset zero, which will become index 0. all
/// field reprs will then be indexable at their passed index + 1
pub fn makeVariant(
    self: *Self,
    ally: Allocator,
    field_reprs: []const ReprId,
) Error!ReprId {
    const fields = try ally.alloc(Repr.Field, field_reprs.len + 1);
    defer ally.free(fields);

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

    return try self.internExtra(ally, repr, variant_sz, max_aln);
}
