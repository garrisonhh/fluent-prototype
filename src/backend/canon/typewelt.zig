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
const IdMap = com.IdMap;
const Type = @import("type.zig").Type;

const Self = @This();

// TODO replace ALL of the low level shit with com.IdMap. it already implements
// pre-allocating TypeIds, also removes the potential for very subtle bugs where
// after calling identify() a currently referenced *Type _might_ be moved

pub const TypeId = com.UId(.type);
const TypeMap = IdMap(TypeId, Type);

const ReverseMapContext = struct {
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

const ReverseMap = std.HashMapUnmanaged(
    *const Type,
    TypeId,
    ReverseMapContext,
    std.hash_map.default_max_load_percentage,
);

// maps TypeId -> Type
map: TypeMap = .{},
// maps TypeId -> Name
// names are owned by the environment
names: std.AutoHashMapUnmanaged(TypeId, Name) = .{},
// maps Type -> TypeId
reverse: ReverseMap = .{},

pub fn deinit(self: *Self, ally: Allocator) void {
    self.map.deinit(ally);
    self.names.deinit(ally);
    self.reverse.deinit(ally);
}

pub fn get(self: Self, id: TypeId) *const Type {
    return self.map.get(id);
}

pub const RenameError = error{RenamedType};

/// used in conjunction with Env's namemap
pub fn setName(
    self: *Self,
    ally: Allocator,
    id: TypeId,
    name: Name,
) (Allocator.Error || RenameError)!void {
    const res = try self.names.getOrPut(ally, id);
    if (res.found_existing) {
        return RenameError.RenamedType;
    }

    res.value_ptr.* = name;
}

pub fn getName(self: Self, id: TypeId) ?Name {
    return self.names.get(id);
}

/// creates a new type id from a type
pub fn distinct(self: *Self, ally: Allocator, ty: Type) Allocator.Error!TypeId {
    const id = try self.map.new(ally, ty);
    try self.reverse.put(ally, self.map.get(id), id);

    return id;
}

/// retrieves a previously stored id or creates a new one if this is a new type
pub fn identify(self: *Self, ally: Allocator, ty: Type) Allocator.Error!TypeId {
    return self.reverse.get(&ty) orelse self.distinct(ally, ty);
}

/// a map from zig @typeName to type id
const SelfMap = std.StringHashMapUnmanaged(TypeId);

fn identifyZig(
    self: *Self,
    ally: Allocator,
    selves: *SelfMap,
    comptime T: type,
) Allocator.Error!TypeId {
    const info = @typeInfo(T);
    const typename = @as([]const u8, @typeName(T));

    // internal self references have already been identified
    if (selves.get(typename)) |this| {
        return this;
    }

    // special case for structured data, which must be distinct and allow self
    // referencing
    switch (info) {
        inline .Struct, .Union => |_, zig_tag| {
            // for whatever reason zig doesn't recognize this is comptime
            // through the switch capture
            const zig_fields = std.meta.fields(T);

            // make distinct id and store self id
            const id = try self.map.newId(ally);
            try selves.put(ally, typename, id);

            // recurse on zig fields to make fluent fields
            const fields = try ally.alloc(Type.Field, zig_fields.len);
            inline for (zig_fields) |field, i| {
                const name = try com.Symbol.init(field.name).clone(ally);
                const ty = try self.identifyZig(ally, selves, field.field_type);

                fields[i] = Type.Field{
                    .name = name,
                    .of = ty,
                };
            }

            // fill in distinct type
            const tag: Type.Tag = switch (zig_tag) {
                .Struct => .@"struct",
                .Union => .variant,
                else => unreachable,
            };
            const final = @unionInit(Type, @tagName(tag), fields);
            try self.map.set(ally, id, final);

            return id;
        },
        else => {},
    }

    var ty: Type = switch (T) {
        void => .unit,
        bool => .bool,
        TypeId => .ty,
        comptime_int => Type{ .number = .{ .layout = .int, .bits = null } },
        comptime_float => Type{ .number = .{ .layout = .float, .bits = null } },
        else => switch (info) {
            .Opaque => .unit,
            .Int => |meta| Type{
                .number = Type.Number{
                    .layout = switch (meta.signedness) {
                        .unsigned => .uint,
                        .signed => .int,
                    },
                    .bits = meta.bits,
                },
            },
            .Float => |meta| Type{
                .number = Type.Number{
                    .layout = .float,
                    .bits = meta.bits,
                },
            },
            .Array => |meta| Type{
                .array = Type.Array{
                    .size = meta.len,
                    .of = try self.identifyZig(ally, selves, meta.child),
                },
            },
            .Pointer => |meta| Type{
                .ptr = Type.Pointer{
                    .kind = switch (meta.size) {
                        .One => .single,
                        .Many => .many,
                        .Slice => .slice,
                        .C => @compileError("fluent does not have C pointers"),
                    },
                    .to = try self.identifyZig(ally, selves, meta.child),
                },
            },
            else => @compileError(comptime std.fmt.comptimePrint(
                "cannot convert {} to fluent type",
                .{T},
            )),
        },
    };
    defer ty.deinit(ally);

    return self.identify(ally, ty);
}

/// translates zig types with clear fluent equivalents into fluent types. very
/// useful for syncing object interfaces.
pub fn identifyZigType(
    self: *Self,
    ally: Allocator,
    comptime T: type,
) Allocator.Error!TypeId {
    var selves = SelfMap{};
    defer selves.deinit(ally);

    return self.identifyZig(ally, &selves, T);
}
