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

pub const TypeId = packed struct(u64) {
    index: usize,

    pub fn eql(self: @This(), id: @This()) bool {
        return self.index == id.index;
    }

    pub const render = @import("render_type.zig").renderTypeId;

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
        // allocate for type and clone
        const cloned = try com.placeOn(ally, try ty.clone(ally));

        // store in internal data structures
        res.key_ptr.* = cloned;
        res.value_ptr.* = TypeId{ .index = self.types.items.len };

        try self.types.append(ally, cloned);
    }

    return res.value_ptr.*;
}

/// very useful for syncing object interfaces
pub fn convertZigType(
    self: *Self,
    ally: Allocator,
    comptime T: type,
) Allocator.Error!TypeId {
    var ty: Type = switch (T) {
        void => .unit,
        bool => .bool,
        TypeId => .ty,
        comptime_int => Type{ .number = .{ .layout = .int, .bits = null } },
        comptime_float => Type{ .number = .{ .layout = .float, .bits = null } },
        else => switch (@typeInfo(T)) {
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
            .Struct => |meta| st: {
                const fields = try ally.alloc(Type.Field, meta.fields.len);
                inline for (meta.fields) |field, i| {
                    fields[i] = Type.Field{
                        .name = com.Symbol.init(try ally.dupe(u8, field.name)),
                        .of = try self.convertZigType(ally, field.field_type),
                    };
                }

                break :st Type{ .@"struct" = fields };
            },
            .Union => |meta| u: {
                const fields = try ally.alloc(Type.Field, meta.fields.len);
                inline for (meta.fields) |field, i| {
                    std.debug.assert(!std.mem.eql(u8, field.name, "tag"));

                    fields[i] = Type.Field{
                        .name = com.Symbol.init(try ally.dupe(u8, field.name)),
                        .of = try self.convertZigType(ally, field.field_type),
                    };
                }

                break :u Type{ .variant = fields };
            },
            else => @compileError(
                std.fmt.comptimePrint("cannot convert {}", .{T}),
            ),
        },
    };
    defer ty.deinit(ally);

    return self.identify(ally, ty);
}
