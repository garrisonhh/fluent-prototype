//! env is the fluent namespace abstraction.
//!
//! envs own and manage everything inside their namespace.

const std = @import("std");
const util = @import("util");
const kz = @import("kritzler");
const types = @import("types.zig");

const Allocator = std.mem.Allocator;
const Symbol = util.Symbol;
const TypeWelt = types.TypeWelt;
const TypeId = types.TypeId;
const Type = types.Type;
const GenericId = Type.GenericId;

const Self = @This();

pub const DefError =
    Allocator.Error
 || error {
    SymbolRedef,
    MisplacedGenericId,
};

pub const BuiltinOp = enum {
    add,
    sub,
    mul,
    div,
    @"and",
    @"or",
    not,
};

pub const BuiltinFlow = enum {
    @"if",
};

pub const Bound = union(enum) {
    builtin_op: BuiltinOp,
    builtin_flow: BuiltinFlow,
    ty: TypeId,
};

const Binding = struct {
    ty: TypeId,
    value: Bound
};

ally: Allocator,
parent: ?*const Self,
typewelt: *TypeWelt,
// this is owned by the base env
typenames: *std.AutoHashMapUnmanaged(TypeId, Symbol),

// keys and values are all owned by this env
ns: Symbol.HashMapUnmanaged(Binding) = .{},
// also local to this env
first_generic: Type.GenericId,
generics: std.ArrayListUnmanaged(TypeId) = .{},

pub fn initBase(ally: Allocator, typewelt: *TypeWelt) Allocator.Error!Self {
    const typenames = std.AutoHashMapUnmanaged(TypeId, Symbol){};

    return Self{
        .ally = ally,
        .typewelt = typewelt,
        .typenames = try util.placeOn(ally, typenames),
        .parent = null,
        .first_generic = GenericId{ .index = 0 },
    };
}

pub fn init(parent: *const Self) Self {
    return Self{
        .ally = parent.ally,
        .typewelt = parent.typewelt,
        .typenames = parent.typenames,
        .parent = parent,
        .first_generic = parent.nextGenericId(),
    };
}

pub fn deinit(self: *Self) void {
    var keys = self.ns.keyIterator();
    while (keys.next()) |key| self.ally.free(key.str);
    self.ns.deinit(self.ally);

    if (self.parent == null) {
        var names = self.typenames.valueIterator();
        while (names.next()) |name| self.ally.free(name.str);

        self.typenames.deinit(self.ally);
        self.ally.destroy(self.typenames);
    }
}

pub fn def(self: *Self, sym: Symbol, ty: TypeId, value: Bound) DefError!void {
    // place symbol
    const res = try self.ns.getOrPut(self.ally, sym);
    if (res.found_existing) return error.SymbolRedef;

    res.key_ptr.* = try sym.clone(self.ally);
    res.value_ptr.* = Binding{ .ty = ty, .value = value };

    // special type behavior: store any generics or typenames
    if (value == .ty) {
        const got = self.typewelt.get(value.ty);

        if (got.* == .generic) {
            const index = got.generic.index;
            if (index >= self.generics.items.len) {
                return error.MisplacedGenericId;
            }
        }

        try self.typenames.put(self.ally, value.ty, try sym.clone(self.ally));
    }
}

pub fn contains(self: Self, sym: Symbol) bool {
    if (self.ns.contains(sym)) {
        return true;
    } else if (self.parent) |parent| {
        return parent.contains(sym);
    } else {
        return false;
    }
}

fn get(self: Self, sym: Symbol) ?*Binding {
    if (self.ns.getPtr(sym)) |binding| {
        return binding;
    } else if (self.parent) |parent| {
        return parent.get(sym);
    } else {
        return null;
    }
}

pub fn getType(self: Self, sym: Symbol) ?TypeId {
    return if (self.get(sym)) |binding| binding.ty else null;
}

pub fn getTypename(self: Self, ty: TypeId) ?Symbol {
    return if (self.typenames.get(ty)) |sym| sym else null;
}

pub fn getBound(self: Self, sym: Symbol) ?*const Bound {
    return if (self.get(sym)) |binding| &binding.value else null;
}

// type-specific functionality =================================================

fn getGeneric(self: Self, gid: GenericId) TypeId {
    if (gid.index < self.first_generic.index) {
        return self.parent.?.getGeneric(gid);
    }

    return self.generics.items[gid.index - self.first_generic.index];
}

fn nextGenericId(self: Self) GenericId {
    const next_index = self.first_generic.index + self.generics.items.len;
    return GenericId{ .index = next_index };
}

pub fn defGeneric(self: *Self, ty: TypeId) Allocator.Error!Type {
    const gid = self.nextGenericId();
    try self.generics.append(self.ally, ty);

    return Type{ .generic = gid };
}

pub fn typeIdentify(self: Self, ty: Type) Allocator.Error!TypeId {
    return try self.typewelt.identify(ty);
}

pub fn typeIdentifyNumber(
    self: Self,
    layout: util.Number.Layout,
    bits: ?u8
) Allocator.Error!TypeId {
    const ty = Type{ .number = .{ .layout = layout, .bits = bits } };
    return try self.typeIdentify(ty);
}

pub fn typeGet(self: Self, ty: TypeId) *const Type {
    return self.typewelt.get(ty);
}

pub fn typeDef(self: *Self, sym: Symbol, ty: Type) DefError!TypeId {
    const id = try self.typeIdentify(ty);
    try self.def(sym, try self.typeIdentify(Type{ .ty = {} }), .{ .ty = id });

    return id;
}

// display =====================================================================

pub fn dump(self: Self, ally: Allocator, writer: anytype) !void {
    var ctx = kz.Context.init(ally);
    defer ctx.deinit();

    // collect variables and sort alphabetically
    const EnvVar = struct {
        name: []const u8,
        ty: TypeId,
        bound: *const Bound,

        pub fn lessThan(context: void, a: @This(), b: @This()) bool {
            _ = context;
            return std.ascii.lessThanIgnoreCase(a.name, b.name);
        }
    };

    var vars = std.ArrayList(EnvVar).init(ally);
    defer vars.deinit();

    var iter = self.ns.iterator();
    while (iter.next()) |entry| {
        try vars.append(EnvVar{
            .name = entry.key_ptr.str,
            .ty = entry.value_ptr.ty,
            .bound = &entry.value_ptr.value
        });
    }

    std.sort.sort(EnvVar, vars.items, {}, EnvVar.lessThan);

    // render variables
    var list = ctx.stub();
    for (vars.items) |ev| {
        const ty_text = try self.typeGet(ev.ty).writeAlloc(ally, self);
        defer ally.free(ty_text);

        const red = kz.Style{ .fg = .red };
        const row = [_]kz.Ref{
            try ctx.print(.{}, "{s}: ", .{ev.name}),
            try ctx.print(.{ .fg = .green }, "{s}", .{ty_text}),
            try ctx.print(.{}, " = ", .{}),
            switch (ev.bound.*) {
                .builtin_op => |op| op: {
                    const tag = @tagName(op);
                    const text = "<operator> {s}";
                    break :op try ctx.print(red, text, .{tag});
                },
                .builtin_flow => |flow| flow: {
                    const tag = @tagName(flow);
                    const text = "<flow> {s}";
                    break :flow try ctx.print(red, text, .{tag});
                },
                .ty => |id| ty: {
                    const got = self.typeGet(id);
                    const text = try got.writeAlloc(ally, self);
                    defer ally.free(text);
                    break :ty try ctx.print(.{}, "{s}", .{text});
                },
            }
        };

        const row_tex = try ctx.stack(&row, .right, .{});
        list = try ctx.slap(list, row_tex, .bottom, .{});
    }

    try ctx.write(list, writer);
}
