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

const Self = @This();

pub const DefError =
    Allocator.Error
 || error { SymbolRedef };

pub const Bound = union(enum) {
    unimpl,
    ty: TypeId,
};

const Binding = struct {
    ty: TypeId,
    value: Bound
};

ally: Allocator,
typewelt: *TypeWelt,
parent: ?*const Self,

// keys and values are all owned by this env
ns: Symbol.HashMapUnmanaged(Binding) = .{},

pub fn initBase(ally: Allocator, typewelt: *TypeWelt) Self {
    return Self{
        .ally = ally,
        .typewelt = typewelt,
        .parent = null,
    };
}

pub fn init(parent: *const Self) Self {
    return Self{
        .ally = parent.ally,
        .typewelt = parent.typewelt,
        .parent = parent,
    };
}

pub fn deinit(self: *Self) void {
    var keys = self.ns.keyIterator();
    while (keys.next()) |key| self.ally.free(key.str);
    self.ns.deinit(self.ally);
}

pub fn def(self: *Self, sym: Symbol, to: Binding) DefError!void {
    if (self.contains(sym)) return error.SymbolRedef;

    try self.ns.put(self.ally, try sym.clone(self.ally), to);
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

pub fn getBound(self: Self, sym: Symbol) ?*const Bound {
    return if (self.get(sym)) |binding| &binding.value else null;
}

// type helpers ================================================================

pub fn typeIdentify(self: Self, ty: Type) Allocator.Error!TypeId {
    return try self.typewelt.identify(ty);
}

pub fn typeIdentifyNumber(
    self: Self,
    layout: util.Number.Layout,
    bits: u8
) Allocator.Error!TypeId {
    const ty = Type{ .number = .{ .layout = layout, .bits = bits } };
    return try self.typeIdentify(ty);
}

pub fn typeUnify(
    self: Self,
    outward: TypeId,
    inward: TypeId
) Allocator.Error!?TypeId {
    return try types.unify(self.typewelt, outward, inward);
}

pub fn typeGet(self: Self, ty: TypeId) *const Type {
    return self.typewelt.get(ty);
}

pub fn typeDef(self: *Self, sym: Symbol, ty: Type) DefError!TypeId {
    // register in typewelt
    const id = try self.typeIdentify(ty);
    try self.typewelt.putName(id, sym);

    // define in env
    try self.def(sym, Binding{
        .ty = try self.typeIdentify(Type{ .ty = {} }),
        .value = .{ .ty = id }
    });

    return id;
}

// display =====================================================================

pub fn render(self: Self, ally: Allocator) !kz.Texture {
    var arena = std.heap.ArenaAllocator.init(ally);
    defer arena.deinit();
    const tmp_ally = arena.allocator();

    // collect variables and sort alphabetically
    const EnvVar = struct {
        name: []const u8,
        ty: TypeId,
        bound: *const Bound,

        pub fn lessThan(ctx: void, a: @This(), b: @This()) bool {
            _ = ctx;
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
    var rows = std.ArrayList([3]kz.Texture).init(ally);
    defer rows.deinit();

    for (vars.items) |ev| {
        const red = kz.Format{ .fg = .red };
        const green = kz.Format{ .fg = .green };

        const ty = self.typeGet(ev.ty);
        const ty_text = try ty.writeAlloc(tmp_ally, self.typewelt);

        const row = try rows.addOne();
        row[0] = try kz.Texture.from(tmp_ally, kz.Format{}, ev.name);
        row[1] = try kz.Texture.from(tmp_ally, green, ty_text);
        row[2] = switch (ev.bound.*) {
            .unimpl => try kz.Texture.from(tmp_ally, red, "?"),
            .ty => |id| ty: {
                const got = self.typeGet(id);
                const text = try got.writeAlloc(tmp_ally, self.typewelt);
                break :ty try kz.Texture.from(tmp_ally, kz.Format{}, text);
            },
        };
    }

    // final rendering
    const titles = [_][]const u8{"name", "type", "binding"};
    const sep = 2;

    // figure out proper column widths
    var widths: [3]usize = undefined;
    for (titles) |title, i| widths[i] = title.len;

    for (rows.items) |row| {
        for (row) |tex, i| {
            widths[i] = std.math.max(widths[i], tex.size[0]);
        }
    }

    // render titles
    const width = width: {
        var total: usize = 0;
        for (widths) |w, i| {
            if (i > 0) total += sep;
            total += w;
        }

        break :width total;
    };

    var rendered = try kz.Texture.init(tmp_ally, .{width, 2});

    var i: usize = 0;
    for (titles) |title, j| {
        for (title) |ch, k| {
            rendered.set(.{i + k, 0}, kz.Format{}, ch);
        }
        i += widths[j] + sep;
    }

    i = 0;
    while (i < width) : (i += 1) {
        rendered.set(.{i, 1}, kz.Format{}, '-');
    }

    // slap all the rows together
    for (rows.items) |row| {
        var row_tex = try kz.Texture.init(tmp_ally, .{width, 0});

        i = 0;
        for (row) |tex, j| {
            const x = @intCast(isize, i);
            row_tex = try row_tex.unify(tmp_ally, tex, .{x, 0});
            i += widths[j] + sep;
        }

        rendered = try rendered.slap(tmp_ally, row_tex, .bottom, .close);
    }

    return try rendered.clone(ally);
}
