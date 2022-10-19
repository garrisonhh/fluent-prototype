//! provides fluent type operations using handles to minimize the amount
//! of in-memory objects being used.

const std = @import("std");
const util = @import("util");
const builtin = @import("builtin");

const Allocator = std.mem.Allocator;
const Wyhash = std.hash.Wyhash;
const Symbol = util.Symbol;

pub const TypeId = packed struct {
    const Self = @This();

    index: usize,

    pub fn eql(self: Self, id: Self) bool {
        return self.index == id.index;
    }
};

/// where types are stored.
pub const TypeWelt = struct {
    const Self = @This();

    const TypeMapContext = struct {
        pub fn hash(ctx: @This(), key: *const Type) u64 {
            _ = ctx;
            var wyhash = Wyhash.init(0);
            key.hash(&wyhash);

            return wyhash.final();
        }

        pub fn eql(ctx: @This(), a: *const Type, b: *const Type) bool {
            _ = ctx;
            return a.eql(b.*);
        }
    };
    const TypeMap = std.HashMapUnmanaged(
        *const Type,
        TypeId,
        TypeMapContext,
        std.hash_map.default_max_load_percentage
    );

    ally: Allocator,
    types: std.ArrayListUnmanaged(Type) = .{},
    map: TypeMap = .{},

    pub fn init(ally: Allocator) Self {
        return Self{
            .ally = ally,
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.types.items) |*ty| ty.deinit(self.ally);
        self.types.deinit(self.ally);
        self.map.deinit(self.ally);
    }

    pub fn get(self: Self, id: TypeId) *const Type {
        return &self.types.items[id.index];
    }

    /// retrieves an established ID or creates a new one.
    pub fn identify(self: *Self, ty: Type) Allocator.Error!TypeId {
        const res = try self.map.getOrPut(self.ally, &ty);
        if (!res.found_existing) {
            const id = TypeId{ .index = self.types.items.len };
            var slot = try self.types.addOne(self.ally);

            slot.* = try ty.clone(self.ally);
            res.key_ptr.* = slot;
            res.value_ptr.* = id;
        }

        return res.value_ptr.*;
    }
};

pub const Type = union(enum) {
    const Self = @This();

    pub const Tag = std.meta.Tag(Self);

    pub const Set = std.AutoHashMapUnmanaged(TypeId, void);
    pub const Number = struct {
        bits: ?u8,
        layout: util.Number.Layout,
    };
    pub const Func = struct {
        takes: []TypeId,
        // implicit parameters for effect management
        contexts: []TypeId,
        returns: TypeId,

        pub fn clone(self: Func, ally: Allocator) Allocator.Error!Func {
            return Func{
                .takes = try ally.dupe(TypeId, self.takes),
                .contexts = try ally.dupe(TypeId, self.contexts),
                .returns = self.returns,
            };
        }
    };

    // unique
    unit,
    hole,

    // dynamic time
    symbol,
    ty,

    // generic
    any,
    set: Set,

    // concrete (exist at runtime)
    atom: Symbol,
    number: Number,

    list: TypeId, // stores subtype; lists are slices
    tuple: []TypeId,

    func: Func,

    pub fn deinit(self: *Self, ally: Allocator) void {
        switch (self.*) {
            .unit, .hole, .symbol, .any, .ty, .number, .list => {},
            .set => |*set| set.deinit(ally),
            .atom => |sym| ally.free(sym.str),
            .tuple => |tup| ally.free(tup),
            .func => |func| {
                ally.free(func.takes);
                ally.free(func.contexts);
            },
        }
    }

    pub fn hash(self: Self, wyhash: *Wyhash) void {
        const asBytes = std.mem.asBytes;
        wyhash.update(asBytes(&std.meta.activeTag(self)));

        switch (self) {
            .unit, .symbol, .hole, .any, .ty => {},
            .set => {
                // NOTE if there is a serious issue here, figure out if there is
                // a way to hash this in constant space. for now I'm just
                // throwing my hands up and allowing .eql() to handle the work
                // of figuring out if two sets match.
            },
            .atom => |sym| wyhash.update(asBytes(&sym.hash)),
            .number => |num| {
                wyhash.update(asBytes(&num.layout));
                wyhash.update(asBytes(&num.bits));
            },
            .list => |subty| wyhash.update(asBytes(&subty)),
            .tuple => |tup| wyhash.update(asBytes(&tup)),
            .func => |func| {
                wyhash.update(asBytes(&func.takes));
                wyhash.update(asBytes(&func.contexts));
                wyhash.update(asBytes(&func.returns));
            }
        }
    }

    /// whether two id arrays are equivalent
    fn idsEql(a: []const TypeId, b: []const TypeId) bool {
        if (a.len != b.len) return false;

        for (a) |elem, i| {
            if (!elem.eql(b[i])) {
                return false;
            }
        }

        return true;
    }

    pub fn eql(self: Self, ty: Self) bool {
        if (@as(Tag, self) != @as(Tag, ty)) return false;

        return switch (self) {
            .unit, .symbol, .hole, .any, .ty => true,
            .set => |set| set: {
                if (set.count() != ty.set.count()) {
                    break :set false;
                }

                var subtypes = set.keyIterator();
                while (subtypes.next()) |subty| {
                    if (!ty.set.contains(subty.*)) break :set false;
                }

                break :set true;
            },
            .atom => |sym| sym.eql(ty.atom),
            .number => |num|
                num.layout == ty.number.layout and num.bits == ty.number.bits,
            .list => |subty| subty.eql(ty.list),
            .tuple => |tup| idsEql(tup, ty.tuple),
            .func => |func|
                idsEql(func.takes, ty.func.takes)
                and idsEql(func.contexts, ty.func.contexts)
                and func.returns.eql(ty.func.returns),
        };
    }

    pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
        return switch (self) {
            .unit, .symbol, .hole, .any, .number, .ty, .list => self,
            .set => |set| Self{ .set = try set.clone(ally) },
            .atom => |sym| Self{ .atom = try sym.clone(ally) },
            .tuple => |tup| Self{ .tuple = try ally.dupe(TypeId, tup) },
            .func => |func| Self{ .func = try func.clone(ally) },
        };
    }

    /// types can coerce to other types in situations where the semantics
    /// remain well-defined, zB u32 coercing to u64
    pub fn coercesTo(
        self: Self,
        typewelt: *TypeWelt,
        ty: Self
    ) Allocator.Error!bool {
        // manage set matching
        switch (ty) {
            .any, .hole => return true,
            .set => {
                // TODO allow subsets to coerce to their supersets
                return ty.set.contains(try typewelt.identify(self));
            },
            else => if (@as(Tag, self) != @as(Tag, ty)) return false
        }

        // concrete matching
        return switch (ty) {
            .any, .set, .hole => unreachable,
            .unit, .symbol, .ty => true,
            .atom => |sym| sym.eql(ty.atom),
            .tuple => self.eql(ty),
            .number => |num| num: {
                // layout must match
                if (num.layout != ty.number.layout) {
                    break :num false;
                }

                // must not lose bits in coercion
                if (ty.number.bits != null and num.bits != null) {
                    break :num num.bits.? <= ty.number.bits.?;
                } else {
                    break :num true;
                }
            },
            .list => |subty| subty.eql(self.list),
            .func => func: {
                // TODO I want functions to be able to coerce to functions with
                // wider effect sets I think?
                break :func self.eql(ty);
            },
        };
    }

    fn writeList(
        list: []TypeId,
        typewelt: TypeWelt,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        try writer.writeByte('[');
        for (list) |ty, i| {
            if (i > 0) try writer.writeAll(", ");
            try typewelt.get(ty).write(typewelt, writer);
        }
        try writer.writeByte(']');
    }

    /// simply the closest you can get to format
    pub fn write(
        self: Self,
        typewelt: TypeWelt,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        switch (self) {
            .unit => try writer.writeAll(@tagName(self)),
            .ty => try writer.writeAll("Type"),
            .hole, .symbol, .any => try util.writeCaps(@tagName(self), writer),
            .set => @panic("TODO write type set"),
            .atom => |sym| try writer.print("#{s}", .{sym.str}),
            .number => |num| {
                const layout = @tagName(num.layout);
                if (num.bits) |bits| {
                    try writer.print("{c}{}", .{layout[0], bits});
                } else {
                    try writer.writeAll(layout);
                }
            },
            .list => |subty| {
                try writer.writeAll("(List ");
                try typewelt.get(subty).write(typewelt, writer);
                try writer.writeByte(')');
            },
            .tuple => |tup| {
                try writer.writeAll("(Tuple");
                for (tup) |ty| {
                    try writer.writeByte(' ');
                    try typewelt.get(ty).write(typewelt, writer);
                }
                try writer.writeByte(')');
            },
            .func => |func| {
                try writer.writeAll("(Fn ");
                try writeList(func.takes, typewelt, writer);
                try writer.writeByte(' ');
                try writeList(func.contexts, typewelt, writer);
                try writer.writeByte(' ');
                try typewelt.get(func.returns).write(typewelt, writer);
                try writer.writeByte(')');
            }
        }
    }

    pub const WriteAllocError =
        std.ArrayList(u8).Writer.Error
     || std.fmt.AllocPrintError;

    pub fn writeAlloc(
        self: Self,
        ally: Allocator,
        typewelt: TypeWelt
    ) WriteAllocError![]const u8 {
        var list = std.ArrayList(u8).init(ally);
        defer list.deinit();

        try self.write(typewelt, list.writer());

        return list.toOwnedSlice();
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        _ = self;
        _ = fmt;
        _ = options;

        @compileError("Type.format is deprecated; "
                   ++ "please use Type.write() or Type.writeAlloc()");
    }
};

/// using an 'outward' expectation for the type of an expression, and the
/// 'inward' information an expression has about its own type, unify()
/// determines what the final type of the expression should be.
///
/// this is basically the core of fluent's 'algorithm w' implementation.
pub fn unify(
    typewelt: *TypeWelt,
    outward: TypeId,
    inward: TypeId
) Allocator.Error!?TypeId {
    const out = typewelt.get(outward);
    const in = typewelt.get(inward);

    if (out.eql(in.*)) {
        // already unified
        return outward;
    } else if (!try in.coercesTo(typewelt, out.*)) {
        // cannot coerce
        return null;
    }

    // coercion is possible, unify
    var ty = switch (out.*) {
        .number => try out.clone(typewelt.ally),
        else => try in.clone(typewelt.ally)
    };
    defer ty.deinit(typewelt.ally);

    // verify unification was successful
    if (builtin.mode == .Debug) {
        const ally = typewelt.ally;
        const in_text = try in.writeAlloc(ally, typewelt.*);
        const ty_text = try ty.writeAlloc(ally, typewelt.*);
        const out_text = try out.writeAlloc(ally, typewelt.*);
        defer ally.free(in_text);
        defer ally.free(ty_text);
        defer ally.free(out_text);

        if (!try in.coercesTo(typewelt, ty)) {
            std.debug.panic(
                "inward {s} does not coerce to unified {s}",
                .{in_text, ty_text}
            );
        } else if (!try ty.coercesTo(typewelt, out.*)) {
            std.debug.panic(
                "unified {s} does not coerce to outward {s}",
                .{ty_text, out_text}
            );
        }
    }

    return try typewelt.identify(ty);
}