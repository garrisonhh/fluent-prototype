//! provides fluent type operations using handles to minimize the amount
//! of in-memory objects being used.

const std = @import("std");
const util = @import("util");
const builtin = @import("builtin");
const Env = @import("env.zig");

const Allocator = std.mem.Allocator;
const Wyhash = std.hash.Wyhash;
const Symbol = util.Symbol;

pub const TypeId = packed struct {
    const Self = @This();

    index: usize,

    pub fn eql(self: Self, id: Self) bool {
        return self.index == id.index;
    }

    pub const WriteError =
        std.ArrayList(u8).Writer.Error
     || std.fmt.AllocPrintError;

    pub fn write(
        self: Self,
        ally: Allocator,
        env: Env,
        writer: anytype
    ) WriteError!void {
        if (env.getTypename(self)) |name| {
            try writer.print("{}", .{name});
        } else {
            try env.typeGet(self).write(ally, env, writer);
        }
    }

    pub fn writeAlloc(
        self: Self,
        ally: Allocator,
        env: Env,
    ) WriteError![]u8 {
        var list = std.ArrayList(u8).init(ally);
        defer list.deinit();

        try self.write(ally, env, list.writer());

        return list.toOwnedSlice();
    }
};

/// storage for types and the context for type handles.
///
/// using a Type, you can call `identify()` which gives you the unique id of
/// this type. TypeIds can be 'dereferenced' with `get`.
///
/// the actual unique Type objects are just stored in the internal allocator,
/// but hashing and storing an arraylist of pointers allows the handle system
/// to function, and the memory/speed tradeoff is worth it given the vast amount
/// of these objects I'm creating and messing around with.
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
    types: std.ArrayListUnmanaged(*Type) = .{}, // TypeId -> Type
    map: TypeMap = .{}, // Type -> TypeId

    pub fn init(ally: Allocator) Self {
        return Self{
            .ally = ally,
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.types.items) |ptr| {
            ptr.deinit(self.ally);
            self.ally.destroy(ptr);
        }
        self.types.deinit(self.ally);
        self.map.deinit(self.ally);
    }

    pub fn get(self: Self, id: TypeId) *const Type {
        return self.types.items[id.index];
    }

    /// retrieves an established ID or creates a new one.
    pub fn identify(self: *Self, ty: Type) Allocator.Error!TypeId {
        const res = try self.map.getOrPut(self.ally, &ty);
        if (!res.found_existing) {
            // find id
            const id = TypeId{ .index = self.types.items.len };

            // allocate for type and clone
            const cloned = try util.placeOn(self.ally, try ty.clone(self.ally));

            // store in internal data structures
            res.key_ptr.* = cloned;
            res.value_ptr.* = id;

            try self.types.append(self.ally, cloned);
        }

        return res.value_ptr.*;
    }
};

pub const Type = union(enum) {
    const Self = @This();

    pub const Tag = std.meta.Tag(Self);

    pub const Set = std.AutoHashMapUnmanaged(TypeId, void);

    pub const Number = struct {
        // number literals don't always have a bit count
        bits: ?u8,
        layout: util.Number.Layout,
    };

    pub const Func = struct {
        generics: []TypeId,
        takes: []TypeId,
        contexts: []TypeId, // implicit parameters for effect management
        returns: TypeId,

        pub fn clone(self: Func, ally: Allocator) Allocator.Error!Func {
            return Func{
                .generics = try ally.dupe(TypeId, self.generics),
                .takes = try ally.dupe(TypeId, self.takes),
                .contexts = try ally.dupe(TypeId, self.contexts),
                .returns = self.returns,
            };
        }
    };

    /// generics only exist in the context of the current function
    pub const GenericId = struct { index: usize };

    // unique
    unit,
    hole,
    generic: GenericId,

    // dynamic time
    symbol,
    ty,

    // generic
    any,
    set: Set,

    // concrete (exist at static runtime)
    @"bool",
    number: Number,
    atom: Symbol,

    // structured
    list: TypeId, // stores subtype; lists are slices
    tuple: []TypeId,

    func: Func,

    pub fn initSet(
        ally: Allocator,
        subtypes: []const TypeId
    ) Allocator.Error!Self {
        var set = Set{};
        for (subtypes) |subty| {
            try set.put(ally, subty, {});
        }

        return Type{ .set = set };
    }

    pub fn deinit(self: *Self, ally: Allocator) void {
        switch (self.*) {
            .unit, .hole, .symbol, .any, .ty, .number, .list, .generic, .@"bool"
                => {},
            .set => |*set| set.deinit(ally),
            .atom => |sym| ally.free(sym.str),
            .tuple => |tup| ally.free(tup),
            .func => |func| {
                ally.free(func.generics);
                ally.free(func.takes);
                ally.free(func.contexts);
            },
        }
    }

    pub fn hash(self: Self, wyhash: *Wyhash) void {
        const asBytes = std.mem.asBytes;
        wyhash.update(asBytes(&std.meta.activeTag(self)));

        switch (self) {
            .unit, .symbol, .hole, .any, .ty, .@"bool" => {},
            .generic => |gid| wyhash.update(asBytes(&gid)),
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
                wyhash.update(asBytes(&func.generics));
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
            .unit, .symbol, .hole, .any, .ty, .@"bool" => true,
            .generic => |gid| gid.index == ty.generic.index,
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
                idsEql(func.generics, ty.func.generics)
                and idsEql(func.takes, ty.func.takes)
                and idsEql(func.contexts, ty.func.contexts)
                and func.returns.eql(ty.func.returns),
        };
    }

    pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
        return switch (self) {
            .unit, .symbol, .hole, .any, .number, .ty, .list, .generic, .@"bool"
                => self,
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
        target: Self
    ) Allocator.Error!bool {
        // manage set matching
        switch (target) {
            .any, .hole => return true,
            .set => {
                if (self == .set) {
                    // subsets coerce to supersets
                    var subtypes = self.set.keyIterator();
                    while (subtypes.next()) |subty| {
                        if (!target.set.contains(subty.*)) return false;
                    }

                    return true;
                } else {
                    // types coerce to sets containing types
                    return target.set.contains(try typewelt.identify(self));
                }
            },
            else => if (@as(Tag, self) != @as(Tag, target)) return false
        }

        // concrete matching
        return switch (target) {
            .any, .set, .hole, .generic => unreachable,
            .unit, .@"bool", .symbol, .ty => true,
            .atom => |sym| sym.eql(target.atom),
            .tuple => self.eql(target),
            .number => |num| num: {
                // layout must match
                if (num.layout != target.number.layout) {
                    break :num false;
                }

                // compiler numbers always coerce if the layout matches
                if (num.bits == null or target.number.bits == null) {
                    break :num true;
                }

                // must not lose bits in coercion
                break :num num.bits.? <= target.number.bits.?;
            },
            .list => |subty| subty.eql(self.list),
            .func => func: {
                // TODO I want functions to be able to coerce to functions with
                // wider effect sets I think?
                break :func self.eql(target);
            },
        };
    }

    /// given two types, create a type that fits the values of each
    pub fn unify(
        self: Self,
        typewelt: *TypeWelt,
        other: Self
    ) Allocator.Error!TypeId {
        std.debug.assert(self != .generic and other != .generic);

        if (self.eql(other)) {
            return try typewelt.identify(self);
        } else if (self == .any or other == .any) {
            return try typewelt.identify(Type{ .any = {} });
        } else if (try self.coercesTo(typewelt, other)) {
            return try typewelt.identify(other);
        } else if (try other.coercesTo(typewelt, self)) {
            return try typewelt.identify(self);
        }

        // need a new set to fit both
        var unified = try Self.initSet(typewelt.ally, &.{
            try typewelt.identify(self),
            try typewelt.identify(other)
        });
        defer unified.deinit(typewelt.ally);

        return try typewelt.identify(unified);
    }

    /// classifications for the runtime of a type; aka stages of the execution
    /// when a type is valid
    pub const RuntimeClass = enum {
        static, // valid at all stages
        dynamic, // valid before static compilation
        analysis, // valid only in semantic analysis

        /// returns the most restrictive class that fits both classes
        fn unify(self: @This(), other: @This()) @This() {
            const max_int = std.math.min(@enumToInt(self), @enumToInt(other));
            return @intToEnum(@This(), max_int);
        }

        fn unifyList(classes: []const @This()) @This() {
            var class = @intToEnum(@This(), 0);
            for (classes) |elem| {
                class = class.unify(elem);
            }

            return class;
        }
    };

    /// helper for classifyRuntime
    fn classifyList(list: []const TypeId, typewelt: TypeWelt) RuntimeClass {
        var class = RuntimeClass.static;
        for (list) |elem| {
            const elem_ty = typewelt.get(elem);
            const elem_class = elem_ty.classifyRuntime(typewelt);

            class = class.unify(elem_class);
        }

        return class;
    }

    /// at what point in the compilation cycle is this type valid?
    pub fn classifyRuntime(self: Self, typewelt: TypeWelt) RuntimeClass {
        return switch (self) {
            .any, .set, .hole, .generic => .analysis,
            .ty, .symbol => .dynamic,
            .unit, .atom, .@"bool" => .static,
            .number => |num| if (num.bits != null) .static else .dynamic,
            .list => |subty| typewelt.get(subty).classifyRuntime(typewelt),
            .tuple => |tup| classifyList(tup, typewelt),
            .func => |func| func: {
                var reqs = [3]RuntimeClass{
                    classifyList(func.takes, typewelt),
                    classifyList(func.contexts, typewelt),
                    typewelt.get(func.returns).classifyRuntime(typewelt)
                };

                break :func RuntimeClass.unifyList(&reqs);
            },
        };
    }

    pub const WriteError =
        std.ArrayList(u8).Writer.Error
     || std.fmt.AllocPrintError;

    fn writeList(
        list: []TypeId,
        ally: Allocator,
        env: Env,
        writer: anytype
    ) WriteError!void {
        try writer.writeByte('[');
        for (list) |ty, i| {
            if (i > 0) try writer.writeAll(", ");
            try ty.write(ally, env, writer);
        }
        try writer.writeByte(']');
    }

    /// simply the closest you can get to format
    pub fn write(
        self: Self,
        ally: Allocator,
        env: Env,
        writer: anytype
    ) WriteError!void {
        switch (self) {
            .unit, .@"bool" => try writer.writeAll(@tagName(self)),
            .ty => try writer.writeAll("Type"),
            .hole, .symbol, .any => try util.writeCaps(@tagName(self), writer),
            .atom => |sym| try writer.print("#{s}", .{sym.str}),
            .set => |set| {
                // render subtypes
                const subtypes = try ally.alloc([]const u8, set.count());
                defer {
                    for (subtypes) |text| ally.free(text);
                    ally.free(subtypes);
                }

                var keys = set.keyIterator();
                var i: usize = 0;
                while (keys.next()) |key| : (i += 1) {
                    subtypes[i] = try key.writeAlloc(ally, env);
                }

                // sort subtypes
                const Closure = struct {
                    /// just makes std.sort.sort happy
                    fn lessThan(ctx: void, a: []const u8, b: []const u8) bool {
                        _ = ctx;
                        return std.ascii.lessThanIgnoreCase(a, b);
                    }
                };

                std.sort.sort([]const u8, subtypes, {}, Closure.lessThan);

                // write subtypes
                try writer.writeAll("(Set");
                for (subtypes) |text| try writer.print(" {s}", .{text});
                try writer.writeByte(')');
            },
            .number => |num| {
                const layout = @tagName(num.layout);
                if (num.bits) |bits| {
                    try writer.print("{c}{}", .{layout[0], bits});
                } else {
                    try writer.print("compiler-{s}", .{layout});
                }
            },
            .list => |subty| {
                try writer.writeAll("(List ");
                try subty.write(ally, env, writer);
                try writer.writeByte(')');
            },
            .tuple => |tup| {
                try writer.writeAll("(Tuple");
                for (tup) |ty| {
                    try writer.writeByte(' ');
                    try ty.write(ally, env, writer);
                }
                try writer.writeByte(')');
            },
            .func => |func| {
                try writer.writeAll("(Fn ");
                try writeList(func.generics, ally, env, writer);
                try writer.writeByte(' ');
                try writeList(func.takes, ally, env, writer);
                try writer.writeByte(' ');
                try writeList(func.contexts, ally, env, writer);
                try writer.writeByte(' ');
                try func.returns.write(ally, env, writer);
                try writer.writeByte(')');
            },
            .generic => |gid| {
                const n = gid.index;
                if (n >= 26) {
                    try writer.print("generic-{}", .{n});
                } else {
                    try writer.writeByte('A' + @intCast(u8, n));
                }
            },
        }
    }

    pub fn writeAlloc(self: Self, ally: Allocator, env: Env) WriteError![]u8 {
        var list = std.ArrayList(u8).init(ally);
        defer list.deinit();

        try self.write(ally, env, list.writer());

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