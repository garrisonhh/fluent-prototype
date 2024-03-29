const std = @import("std");
const Allocator = std.mem.Allocator;
const Wyhash = std.hash.Wyhash;
const kz = @import("kritzler");
const com = @import("common");
const Name = com.Name;
const Symbol = com.Symbol;
const TypeWelt = @import("typewelt.zig");
const TypeId = TypeWelt.TypeId;
const Repr = @import("repr.zig").Repr;

pub const Type = union(enum) {
    const Self = @This();

    pub const Tag = std.meta.Tag(Self);
    pub const Set = std.AutoHashMapUnmanaged(TypeId, void);

    pub const Field = struct {
        name: Symbol,
        of: TypeId,
    };

    pub const Number = struct {
        // number literals don't always have a bit count
        bits: ?u8,
        layout: com.Number.Layout,
    };

    /// arrays are stack allocated slices with a size known before execution
    pub const Array = struct {
        size: usize,
        of: TypeId,
    };

    pub const Pointer = struct {
        // generally self explanatory. slices have very similar type rules to
        // pointers, but are actually a `struct { ptr: [*]to, len: usize }`
        pub const Kind = enum { single, many, slice };

        kind: Kind,
        to: TypeId,
    };

    pub const Func = struct {
        // TODO add context
        takes: []const TypeId,
        returns: TypeId,
    };

    // unique
    unit,
    hole,
    builtin,

    // dynamic time
    name,
    ty,

    // generic
    any,
    set: Set,

    // concrete (exist at static runtime)
    @"bool",
    number: Number,

    // structured
    array: Array,
    ptr: Pointer,
    tuple: []const TypeId,
    @"struct": []const Field,
    variant: []const Field,

    func: Func,

    pub fn initSet(
        ally: Allocator,
        subtypes: []const TypeId,
    ) Allocator.Error!Self {
        var set = Set{};
        for (subtypes) |subty| {
            try set.put(ally, subty, {});
        }

        return Type{ .set = set };
    }

    pub fn initPtr(kind: Pointer.Kind, to: TypeId) Self {
        return Self{ .ptr = .{ .kind = kind, .to = to } };
    }

    pub fn deinit(self: *Self, ally: Allocator) void {
        switch (self.*) {
            .unit,
            .hole,
            .builtin,
            .name,
            .any,
            .ty,
            .number,
            .array,
            .@"bool",
            .@"ptr",
            => {},
            .set => |*set| set.deinit(ally),
            .tuple => |tup| ally.free(tup),
            .@"struct", .variant => |fields| {
                for (fields) |field| ally.free(field.name.str);
                ally.free(fields);
            },
            .func => |func| ally.free(func.takes),
        }
    }

    pub const render = @import("render_type.zig").renderType;

    /// TODO this needs to be aware of it's 'self' TypeId, which should always
    /// be hashed to zero. once that is implemented, I can actually 'identify'
    /// structured types (which is currently broken)
    pub fn hash(self: Self, wyhash: *Wyhash) void {
        const b = std.mem.asBytes;
        wyhash.update(b(&@as(Tag, self)));

        switch (self) {
            // NOTE using an sorted ordered set datatype of some kind would
            // allow hasing here, but idk if it's worth it at this moment
            .set => {},

            inline .unit,
            .hole,
            .builtin,
            .name,
            .any,
            .@"bool",
            .ty,
            .number,
            .array,
            .ptr,
            .tuple,
            .func,
            => |data| std.hash.autoHashStrat(wyhash, data, .DeepRecursive),

            .@"struct", .variant => |fields| {
                for (fields) |field| {
                    wyhash.update(b(&field.name.hash));
                    wyhash.update(b(&field.of));
                }
            },
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

    /// TODO this needs to be aware of its 'self' id, or I need a version which
    /// does so
    pub fn eql(self: Self, ty: Self) bool {
        if (@as(Tag, self) != @as(Tag, ty)) return false;

        return switch (self) {
            .unit, .name, .hole, .any, .ty, .@"bool", .builtin => true,
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
            .ptr => |ptr| ptr.kind == ty.ptr.kind and ptr.to.eql(ty.ptr.to),
            .tuple => |tup| idsEql(tup, ty.tuple),
            inline .@"struct", .variant => |fields, tag| coll: {
                const ty_fields = @field(ty, @tagName(tag));

                if (fields.len != ty_fields.len) {
                    break :coll false;
                }

                for (fields) |field, i| {
                    const other = ty_fields[i];

                    if (!field.name.eql(other.name) or
                        !field.of.eql(other.of))
                    {
                        break :coll false;
                    }
                }

                break :coll true;
            },
            .number => |num| num.layout == ty.number.layout and
                num.bits == ty.number.bits,
            .array => |arr| arr.size == ty.array.size and
                arr.of.eql(ty.array.of),
            .func => |func| idsEql(func.takes, ty.func.takes) and
                func.returns.eql(ty.func.returns),
        };
    }

    pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
        return switch (self) {
            .unit,
            .name,
            .hole,
            .builtin,
            .any,
            .number,
            .ty,
            .array,
            .@"bool",
            .ptr,
            => self,

            .set => |set| Self{ .set = try set.clone(ally) },
            .tuple => |tup| Self{ .tuple = try ally.dupe(TypeId, tup) },
            inline .@"struct", .variant => |fields, tag| coll: {
                const cloned = try ally.alloc(Field, fields.len);
                for (fields) |field, i| {
                    cloned[i] = Field{
                        .name = try field.name.clone(ally),
                        .of = field.of,
                    };
                }

                break :coll @unionInit(Self, @tagName(tag), cloned);
            },
            .func => |func| Self{
                .func = Func{
                    .takes = try ally.dupe(TypeId, func.takes),
                    .returns = func.returns,
                },
            },
        };
    }

    pub const Coercion = enum {
        /// equivalent types, a type within a set, or a subset of another set
        inbounds,
        /// bitcast or truncation
        natural,
        /// `*[_]T` -> `[]T`
        array_ptr_to_slice,
    };

    /// types can coerce to other types in situations where the semantics
    /// remain well-defined, zB u32 coercing to u64
    pub fn coercesTo(
        self: Self,
        ally: Allocator,
        tw: *TypeWelt,
        target: Self,
    ) Allocator.Error!?Coercion {
        return switch (target) {
            .hole => unreachable,
            .any => .inbounds,
            .set => set: {
                if (self == .set) {
                    // subsets coerce to supersets
                    var subtypes = self.set.keyIterator();
                    while (subtypes.next()) |subty| {
                        if (!target.set.contains(subty.*)) {
                            break :set null;
                        }
                    }

                    break :set .inbounds;
                } else if (target.set.contains(try tw.identify(ally, self))) {
                    // types coerce to sets containing types
                    break :set .inbounds;
                }

                break :set null;
            },
            .number => |num| num: {
                if (self != .number) break :num null;

                const layouts_match = self.number.layout == num.layout;

                const from_compiler_num = self.number.bits == null;
                const have_bits = num.bits != null and self.number.bits != null;

                const bits_eql = have_bits and self.number.bits.? == num.bits.?;
                const bits_fit = have_bits and self.number.bits.? < num.bits.?;

                if (layouts_match) {
                    if (bits_eql) {
                        break :num .inbounds;
                    } else if (bits_fit or from_compiler_num) {
                        break :num .natural;
                    }
                }

                break :num null;
            },
            .ptr => |ptr| ptr: {
                if (self != .ptr) {
                    break :ptr null;
                } else if (self.eql(target)) {
                    break :ptr .inbounds;
                } else if (ptr.kind == .slice and
                    self.ptr.kind == .single)
                aptr: {
                    // inner must be array
                    const inner = tw.get(self.ptr.to);
                    if (inner.* != .array) break :aptr;

                    // array element type must be inbounds of slice element type
                    const arr_el = tw.get(inner.array.of);
                    const slice_el = tw.get(ptr.to);
                    const method = try arr_el.coercesTo(ally, tw, slice_el.*);
                    if (method != Coercion.inbounds) break :aptr;

                    break :ptr .array_ptr_to_slice;
                }

                break :ptr null;
            },
            else => if (self.eql(target)) .inbounds else null,
        };
    }

    /// classifications for the runtime of a type; aka stages of the execution
    /// when a type is valid
    pub const RuntimeClass = enum {
        static, // valid at all stages
        dynamic, // valid before static compilation
        analysis, // valid only in semantic analysis

        /// returns the most restrictive class that fits both classes
        fn peerClassify(self: @This(), other: @This()) @This() {
            const max_int = std.math.min(@enumToInt(self), @enumToInt(other));
            return @intToEnum(@This(), max_int);
        }

        fn peerClassifyList(classes: []const @This()) @This() {
            var class = @intToEnum(@This(), 0);
            for (classes) |elem| {
                class = class.peerClassify(elem);
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

            class = class.peerClassify(elem_class);
        }

        return class;
    }

    /// at what point in the compilation cycle is this type valid?
    pub fn classifyRuntime(self: Self, typewelt: TypeWelt) RuntimeClass {
        return switch (self) {
            .any, .set, .hole => .analysis,
            .ty, .name, .namespace => .dynamic,
            .unit, .atom, .@"bool", .builtin => .static,
            .number => |num| if (num.bits != null) .static else .dynamic,
            .array => |arr| typewelt.get(arr.of).classifyRuntime(typewelt),
            .ptr => |ptr| typewelt.get(ptr.to).classifyRuntime(typewelt),
            .tuple => |tup| classifyList(tup, typewelt),
            .func => |func| func: {
                var reqs = [2]RuntimeClass{
                    classifyList(func.takes, typewelt),
                    typewelt.get(func.returns).classifyRuntime(typewelt),
                };

                break :func RuntimeClass.peerClassifyList(&reqs);
            },
        };
    }
};
