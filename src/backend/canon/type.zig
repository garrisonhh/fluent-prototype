const std = @import("std");
const Allocator = std.mem.Allocator;
const Wyhash = std.hash.Wyhash;
const kz = @import("kritzler");
const com = @import("common");
const Name = com.Name;
const TypeWelt = @import("typewelt.zig");
const TypeId = TypeWelt.TypeId;
const Repr = @import("repr.zig").Repr;

pub const Type = union(enum) {
    const Self = @This();

    pub const Tag = std.meta.Tag(Self);
    pub const Set = std.AutoHashMapUnmanaged(TypeId, void);

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
        takes: []TypeId,
        returns: TypeId,
    };

    // unique
    unit,
    hole,
    namespace,
    builtin,

    // dynamic time
    symbol,
    ty,

    // generic
    any,
    set: Set,

    // concrete (exist at static runtime)
    @"bool",
    number: Number,
    atom: Name, // this is not owned by the type

    // structured
    array: Array,
    ptr: Pointer,
    tuple: []TypeId,

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
            // zig fmt: off
            .unit, .hole, .namespace, .builtin, .symbol, .any, .ty, .number,
            .array, .@"bool", .@"ptr", .atom,
            // zig fmt: on
            => {},
            .set => |*set| set.deinit(ally),
            .tuple => |tup| ally.free(tup),
            .func => |func| ally.free(func.takes),
        }
    }

    pub const render = @import("render_type.zig").renderType;

    /// this uses kritzler, so ANSI codes WILL be included
    pub fn toString(
        self: Self,
        ally: Allocator,
        tw: TypeWelt,
    ) Allocator.Error![]u8 {
        var buf = std.ArrayList(u8).init(ally);
        defer buf.deinit();

        try kz.display(ally, tw, self, buf.writer());

        return buf.toOwnedSlice();
    }

    pub fn hash(self: Self, wyhash: *Wyhash) void {
        const asBytes = std.mem.asBytes;
        wyhash.update(asBytes(&std.meta.activeTag(self)));

        switch (self) {
            // zig fmt: off
            .unit, .symbol, .hole, .any, .ty, .@"bool", .namespace, .builtin
            // zig fmt: on
            => {},
            .set => {
                // NOTE if there is a serious issue here, figure out if there is
                // a way to hash this in constant space. for now I'm just
                // throwing my hands up and allowing .eql() to handle the work
                // of figuring out if two sets match.
                // NOTE after some thought I think the easiest solution is just
                // using an ordered set
            },
            .atom => |sym| wyhash.update(asBytes(&sym.hash)),
            .number => |num| {
                wyhash.update(asBytes(&num.layout));
                wyhash.update(asBytes(&num.bits));
            },
            .array => |arr| {
                wyhash.update(asBytes(&arr.size));
                wyhash.update(asBytes(&arr.of));
            },
            .ptr => |ptr| {
                wyhash.update(asBytes(&ptr.kind));
                wyhash.update(asBytes(&ptr.to));
            },
            .tuple => |tup| wyhash.update(asBytes(&tup)),
            .func => |func| {
                wyhash.update(asBytes(&func.takes));
                wyhash.update(asBytes(&func.returns));
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

    pub fn eql(self: Self, ty: Self) bool {
        if (@as(Tag, self) != @as(Tag, ty)) return false;

        return switch (self) {
            // zig fmt: off
            .unit, .symbol, .hole, .any, .ty, .@"bool", .namespace, .builtin,
            // zig fmt: on
            => true,
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
            .ptr => |ptr| ptr.kind == ty.ptr.kind and ptr.to.eql(ty.ptr.to),
            .tuple => |tup| idsEql(tup, ty.tuple),
            // zig fmt: off
            .number => |num| num.layout == ty.number.layout
                         and num.bits == ty.number.bits,
            .array => |arr| arr.size == ty.array.size
                        and arr.of.eql(ty.array.of),
            .func => |func| idsEql(func.takes, ty.func.takes)
                        and func.returns.eql(ty.func.returns),
            // zig fmt: on
        };
    }

    pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
        return switch (self) {
            // zig fmt: off
            .unit, .symbol, .hole, .namespace, .builtin, .any, .number, .ty,
            .array, .@"bool", .ptr,
            // zig fmt: on
            => self,
            .set => |set| Self{ .set = try set.clone(ally) },
            .atom => |name| Self{ .atom = name },
            .tuple => |tup| Self{ .tuple = try ally.dupe(TypeId, tup) },
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
            .ty, .symbol, .namespace => .dynamic,
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
