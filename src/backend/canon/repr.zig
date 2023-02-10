const std = @import("std");
const Allocator = std.mem.Allocator;
const Wyhash = std.hash.Wyhash;
const com = @import("common");
const kz = @import("kritzler");
const TypeWelt = @import("typewelt.zig");
const TypeId = TypeWelt.TypeId;
const ReprWelt = @import("reprwelt.zig");
const ReprId = ReprWelt.ReprId;

/// encodes fluent data representation
///
/// used alongside ReprWelt
pub const Repr = union(enum) {
    pub const Tag = std.meta.Tag(Self);
    pub const Error = ConversionError || AccessError || QualError;

    const Self = @This();

    pub const Array = struct {
        size: usize,
        of: ReprId,
    };

    pub const Field = struct {
        offset: usize,
        of: ReprId,
    };

    pub const Conv = enum { by_value, by_ref };

    /// this encodes the relationship of a function parameter to its type
    pub const Param = struct {
        conv: Conv,
        of: ReprId,
    };

    pub const Func = struct {
        ctx: Param,
        takes: []Param,
        returns: Param,
    };

    // primitives
    unit,
    // hold byte width (MUST be 1, 2, 4, or 8 for int and 4 or 8 for float)
    uint: u4,
    int: u4,
    float: u4,

    // structured data
    ptr: ReprId,
    array: Array,
    coll: []Field,

    // else
    func: Func,

    /// dupes input
    pub fn initColl(
        ally: Allocator,
        coll: []const Field,
    ) Allocator.Error!Self {
        return Self{ .coll = try ally.dupe(Field, coll) };
    }

    /// dupes input
    pub fn initFunc(
        ally: Allocator,
        ctx: Param,
        takes: []const Param,
        returns: Param,
    ) Allocator.Error!Self {
        return Self{ .func = Func{
            .ctx = ctx,
            .takes = try ally.dupe(Param, takes),
            .returns = returns,
        } };
    }

    pub fn deinit(self: Self, ally: Allocator) void {
        switch (self) {
            .unit, .uint, .int, .float, .ptr, .array => {},
            .coll => |coll| ally.free(coll),
            .func => |func| ally.free(func.takes),
        }
    }

    fn paramOfType(
        ally: Allocator,
        rw: *ReprWelt,
        tw: TypeWelt,
        id: TypeId,
    ) ConversionError!Param {
        const repr = try Self.ofType(ally, rw, tw, id);
        return Param{
            .conv = repr.getConv(),
            .of = try rw.intern(ally, repr),
        };
    }

    pub const ConversionError = Allocator.Error || error{NoRepr};

    pub fn ofType(
        ally: Allocator,
        rw: *ReprWelt,
        tw: TypeWelt,
        id: TypeId,
    ) ConversionError!Self {
        const ty = tw.get(id);
        return switch (ty.*) {
            // no repr
            .hole,
            .namespace,
            .any,
            .set,
            => return error.NoRepr,

            .unit => .unit,
            .@"bool" => Self{ .uint = 1 },
            .atom, .ty, .builtin => Self{ .uint = 8 },
            .array => |arr| Self{ .array = .{
                .size = arr.size,
                .of = try rw.reprOf(ally, tw, arr.of),
            } },
            .number => |num| num: {
                const nbytes = if (num.bits) |bits| @divExact(bits, 8) else 8;
                break :num switch (num.layout) {
                    inline else => |tag| @unionInit(
                        Self,
                        @tagName(tag),
                        @intCast(u4, nbytes),
                    ),
                };
            },
            .ptr => |ptr| switch (ptr.kind) {
                .single, .many => Self{
                    .ptr = try rw.reprOf(ally, tw, ptr.to),
                },
                .slice => slice: {
                    // TODO this should be const
                    const u64_repr = rw.reprs.get(.{ .uint = 8 }).?;
                    const ptr_repr = try rw.intern(ally, Self{
                        .ptr = try rw.reprOf(ally, tw, ptr.to),
                    });

                    const fields = [_]Field{
                        Field{ .offset = 0, .of = ptr_repr },
                        Field{ .offset = 8, .of = u64_repr },
                    };

                    break :slice try Self.initColl(ally, &fields);
                },
            },
            .func => |func| func: {
                // NOTE this implements the base truth for the fluent callconv
                const takes = try ally.alloc(Param, func.takes.len);
                for (func.takes) |param, i| {
                    takes[i] = try paramOfType(ally, rw, tw, param);
                }

                const unit = try rw.intern(ally, .unit);
                const ctx = Param{
                    .conv = rw.get(unit).getConv(),
                    .of = unit,
                };

                const returns = try paramOfType(ally, rw, tw, func.returns);

                break :func Self{ .func = .{
                    .ctx = ctx,
                    .takes = takes,
                    .returns = returns,
                } };
            },
            else => |tag| std.debug.panic("TODO convert repr of {}", .{tag}),
        };
    }

    /// whether this repr is a collection with accessible fields
    pub fn isStructured(self: Self) bool {
        return switch (self) {
            .unit, .uint, .int, .float, .ptr, .func => false,
            .array, .coll => true,
        };
    }

    /// how a value of this repr is conventionally represented in memory
    pub fn getConv(self: Self) Conv {
        return switch (self) {
            .unit, .uint, .int, .float, .ptr => .by_value,
            .func, .array, .coll => .by_ref,
        };
    }

    pub const AccessError = QualError || error{ NotCollection, OutOfBounds };

    /// get the repr of a field (assuming this is a collection of some kind)
    pub fn access(self: Self, rw: ReprWelt, index: usize) AccessError!Field {
        return switch (self) {
            .unit,
            .uint,
            .int,
            .float,
            .ptr,
            .func,
            => AccessError.NotCollection,

            .array => |arr| arr: {
                if (index >= arr.size) {
                    break :arr AccessError.OutOfBounds;
                }

                const el_aln = try rw.alignOf(arr.of);
                const el_size = try rw.sizeOfAligned(arr.of, el_aln);

                return Field{
                    .offset = el_size * index,
                    .of = arr.of,
                };
            },
            .coll => |coll| coll: {
                if (index >= coll.len) {
                    break :coll AccessError.NotCollection;
                }

                break :coll coll[index];
            },
        };
    }

    pub const QualError = error{ SizeOfFunc, AlignOfFunc };

    // TODO cache this
    pub fn alignOf(self: Self, rw: ReprWelt) QualError!usize {
        return switch (self) {
            .func => error.AlignOfFunc,
            .unit => 1,
            .ptr => 8,
            .uint, .int, .float => |nbytes| nbytes,
            .array => |arr| try rw.get(arr.of).alignOf(rw),
            .coll => |coll| coll: {
                var max_aln: usize = 1;
                for (coll) |field| {
                    const child_aln = try rw.get(field.of).alignOf(rw);
                    max_aln = @max(max_aln, child_aln);
                }

                break :coll max_aln;
            },
        };
    }

    // TODO cache this
    pub fn sizeOf(self: Self, rw: ReprWelt) QualError!usize {
        return switch (self) {
            .func => error.SizeOfFunc,
            .unit => 0,
            .ptr => 8,
            .uint, .int, .float => |nbytes| nbytes,
            .array => |arr| arr.size * try rw.get(arr.of).sizeOf(rw),
            .coll => |coll| coll: {
                if (coll.len == 0) break :coll 0;

                var size: usize = 0;
                for (coll) |field| {
                    const repr = rw.get(field.of);

                    // ensure field is on an aligned boundary
                    const aln = try repr.alignOf(rw);
                    const aln_diff = size % aln;
                    if (aln_diff > 0) {
                        size += aln - aln_diff;
                    }

                    size += try repr.sizeOf(rw);
                }

                break :coll size;
            },
        };
    }

    pub fn sizeOfAligned(self: Self, rw: ReprWelt, aln: usize) QualError!usize {
        const size = try self.sizeOf(rw);

        const aln_diff = size % aln;
        if (aln_diff > 0) {
            return size + (aln - aln_diff);
        }

        return size;
    }

    pub fn hash(self: Self, wyhash: *Wyhash) void {
        std.hash.autoHashStrat(wyhash, self, .Deep);
    }

    fn sliceEql(comptime T: type, a: []const T, b: []const T) bool {
        if (a.len != b.len) return false;

        for (a) |el, i| {
            if (!std.meta.eql(el, b[i])) {
                return false;
            }
        }

        return true;
    }

    pub fn eql(self: Self, other: Self) bool {
        return @as(Tag, self) == @as(Tag, other) and switch (self) {
            .coll => |coll| sliceEql(Field, coll, other.coll),
            .func => |func| std.meta.eql(func.ctx, other.func.ctx) and
                std.meta.eql(func.returns, other.func.returns) and
                sliceEql(Param, func.takes, other.func.takes),
            else => std.meta.eql(self, other),
        };
    }

    pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
        return switch (self) {
            .unit, .uint, .int, .float, .ptr, .array => self,
            .coll => |coll| try initColl(ally, coll),
            .func => |func| try initFunc(ally, func.takes, func.returns),
        };
    }

    fn renderParam(
        param: Param,
        ctx: *kz.Context,
        rw: ReprWelt,
    ) Allocator.Error!kz.Ref {
        const repr = try param.of.render(ctx, rw);

        return switch (param.conv) {
            .by_value => repr,
            .by_ref => try ctx.slap(
                try ctx.print(.{}, "&", .{}),
                repr,
                .right,
                .{},
            ),
        };
    }

    pub fn render(
        self: Self,
        ctx: *kz.Context,
        rw: ReprWelt,
    ) Allocator.Error!kz.Ref {
        const sty = kz.Style{ .fg = .blue };
        const offset_sty = kz.Style{ .fg = .yellow };

        return switch (self) {
            .unit => try ctx.print(sty, "unit", .{}),
            .uint, .int, .float => |bytes| try ctx.print(
                sty,
                "{c}{}",
                .{ @tagName(self)[0], 8 * @intCast(usize, bytes) },
            ),
            .ptr => |to| try ctx.slap(
                try ctx.print(.{}, "*", .{}),
                try to.render(ctx, rw),
                .right,
                .{},
            ),
            .array => |arr| try ctx.slap(
                try ctx.print(.{}, "[{}]", .{arr.size}),
                try arr.of.render(ctx, rw),
                .right,
                .{},
            ),
            .coll => |coll| coll: {
                var list = try ctx.ally.alloc(kz.Ref, coll.len);
                defer ctx.ally.free(list);

                for (coll) |field, i| {
                    list[i] = try ctx.slap(
                        try field.of.render(ctx, rw),
                        try ctx.print(offset_sty, "+{}", .{field.offset}),
                        .right,
                        .{ .space = 1 },
                    );
                }

                break :coll try ctx.stack(
                    &.{
                        try ctx.print(.{}, "(", .{}),
                        try ctx.sep(
                            try ctx.print(.{}, ", ", .{}),
                            list,
                            .right,
                            .{},
                        ),
                        try ctx.print(.{}, ")", .{}),
                    },
                    .right,
                    .{},
                );
            },
            .func => |func| func: {
                const takes = try ctx.ally.alloc(kz.Ref, func.takes.len);
                defer ctx.ally.free(takes);

                for (func.takes) |param, i| {
                    takes[i] = try renderParam(param, ctx, rw);
                }

                break :func try ctx.stack(
                    &.{
                        try ctx.print(.{}, "with ", .{}),
                        try renderParam(func.ctx, ctx, rw),
                        try ctx.print(.{}, " {{", .{}),
                        try ctx.sep(
                            try ctx.print(.{}, ", ", .{}),
                            takes,
                            .right,
                            .{},
                        ),
                        try ctx.print(.{}, "}} -> ", .{}),
                        try renderParam(func.returns, ctx, rw),
                    },
                    .right,
                    .{},
                );
            },
        };
    }
};
