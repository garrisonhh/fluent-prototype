const std = @import("std");
const Allocator = std.mem.Allocator;
const Wyhash = std.hash.Wyhash;
const com = @import("common");
const kz = @import("kritzler");
const TypeWelt = @import("typewelt.zig");
const TypeId = TypeWelt.TypeId;
const ReprWelt = @import("reprwelt.zig");
const ReprId = ReprWelt.ReprId;

/// encodes fluent representational types.
///
/// generally this should be interacted with through ReprWelt
pub const Repr = union(enum) {
    pub const Tag = std.meta.Tag(Self);
    const Error = ReprWelt.Error;

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

        pub fn ofType(
            ally: Allocator,
            rw: *ReprWelt,
            tw: TypeWelt,
            id: TypeId,
        ) Error!Param {
            const repr = try rw.ofType(ally, tw, id);
            return Param{
                .conv = rw.get(repr).getConv(),
                .of = repr,
            };
        }
    };

    pub const Func = struct {
        ctx: Param,
        takes: []const Param,
        returns: Param,
    };

    // for opaque pointers
    opaq,

    // primitives
    unit,
    uint: u4, // 1, 2, 4, or 8
    int: u4, // 1, 2, 4, or 8
    float: u4, // 4 or 8

    // structured data
    ptr: ReprId,
    array: Array,
    coll: []const Field,

    // else
    func: Func,

    pub fn deinit(self: Self, ally: Allocator) void {
        switch (self) {
            .opaq, .unit, .uint, .int, .float, .ptr, .array => {},
            .coll => |coll| ally.free(coll),
            .func => |func| ally.free(func.takes),
        }
    }

    /// whether this repr is a collection with accessible fields
    pub fn isStructured(self: Self) bool {
        return switch (self) {
            .opaq => unreachable,
            .unit, .uint, .int, .float, .ptr, .func => false,
            .array, .coll => true,
        };
    }

    /// how a value of this repr is conventionally represented in memory
    pub fn getConv(self: Self) Conv {
        return switch (self) {
            .opaq => unreachable,
            .unit, .uint, .int, .float, .ptr => .by_value,
            .func, .array, .coll => .by_ref,
        };
    }

    pub const AccessError = QualError || error{ NotCollection, OutOfBounds };

    /// get the repr of a field (assuming this is a collection of some kind)
    pub fn access(self: Self, rw: ReprWelt, index: usize) AccessError!Field {
        return switch (self) {
            .opaq,
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

    pub const QualError =
        error{ SizeOfFunc, AlignOfFunc, SizeOfOpaque, AlignOfOpaque };

    pub fn alignOf(self: Self, rw: ReprWelt) QualError!usize {
        return switch (self) {
            .opaq => error.AlignOfOpaque,
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

    pub fn sizeOf(self: Self, rw: ReprWelt) QualError!usize {
        return switch (self) {
            .opaq => error.SizeOfOpaque,
            .func => error.SizeOfFunc,
            .unit => 0,
            .ptr => 8,
            .uint, .int, .float => |nbytes| nbytes,
            .array => |arr| arr.size * try rw.get(arr.of).sizeOf(rw),
            .coll => |fields| coll: {
                var sz: usize = 0;
                for (fields) |field| {
                    const field_sz = try rw.sizeOf(field.of);
                    sz = @max(sz, field.offset + field_sz);
                }

                break :coll com.padAlignment(sz, try self.alignOf(rw));
            },
        };
    }

    pub fn hash(self: Self, wyhash: *Wyhash) void {
        std.hash.autoHashStrat(wyhash, self, .Deep);
    }

    fn sliceEql(comptime T: type, a: []const T, b: []const T) bool {
        if (a.len != b.len) return false;

        for (a) |el, i| {
            if (!std.meta.eql(el, b[i])) return false;
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
            .opaq, .unit, .uint, .int, .float, .ptr, .array => self,
            .coll => |coll| Self{ .coll = try ally.dupe(Field, coll) },
            .func => |func| Self{
                .func = Func{
                    .ctx = func.ctx,
                    .takes = try ally.dupe(Param, func.takes),
                    .returns = func.returns,
                },
            },
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

    // TODO move this to render_repr.zig
    pub fn render(
        self: Self,
        ctx: *kz.Context,
        rw: ReprWelt,
    ) Allocator.Error!kz.Ref {
        const sty = kz.Style{ .fg = .blue };
        const offset_sty = kz.Style{ .fg = .yellow };

        return switch (self) {
            .opaq, .unit => try ctx.print(sty, "{s}", .{@tagName(self)}),
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
