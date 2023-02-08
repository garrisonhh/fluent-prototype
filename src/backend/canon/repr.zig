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

    pub const Array = struct {
        size: usize,
        of: ReprId,
    };

    const Self = @This();

    // TODO move this
    pub const U8 = Self{ .uint = 1 };
    pub const U16 = Self{ .uint = 2 };
    pub const U32 = Self{ .uint = 4 };
    pub const U64 = Self{ .uint = 8 };
    pub const I8 = Self{ .int = 1 };
    pub const I16 = Self{ .int = 2 };
    pub const I32 = Self{ .int = 4 };
    pub const I64 = Self{ .int = 8 };
    pub const F32 = Self{ .float = 4 };
    pub const F64 = Self{ .float = 8 };

    // hold byte width (MUST be 1, 2, 4, or 8)
    uint: u4,
    int: u4,
    float: u4,
    // structured data
    ptr: ReprId,
    array: Array,
    coll: []ReprId,

    /// dupes input
    pub fn initColl(
        ally: Allocator,
        coll: []const ReprId,
    ) Allocator.Error!Self {
        return Self{ .coll = try ally.dupe(ReprId, coll) };
    }

    pub fn deinit(self: Self, ally: Allocator) void {
        switch (self) {
            .uint, .int, .float, .ptr, .array => {},
            .coll => |coll| ally.free(coll),
        }
    }

    pub fn ofType(
        ally: Allocator,
        rw: *ReprWelt,
        tw: TypeWelt,
        id: TypeId,
    ) ReprWelt.ConvertError!Self {
        const ty = tw.get(id);

        // analysis types have no repr
        if (ty.classifyRuntime(tw) == .analysis) {
            return ReprWelt.ConvertError.AnalysisType;
        }

        return switch (ty.*) {
            .hole,
            .namespace,
            .any,
            .set,
            => return ReprWelt.ConvertError.AnalysisType,
            .func => return ReprWelt.ConvertError.UnknownConversion,
            .unit => Self{ .coll = &.{} },
            .@"bool" => U8,
            .atom, .ty, .builtin => U64,
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
                    const u64_id = rw.reprs.get(U64).?;
                    const ptr_repr = try rw.intern(ally, Self{
                        .ptr = try rw.reprOf(ally, tw, ptr.to),
                    });

                    break :slice try Self.initColl(
                        ally,
                        &.{ ptr_repr, u64_id },
                    );
                },
            },
            else => |tag| std.debug.panic("TODO convert repr of {}", .{tag}),
        };
    }

    pub fn alignOf(self: Self, rw: ReprWelt) usize {
        return switch (self) {
            .uint, .int, .float => |nbytes| nbytes,
            .ptr => 8,
            .array => |arr| rw.get(arr.of).alignOf(rw),
            .coll => |coll| coll: {
                var max_aln: usize = 1;
                for (coll) |child| {
                    const child_aln = rw.get(child).alignOf(rw);
                    max_aln = @max(max_aln, child_aln);
                }

                break :coll max_aln;
            },
        };
    }

    pub fn sizeOf(self: Self, rw: ReprWelt) usize {
        return switch (self) {
            .uint, .int, .float => |nbytes| nbytes,
            .ptr => 8,
            .array => |arr| arr.size * rw.get(arr.of).sizeOf(rw),
            .coll => |coll| coll: {
                if (coll.len == 0) break :coll 0;

                var size: usize = 0;
                for (coll) |field| {
                    const repr = rw.get(field);

                    // ensure field is on an aligned boundary
                    const aln = repr.alignOf(rw);
                    const aln_diff = size % aln;
                    if (aln_diff > 0) {
                        size += aln - aln_diff;
                    }

                    size += repr.sizeOf(rw);
                }

                break :coll size;
            },
        };
    }

    pub fn hash(self: Self, wyhash: *Wyhash) void {
        const b = std.mem.asBytes;

        wyhash.update(b(&@as(Tag, self)));

        switch (self) {
            .int, .uint, .float => |nbytes| wyhash.update(b(&nbytes)),
            .ptr => |to| wyhash.update(b(&to)),
            .array => |arr| {
                wyhash.update(b(&arr.size));
                wyhash.update(b(&arr.of));
            },
            .coll => |coll| wyhash.update(b(&coll)),
        }
    }

    pub fn eql(self: Self, other: Self) bool {
        return @as(Tag, self) == @as(Tag, other) and switch (self) {
            inline .int,
            .uint,
            .float,
            => |nbytes, tag| nbytes == @field(other, @tagName(tag)),
            .ptr => |id| id.eql(other.ptr),
            .array => |arr| arr.size == other.array.size and
                arr.of.eql(other.array.of),
            .coll => |coll| coll: {
                if (coll.len != other.coll.len) {
                    break :coll false;
                }

                for (coll) |child, i| {
                    if (!child.eql(other.coll[i])) {
                        break :coll false;
                    }
                }

                break :coll true;
            },
        };
    }

    pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
        return switch (self) {
            .uint, .int, .float, .ptr, .array => self,
            .coll => |coll| try initColl(ally, coll),
        };
    }

    pub fn isStructured(self: Self) bool {
        return switch (self) {
            .uint, .int, .float, .ptr => false,
            .array, .coll => true,
        };
    }

    pub fn render(
        self: Self,
        ctx: *kz.Context,
        rw: ReprWelt,
    ) Allocator.Error!kz.Ref {
        const sty = kz.Style{ .fg = .blue };

        return switch (self) {
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

                for (coll) |elem, i| {
                    list[i] = try elem.render(ctx, rw);
                }

                break :coll try ctx.stack(&.{
                    try ctx.print(.{}, "(", .{}),
                    try ctx.sep(
                        try ctx.print(.{}, ", ", .{}),
                        list,
                        .right,
                        .{},
                    ),
                    try ctx.print(.{}, ")", .{}),
                }, .right, .{});
            },
        };
    }
};
