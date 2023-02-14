const std = @import("std");
const expect = std.testing.expect;
const Allocator = std.mem.Allocator;
const log = std.log.scoped(.idmap);

/// creates a unique, labeled Id type for usage in an IdMap.
pub fn UniqueId(comptime scope: @TypeOf(.enum_literal)) type {
    return struct {
        const Self = @This();

        page: u32,
        index: u32,

        pub fn eql(self: Self, other: Self) bool {
            return std.meta.eql(self, other);
        }

        pub fn format(
            self: Self,
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) @TypeOf(writer).Error!void {
            try writer.print(
                "[{s}@{x}:{x}]",
                .{ @tagName(scope), self.page, self.index },
            );
        }
    };
}

/// used for indexing Chalkboard
const Bit = struct {
    const Self = @This();

    index: usize,

    const ROOT = Self.of(1);

    fn of(index: usize) Self {
        std.debug.assert(index != 0);
        return Self{ .index = index };
    }

    fn isRoot(self: Self) bool {
        return self.index == 1;
    }

    fn parent(self: Self) Self {
        return Self.of(self.index >> 1);
    }

    fn left(self: Self) Self {
        return Self.of(self.index << 1);
    }

    fn right(self: Self) Self {
        return Self.of((self.index << 1) | 1);
    }

    fn sibling(self: Self) Self {
        return Self.of(self.index ^ 1);
    }
};

/// binary tree for mapping into page
fn Chalkboard(comptime LEAVES: usize) type {
    comptime {
        // LEAVES must be a power of 2
        std.debug.assert(@popCount(LEAVES) == 1);
    }

    return struct {
        const Self = @This();

        const Tree = std.StaticBitSet(LEAVES * 2);

        tree: Tree = Tree.initEmpty(),

        fn isFull(self: Self) bool {
            return self.get(Bit.ROOT);
        }

        fn get(self: Self, bit: Bit) bool {
            return self.tree.isSet(bit.index);
        }

        fn set(self: *Self, bit: Bit) void {
            self.tree.set(bit.index);
        }

        fn unset(self: *Self, bit: Bit) void {
            self.tree.unset(bit.index);
        }

        fn state(self: Self, index: usize) bool {
            return self.get(Bit.of(LEAVES + index));
        }

        /// mark a leaf as used and propagate
        fn markUsed(self: *Self, index: usize) void {
            var bit = Bit.of(LEAVES + index);
            std.debug.assert(self.get(bit) == false);

            while (true) : (bit = bit.parent()) {
                self.set(bit);
                if (bit.isRoot() or !self.get(bit.sibling())) break;
            }
        }

        /// mark a leaf as free and propagate
        fn markFree(self: *Self, index: usize) void {
            var bit = Bit.of(LEAVES + index);
            std.debug.assert(self.get(bit) == true);

            while (true) : (bit = bit.parent()) {
                self.unset(bit);
                if (bit.isRoot()) break;
            }
        }

        /// get first free slot, set it to used, return index
        fn alloc(self: *Self) ?usize {
            if (self.isFull()) return null;

            var bit = Bit.ROOT;
            while (bit.index < LEAVES) {
                const left = bit.left();
                bit = if (self.get(left)) bit.right() else left;
            }

            self.set(bit);

            return bit.index - LEAVES;
        }

        /// given the last known capacity (highest index allocated in page),
        /// tries to shrink this value
        fn recalcCap(self: Self, prev_cap: usize) usize {
            var index = prev_cap;
            while (index > 0) : (index -= 1) {
                const bit = Bit.of(LEAVES + index - 1);
                if (self.get(bit)) break;
            }

            return index;
        }
    };
}

/// manages memory by:
/// 1. bump allocating when possible
/// 2. reusing unused slots when bump is unavailable
fn Page(comptime T: type, comptime Id: type, comptime PAGE_SIZE: usize) type {
    return struct {
        const Self = @This();

        const Slot = struct {
            index: usize,
            ptr: *T,
        };

        index: usize,
        mem: [PAGE_SIZE]T = undefined,
        cap: usize = 0,
        cb: Chalkboard(PAGE_SIZE) = .{},

        fn alloc(self: *Self) Allocator.Error!Id {
            const index = if (self.cap < self.mem.len) bump: {
                // bump alloc
                defer self.cap += 1;
                break :bump self.cap;
            } else if (self.cb.alloc()) |index| reuse: {
                // reuse memory
                break :reuse index;
            } else {
                return error.OutOfMemory;
            };

            self.cb.markUsed(index);

            return Id{
                .page = @intCast(u32, self.index),
                .index = @intCast(u32, index),
            };
        }

        fn free(self: *Self, id: Id) void {
            std.debug.assert(id.page == self.index);
            self.cb.markFree(id.index);
            self.cap = self.cb.recalcCap(self.cap);
        }

        fn isAlive(self: *const Self, id: Id) bool {
            std.debug.assert(id.page == self.index);
            return self.cb.state(id.index);
        }
    };
}

/// a non-moving handle table implementation.
pub fn IdMap(comptime T: type, comptime Id: type) type {
    comptime {
        const Deinit = fn (*T, Allocator) void;
        if (@TypeOf(T.deinit) != Deinit) {
            const msg = "IdMap expects a T with a `deinit(*T, Allocator) void`";
            @compileError(msg);
        }
    }

    return struct {
        const Self = @This();

        const PAGE_SIZE = 1 << 16;
        const P = Page(T, Id, PAGE_SIZE);

        pages: std.ArrayListUnmanaged(*P) = .{},

        pub fn deinit(self: *Self, ally: Allocator) void {
            for (self.pages.items) |page| ally.destroy(page);
            self.pages.deinit(ally);
        }

        /// allocate a new T in the memory pool
        fn alloc(self: *Self, ally: Allocator) Allocator.Error!Id {
            // find an existing page
            var i = self.pages.items.len;
            while (i > 0) {
                i -= 1;

                const page = self.pages.items[i];
                if (page.alloc()) |id| {
                    return id;
                } else |_| {
                    // ignore error
                }
            }

            // make a new page
            const page = try ally.create(P);
            page.* = .{ .index = self.pages.items.len };
            try self.pages.append(ally, page);

            return page.alloc();
        }

        /// place an item in the map and return a new id
        pub fn new(self: *Self, ally: Allocator, item: T) Allocator.Error!Id {
            const id = try self.alloc(ally);
            const ptr = self.get(id);
            ptr.* = item;

            log.debug("new {}", .{id});

            return id;
        }

        /// free up an id for reusage
        pub fn del(self: *Self, ally: Allocator, id: Id) void {
            self.get(id).deinit(ally);
            self.pages.items[id.page].free(id);

            log.debug("del {}", .{id});
        }

        /// get without checking liveness
        fn rawGet(self: Self, id: Id) *T {
            return &self.pages.items[id.page].mem[id.index];
        }

        /// retrieve an id
        pub fn get(self: Self, id: Id) *T {
            log.debug("get {}", .{id});

            const page = self.pages.items[id.page];
            std.debug.assert(page.isAlive(id));
            return &page.mem[id.index];
        }

        /// safely moves data from one id to another, deleting the first id.
        pub fn squash(self: *Self, ally: Allocator, dst: Id, src: Id) void {
            log.debug("squash {} <- {}", .{ dst, src });

            const dst_page = self.pages.items[dst.page];
            const src_page = self.pages.items[src.page];

            dst_page.mem[dst.index].deinit(ally);
            dst_page.mem[dst.index] = src_page.mem[src.index];
            src_page.cb.markFree(src.index);
        }
    };
}

const X = struct {
    const Self = @This();

    x: u32,

    fn deinit(_: *Self, _: Allocator) void {}
};

test "basic usage" {
    const ally = std.testing.allocator;

    const Id = UniqueId(.basic);
    var map = IdMap(X, Id){};
    defer map.deinit(ally);

    const a = try map.new(ally, .{ .x = 3 });
    const b = try map.new(ally, .{ .x = 9 });

    try expect(map.get(a).x == 3);
    try expect(map.get(b).x == 9);
}
