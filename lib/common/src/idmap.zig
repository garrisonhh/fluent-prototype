const std = @import("std");
const expect = std.testing.expect;
const Allocator = std.mem.Allocator;

/// a non-moving handle table implementation.
pub fn IdMap(comptime T: type) type {
    comptime {
        const Deinit = fn (*T, Allocator) void;
        if (@TypeOf(T.deinit) != Deinit) {
            const msg = std.fmt.comptimePrint(
                "IdMap expects a T with a deinit function of type `{}`",
                .{Deinit},
            );
            @compileError(msg);
        }
    }

    return struct {
        const PAGE_SIZE = 1024;

        const Self = @This();

        pub const Id = struct {
            index: usize,

            pub fn of(index: usize) Id {
                return .{ .index = index };
            }
        };

        const Page = struct {
            mem: [PAGE_SIZE]T = undefined,
            cap: usize = 0,

            fn alloc(self: *Page) Allocator.Error!*T {
                if (self.cap >= PAGE_SIZE) {
                    return Allocator.Error.OutOfMemory;
                }

                const ptr = &self.mem[self.cap];
                self.cap += 1;

                return ptr;
            }
        };

        const PageList = std.SinglyLinkedList(*Page);

        const Slot = struct {
            id: Id,
            ptr: *T,
        };

        /// memory pool for items
        pages: PageList = .{},
        /// stores deleted ids ready for reuse
        unused: std.ArrayListUnmanaged(Slot) = .{},
        /// maps id -> item
        items: std.ArrayListUnmanaged(?*T) = .{},

        pub fn deinit(self: *Self, ally: Allocator) void {
            // deinit live items
            for (self.items.items) |ptr| {
                if (ptr) |item| {
                    item.deinit(ally);
                }
            }

            // deinit pages
            while (self.pages.popFirst()) |node| {
                ally.destroy(node.data);
                ally.destroy(node);
            }

            self.unused.deinit(ally);
            self.items.deinit(ally);
        }

        /// allocate a new T in the memory pool
        fn alloc(self: *Self, ally: Allocator) Allocator.Error!*T {
            // use current page
            const head = self.pages.first;
            if (head) |node| {
                if (node.data.alloc()) |ptr| {
                    return ptr;
                } else |_| {
                    // continue
                }
            }

            // make a new page
            const page = try ally.create(Page);
            page.* = .{};

            const node = try ally.create(PageList.Node);
            node.data = page;

            self.pages.prepend(node);

            return page.alloc() catch unreachable;
        }

        /// place an item in the refmap and return a new id
        pub fn new(self: *Self, ally: Allocator, item: T) Allocator.Error!Id {
            // reuse an unused id
            if (self.unused.popOrNull()) |slot| {
                std.debug.assert(self.items.items[slot.id.index] == null);
                self.items.items[slot.id.index] = slot.ptr;
                slot.ptr.* = item;
                return slot.id;
            }

            // allocate a new id
            const ptr = try self.alloc(ally);
            ptr.* = item;

            const id = Id.of(self.items.items.len);
            try self.items.append(ally, ptr);

            return id;
        }

        /// free up an id for reusage
        pub fn del(self: *Self, ally: Allocator, id: Id) Allocator.Error!void {
            const slot = Slot{
                .id = id,
                .ptr = self.get(id),
            };

            try self.unused.append(ally, slot);
            slot.ptr.deinit(ally);
            self.items.items[id.index] = null;
        }

        /// retrieve an id
        pub fn get(self: Self, id: Id) *T {
            // if this fails, this id was deleted
            std.debug.assert(self.items.items[id.index] != null);
            return self.items.items[id.index].?;
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

    var map = IdMap(X){};
    defer map.deinit(ally);

    const a = try map.new(ally, .{ .x = 3 });
    const b = try map.new(ally, .{ .x = 9 });

    try expect(map.get(a).x == 3);
    try expect(map.get(b).x == 9);
}
