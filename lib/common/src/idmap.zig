const std = @import("std");
const expect = std.testing.expect;
const Allocator = std.mem.Allocator;
const kz = @import("kritzler");

pub fn UId(comptime UniqueTag: @TypeOf(.EnumLiteral)) type {
    return packed struct(usize) {
        const Self = @This();
        const UTag = UniqueTag;

        index: usize,

        pub fn of(index: usize) Self {
            return .{ .index = index };
        }

        pub fn eql(self: Self, other: Self) bool {
            return self.index == other.index;
        }

        /// kritzler compat
        pub fn render(self: Self, ctx: *kz.Context, _: void) !kz.Ref {
            return try ctx.print(.{}, "{}", .{self});
        }

        /// std.fmt compat
        pub fn format(
            self: Self,
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) @TypeOf(writer).Error!void {
            try writer.print("{s}{}", .{ @tagName(UTag), self.index });
        }
    };
}

/// a non-moving persistent handle table implementation
///
/// Id parameter should be created using UId.
pub fn IdMap(comptime Id: type, comptime T: type) type {
    const must_deinit = comptime @hasDecl(T, "deinit");

    comptime {
        std.debug.assert(Id == UId(Id.UTag));

        const Deinit = fn (*T, Allocator) void;
        if (must_deinit and @TypeOf(T.deinit) != Deinit) {
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

        const PageList = std.SinglyLinkedList(Page);

        /// memory pool for items
        pages: PageList = .{},
        /// stores deleted ids ready for reuse
        unused: std.ArrayListUnmanaged(Id) = .{},
        /// maps id -> item
        items: std.ArrayListUnmanaged(?*T) = .{},

        pub fn deinit(self: *Self, ally: Allocator) void {
            // deinit live items
            if (must_deinit) {
                for (self.items.items) |ptr| {
                    if (ptr) |item| {
                        item.deinit(ally);
                    }
                }
            }

            // deinit pages
            while (self.pages.popFirst()) |node| {
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
                if (node.data.alloc()) |ptr| return ptr else |_| {}
            }

            // make a new page
            const node = try ally.create(PageList.Node);
            node.data = .{};

            self.pages.prepend(node);

            return node.data.alloc() catch unreachable;
        }

        /// creates an unbound id and ensures that the id's slot exists
        pub fn newId(self: *Self, ally: Allocator) Allocator.Error!Id {
            // reuse an old id if possible
            if (self.unused.popOrNull()) |id| {
                return id;
            }

            // create a new id
            const id = Id.of(self.items.items.len);
            try self.items.append(ally, null);

            return id;
        }

        /// initialize a slot
        pub fn set(
            self: *Self,
            ally: Allocator,
            id: Id,
            item: T,
        ) Allocator.Error!void {
            const slot = try self.alloc(ally);

            slot.* = item;
            self.items.items[id.index] = slot;
        }

        /// create a new id and initialize a slot
        pub fn new(self: *Self, ally: Allocator, item: T) Allocator.Error!Id {
            const id = try self.newId(ally);
            try self.set(ally, id, item);

            return id;
        }

        /// free up an id for reusage, deinitializes item if it exists
        pub fn del(self: *Self, ally: Allocator, id: Id) Allocator.Error!void {
            try self.unused.append(ally, id);

            const ptr: *?*T = &self.items.items[id.index];
            if (ptr.*) |item| {
                ptr.* = null;
                if (must_deinit) item.deinit(ally);
            }
        }

        /// retrieve an id
        pub fn getOpt(self: Self, id: Id) ?*T {
            return self.items.items[id.index];
        }

        /// retrieve an id when it must exist
        pub fn get(self: Self, id: Id) *T {
            return self.getOpt(id).?;
        }
    };
}

// tests =======================================================================

test "basic usage" {
    const ally = std.testing.allocator;

    const X = struct { x: u32 };

    var map = IdMap(.x, X){};
    defer map.deinit(ally);

    const a = try map.new(ally, .{ .x = 3 });
    const b = try map.new(ally, .{ .x = 9 });

    try expect(map.get(a).x == 3);
    try expect(map.get(b).x == 9);
}
