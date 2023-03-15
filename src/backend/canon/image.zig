const std = @import("std");
const expectEqual = std.testing.expectEqual;
const Allocator = std.mem.Allocator;
const com = @import("common");
const Success = com.Success;

pub const AllocError = error{ OutOfMemory, TooManyBytes };

pub const Section = enum(u8) {
    stack,
    heap,
    /// read-only memory at eval-time
    static,
    /// relative address to function frame
    frame,
};

const UIntPtr = usize;
pub const Ptr = packed struct(UIntPtr) {
    /// a type that is a uint that is (UIntPtr bits) - (section bits) wide
    const Offset = @Type(.{
        .Int = .{
            .signedness = .unsigned,
            .bits = @bitSizeOf(UIntPtr) - @bitSizeOf(Section),
        },
    });

    offset: Offset,
    section: Section,
};

/// an indexable buddy allocator for fluent Ptrs.
const Pager = struct {
    const PAGE_POW = 16; // 64 KiB
    const PAGE_SIZE = 1 << PAGE_POW;

    const Page = struct {
        const Mem = [PAGE_SIZE]u8;

        /// fat pointer metadata
        const Block = struct {
            // if this breaks, alignment for allocations breaks
            comptime {
                std.debug.assert(@alignOf(Block) >= 8);
            }

            next: ?*Block,
        };

        /// each list contains free segments of 2 ** index size
        unused: [PAGE_POW + 1]?*Block = [1]?*Block{null} ** (PAGE_POW + 1),
        mem: *align(8) Mem,

        fn init(ally: Allocator) Allocator.Error!Page {
            const slice = try ally.alignedAlloc(u8, 8, PAGE_SIZE);
            var self = Page{ .mem = @ptrCast(*align(8) Mem, slice.ptr) };

            // set up first block
            const block = @ptrCast(*Block, self.mem);
            block.* = .{ .next = null };

            self.unused[PAGE_POW] = block;

            return self;
        }

        fn deinit(self: Page, ally: Allocator) void {
            ally.free(@as([]u8, self.mem));
        }

        /// which size block based on nbytes required
        fn blockPow(nbytes: usize) u6 {
            const size = nbytes + @sizeOf(Block);

            if (@popCount(size) == 1) {
                return @intCast(u6, @ctz(size));
            } else {
                return @intCast(u6, @bitSizeOf(usize) - @clz(size));
            }
        }

        /// split a block at a size. assumes this block exists.
        fn splitBlock(self: *Page, pow: u6) void {
            const new_pow = pow - 1;
            const new_size = @as(usize, 1) << new_pow;

            // pop block
            const block = self.unused[pow].?;
            self.unused[pow] = block.next;
            block.next = null;

            // create second block
            const new_block = @intToPtr(*Block, @ptrToInt(block) + new_size);
            new_block.* = .{ .next = null };

            // prepend both blocks to the next list
            new_block.next = self.unused[new_pow];
            block.next = new_block;
            self.unused[new_pow] = block;
        }

        /// split blocks until you have a block at the correct power
        fn splitFor(self: *Page, pow: u6) Success {
            var i: u6 = pow + 1;
            return while (i < self.unused.len) : (i += 1) {
                if (self.unused[i] != null) {
                    // found a bigger block, split it up
                    while (i > pow) : (i -= 1) {
                        self.splitBlock(i);
                    }

                    break .success;
                }
            } else .failure;
        }

        fn getFreeBlock(self: *Page, pow: u6) ?*Block {
            // if a unused block of the correct size is not available, try to
            // create one
            if (self.unused[pow] == null and
                self.splitFor(pow) == .failure)
            {
                // no blocks available :(
                return null;
            }

            // pop a unused block
            const block = self.unused[pow].?;
            self.unused[pow] = block.next;

            return block;
        }

        fn alloc(self: *Page, nbytes: usize) AllocError!?Ptr.Offset {
            // get block power, check that it fits
            const pow = blockPow(nbytes);
            if (pow > PAGE_POW) return AllocError.TooManyBytes;

            // get an unused block
            const block = self.getFreeBlock(pow) orelse {
                return null;
            };

            // get ptr as an offset
            const base = @ptrToInt(self.mem);
            const addr = @ptrToInt(block) + @sizeOf(Block) - base;
            return @intCast(Ptr.Offset, addr);
        }

        /// attempt to merge the first block with any other blocks at a power
        fn mergeFirst(self: *Page, pow: u6) Success {
            const diff = @as(usize, 1) << pow;

            const fst = self.unused[pow].?;

            var trav = fst.next;
            var prev = fst;
            while (trav) |got| : ({
                prev = got;
                trav = got.next;
            }) {
                // attempt to see if these blocks can be merged
                const fst_addr = @ptrToInt(fst);
                const trav_addr = @ptrToInt(got);

                var block: ?*Block = null;
                if (fst_addr > trav_addr and
                    fst_addr - trav_addr == diff)
                {
                    block = got;
                } else if (trav_addr > fst_addr and
                    trav_addr - fst_addr == diff)
                {
                    block = fst;
                }

                // blocks could be merged, merge them
                // (I don't have to check if pow > PAGE_POW because you will
                // never have two PAGE_POW sized blocks)
                if (block) |found| {
                    // remove inner block
                    prev.next = got.next;

                    // pop first block
                    self.unused[pow] = fst.next;

                    // push merged block
                    const merged_pow = pow + 1;
                    found.next = self.unused[merged_pow];
                    self.unused[merged_pow] = found;
                }
            }

            return .failure;
        }

        /// merge all blocks possible at a given power up.
        fn mergeFor(self: *Page, pow: u6) void {
            var cur = pow;
            while (cur <= PAGE_POW and
                self.mergeFirst(pow) == .success)
            {
                cur += 1;
            }
        }

        fn free(self: *Page, ptr: Ptr.Offset, len: usize) void {
            // get ptr to fat data
            const base = @ptrToInt(self.mem);
            const addr = @as(usize, ptr) - @sizeOf(Block) + base;
            const block = @intToPtr(*Block, addr);

            // adds a block back onto the unused list
            const pow = blockPow(len);
            block.next = self.unused[pow];
            self.unused[pow] = block;

            // try to merge this list
            self.mergeFor(pow);
        }

        /// number of bytes allocated
        fn allocated(self: Page) usize {
            var total: usize = 0;
            for (self.unused) |head, pow| {
                const size = @as(usize, 1) << @intCast(u6, pow);

                var trav = head;
                while (trav) |block| : (trav = block.next) {
                    total += size;
                }
            }

            return PAGE_SIZE - total;
        }
    };

    pages: std.ArrayListUnmanaged(Page) = .{},

    fn deinit(self: *Pager, ally: Allocator) void {
        for (self.pages.items) |page| page.deinit(ally);
        self.pages.deinit(ally);
    }

    fn allocFromPage(
        self: *Pager,
        nbytes: usize,
        index: usize,
    ) AllocError!?Ptr.Offset {
        const page = &self.pages.items[index];
        const page_offset = (try page.alloc(nbytes)) orelse {
            return null;
        };
        return @intCast(Ptr.Offset, PAGE_SIZE * index) + page_offset;
    }

    fn alloc(
        self: *Pager,
        ally: Allocator,
        nbytes: usize,
    ) AllocError!Ptr.Offset {
        // attempt to allocate from an existing page
        var i = self.pages.items.len;
        while (i > 0) {
            i -= 1;
            if (try self.allocFromPage(nbytes, i)) |offset| {
                return offset;
            }
        }

        // create a new page and allocate from it
        try self.pages.append(ally, try Page.init(ally));
        return (try self.allocFromPage(nbytes, self.pages.items.len - 1)).?;
    }

    fn free(self: *Pager, ptr: Ptr.Offset, len: usize) void {
        // find page and local offset, free from it
        const page_index = ptr / PAGE_SIZE;
        const offset = ptr % PAGE_SIZE;
        self.pages.items[page_index].free(offset, len);
    }

    const Raw = *align(8) anyopaque;

    fn get(self: *const Pager, ptr: Ptr.Offset) Raw {
        const page_index = ptr / PAGE_SIZE;
        const offset = ptr % PAGE_SIZE;

        const rawptr = &self.pages.items[page_index].mem[offset];
        return @ptrCast(Raw, @alignCast(8, rawptr));
    }

    /// number of bytes allocated
    fn allocated(self: Pager) usize {
        var total: usize = 0;
        for (self.pages.items) |page| {
            total += page.allocated();
        }

        return total;
    }

    /// debugging function
    fn dump(self: Pager) void {
        std.debug.print("[[pager]]\n", .{});
        for (self.pages.items) |page, i| {
            std.debug.print("[page {}]\n", .{i});

            for (page.unused) |head, pow| {
                const size = @as(usize, 1) << @intCast(u6, pow);

                var trav = head;
                var n: usize = 0;
                while (trav) |block| : (trav = block.next) {
                    n += 1;
                }

                if (n > 0) {
                    std.debug.print("{}B: {}\n", .{ size, n });
                }
            }

            const total = page.allocated();
            std.debug.print(
                "{} allocated ({} free)\n",
                .{ total, PAGE_SIZE - total },
            );
        }
        std.debug.print("\n", .{});
    }
};

const Self = @This();

const SectionMap = std.EnumArray(Section, Pager);

ally: Allocator,
sections: SectionMap = SectionMap.initFill(Pager{}),

pub fn init(ally: Allocator) Self {
    return Self{ .ally = ally };
}

pub fn deinit(self: *Self) void {
    for (self.sections.values) |*pager| pager.deinit(self.ally);
}

pub fn alloc(self: *Self, section: Section, nbytes: usize) AllocError!Ptr {
    const pager = self.sections.getPtr(section);
    const offset = try pager.alloc(self.ally, nbytes);

    return Ptr{
        .offset = offset,
        .section = section,
    };
}

pub fn free(self: *Self, ptr: Ptr, nbytes: usize) void {
    const pager = self.sections.getPtr(ptr.section);
    pager.free(ptr.offset, nbytes);
}

pub const RawPtr = Pager.Raw;

pub fn raw(self: Self, ptr: Ptr) RawPtr {
    const pager = self.sections.getPtrConst(ptr.section);
    return pager.get(ptr.offset);
}

pub fn into(self: Self, ptr: Ptr, comptime P: type) P {
    return @ptrCast(P, self.raw(ptr));
}

// tests =======================================================================

test "image" {
    const ally = std.testing.allocator;
    const stderr = std.io.getStdErr().writer();

    try stderr.writeAll("\n");

    var img = Self.init(ally);
    defer img.deinit();

    const ptr = try img.alloc(.static, @sizeOf(u64));
    defer img.free(ptr, @sizeOf(u64));
    const n = img.into(ptr, *u64);

    n.* = 420;
}

test "image-pager" {
    const ally = std.testing.allocator;
    const stderr = std.io.getStdErr().writer();

    try stderr.writeAll("\n");

    var pager = Pager{};
    defer pager.deinit(ally);

    const mem0 = try pager.alloc(ally, 16);
    try expectEqual(@as(usize, 32), pager.allocated());

    const mem1 = try pager.alloc(ally, 32);
    try expectEqual(@as(usize, 96), pager.allocated());

    pager.free(mem0, 16);
    try expectEqual(@as(usize, 64), pager.allocated());

    pager.free(mem1, 32);
    try expectEqual(@as(usize, 0), pager.allocated());
}
