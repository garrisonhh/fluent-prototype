const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const ssa = @import("../ssa/ssa.zig");
const FuncRef = ssa.FuncRef;
const Label = ssa.Label;
const Pos = ssa.Pos;
const bytecode = @import("bytecode.zig");
const Inst = bytecode.Inst;
const InstRef = bytecode.InstRef;
const Program = bytecode.Program;

/// a persistent data structure use in the Env to build the program over time.
pub const Builder = struct {
    const Self = @This();

    pub const Comments = std.AutoHashMapUnmanaged(
        InstRef,
        std.ArrayListUnmanaged([]u8),
    );

    const BackRef = struct {
        // what a Pos resolved to
        ref: ?InstRef = null,
        // locations of instructions mapped to this backref
        assoc: std.ArrayListUnmanaged(InstRef) = .{},
    };

    const Region = struct {
        start: InstRef,
        stop: InstRef,
    };

    program: std.ArrayListAlignedUnmanaged(Inst, 16) = .{},
    // tracks backreferences while compiling
    refs: std.AutoHashMapUnmanaged(Pos, BackRef) = .{},
    // tracks slices of the program
    regions: std.AutoHashMapUnmanaged(FuncRef, Region) = .{},
    comments: Comments = .{},

    pub fn deinit(self: *Self, ally: Allocator) void {
        self.program.deinit(ally);
        self.refs.deinit(ally);
        self.regions.deinit(ally);

        var comments = self.comments.valueIterator();
        while (comments.next()) |list| {
            for (list.items) |str| ally.free(str);
            list.deinit(ally);
        }
        self.comments.deinit(ally);
    }

    /// returns an executable view' into the builder
    pub fn build(self: *Self, func: FuncRef) Program {
        self.finalize();

        return Program{
            .entry = self.getResolved(Pos.ofEntry(func)).index,
            .program = self.program.items,
        };
    }

    /// for jumps + backrefs
    pub fn here(self: Self) InstRef {
        return InstRef.of(@intCast(u32, self.program.items.len));
    }

    /// contract is that finalize has been called
    pub fn getResolved(self: Self, sp: Pos) InstRef {
        return self.refs.get(sp).?.ref.?;
    }

    pub fn append(
        self: *Self,
        ally: Allocator,
        other: Self
    ) Allocator.Error!void {
        const offset = @intCast(u32, self.program.items.len);

        // add insts, offsetting all instrefs
        try self.program.appendSlice(ally, other.program.items);

        // merge refs
        var entries = other.refs.iterator();
        while (entries.next()) |entry| {
            var resolved = entry.value_ptr.ref;
            if (resolved) |*r| r.index += offset;

            // add to my entry
            const backref = try self.getBackref(ally, entry.key_ptr.*);
            backref.ref = backref.ref orelse resolved;

            for (entry.value_ptr.assoc.items) |ref| {
                const updated = InstRef.of(ref.index + offset);
                try backref.assoc.append(ally, updated);
            }
        }

        // add regions
        var regions = other.regions.iterator();
        while (regions.next()) |entry| {
            const region = entry.value_ptr;
            try self.regions.put(ally, entry.key_ptr.*, Region{
                .start = InstRef.of(region.start.index + offset),
                .stop = InstRef.of(region.stop.index + offset),
            });
        }

        // add comments
        var comments = other.comments.iterator();
        while (comments.next()) |entry| {
            const ref = InstRef.of(entry.key_ptr.index + offset);
            for (entry.value_ptr.items) |comment| {
                try self.addCommentAt(ally, ref, "{s}", .{comment});
            }
        }
    }

    fn getBackref(
        self: *Self,
        ally: Allocator,
        sp: Pos
    ) Allocator.Error!*BackRef {
        const res = try self.refs.getOrPut(ally, sp);
        if (!res.found_existing) {
            res.value_ptr.* = .{};
        }

        return res.value_ptr;
    }

    /// branching instructions expect a position to branch to. this effectively
    /// handles adding the position with any required backreferencing, and hides
    /// the internal backreference impl
    pub fn addBranch(
        self: *Self,
        ally: Allocator,
        sp: Pos
    ) Allocator.Error!void {
        const backref = try self.getBackref(ally, sp);
        try backref.assoc.append(ally, self.here());
        try self.addInst(ally, Inst.fromInt(0));
    }

    //// resolve a backreference to a real instref
    pub fn resolve(
        self: *Self,
        ally: Allocator,
        sp: Pos,
        to: InstRef
    ) Allocator.Error!void {
        const backref = try self.getBackref(ally, sp);
        backref.ref = to;
    }

    /// does the actual backref resolution
    pub fn finalize(self: *Self) void {
        var entries = self.refs.iterator();
        while (entries.next()) |entry| {
            const assoc = entry.value_ptr.assoc.items;
            const resolved = entry.value_ptr.ref orelse {
                // if it's not currently known, don't finalize. this may happen
                // when a backref references something in a different builder
                // that this builder will be appended to.
                continue;
            };

            for (assoc) |ref| {
                self.program.items[ref.index] = Inst.fromInt(resolved.index);
            }
        }
    }

    pub fn addInst(
        self: *Self,
        ally: Allocator,
        inst: Inst
    ) Allocator.Error!void {
        try self.program.append(ally, inst);
    }

    pub fn addCommentAt(
        self: *Self,
        ally: Allocator,
        ref: InstRef,
        comptime fmt: []const u8,
        args: anytype
    ) Allocator.Error!void {
        const res = try self.comments.getOrPut(ally, ref);
        if (!res.found_existing) {
            res.value_ptr.* = .{};
        }

        const msg = try std.fmt.allocPrint(ally, fmt, args);
        try res.value_ptr.append(ally, msg);
    }

    pub fn addComment(
        self: *Self,
        ally: Allocator,
        comptime fmt: []const u8,
        args: anytype
    ) Allocator.Error!void {
        try self.addCommentAt(ally, self.here(), fmt, args);
    }

    pub fn addRegion(
        self: *Self,
        ally: Allocator,
        func: FuncRef,
        start: InstRef,
        stop: InstRef
    ) Allocator.Error!void {
        std.debug.assert(start.index < stop.index);
        try self.regions.put(ally, func, Region{
            .start = start,
            .stop = stop
        });
    }

    /// removes instructions associated with a function. this is very useful
    /// for `eval` to not clutter the env's builder.
    ///
    /// this operation is relatively slow, O(n) several times. performance here
    /// is probably a useful addition. that being said, the main use case for
    /// this is removing anon exprs for the repl, which should end up at
    /// the end of the builder's memory and thus O(1) removal in most cases.
    pub fn removeFunc(
        self: *Self,
        ally: Allocator,
        ref: FuncRef
    ) Allocator.Error!void {
        // remove the function's associated region
        const region = self.regions.get(ref).?;
        const start = region.start.index;
        const len = region.stop.index - start;

        try self.program.replaceRange(ally, start, len, &.{});

        // remove all backrefs within the function region
        var backrefs = self.refs.valueIterator();
        var to_remove = std.ArrayList(usize).init(ally);
        defer to_remove.deinit();

        while (backrefs.next()) |backref| {
            for (backref.assoc.items) |iref, i| {
                if (iref.index >= start and iref.index < region.stop.index) {
                    try to_remove.append(i);
                }
            }

            while (to_remove.popOrNull()) |i| {
                _ = backref.assoc.swapRemove(i);
            }
        }

        // collect + remove comments within the region
        var comms = std.ArrayList(InstRef).init(ally);
        defer comms.deinit();

        var comm_refs = self.comments.keyIterator();
        while (comm_refs.next()) |iref| {
            if (iref.index >= start and iref.index < region.stop.index) {
                try comms.append(iref.*);
            }
        }

        for (comms.items) |iref| {
            const list = self.comments.getPtr(iref).?;
            for (list.items) |str| ally.free(str);
            list.deinit(ally);
            _ = self.comments.remove(iref);
        }

        // collect + remove ssa positions with the funcref
        var positions = std.ArrayList(Pos).init(ally);
        defer positions.deinit();

        var keys = self.refs.keyIterator();
        while (keys.next()) |key| {
            if (key.ref.eql(ref)) {
                try positions.append(key.*);
            }
        }

        for (positions.items) |pos| {
            _ = self.refs.remove(pos);
        }
    }
};
