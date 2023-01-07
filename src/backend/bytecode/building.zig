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

    const BackRef = union(enum) {
        // what a Pos resolved to
        resolved: InstRef,
        // instructions to resolve once this Pos is resolved
        unresolved: std.ArrayListUnmanaged(InstRef),
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

    pub fn deinit(self: *Self, ally: Allocator) void {
        self.program.deinit(ally);
        self.refs.deinit(ally);
        self.regions.deinit(ally);
    }

    /// returns an executable view' into the builder
    pub fn build(self: *Self, func: FuncRef) Program {
        if (builtin.mode == .Debug) {
            // validate backrefs are completed
            var entries = self.refs.iterator();
            while (entries.next()) |entry| {
                if (entry.value_ptr.* == .unresolved) {
                    const sp = entry.key_ptr;
                    const label = sp.block.index;

                    std.debug.panic(
                        "bytecode compilation failed to resolve {}@{}",
                        .{func.index, label}
                    );
                }
            }
        }

        // get function backref
        const backref = self.refs.get(Pos.ofEntry(func)).?;

        return Program{
            .entry = backref.resolved.index,
            .program = self.program.items,
        };
    }

    /// for jumps + backrefs
    pub fn here(self: Self) InstRef {
        return InstRef.of(@intCast(u32, self.program.items.len));
    }

    pub fn append(
        self: Self,
        ally: Allocator,
        other: Self
    ) Allocator.Error!void {
        _ = self;
        _ = ally;
        _ = other;

        @compileError("TODO append builders");
    }

    /// branching instructions expect a position to branch to. this effectively
    /// handles adding the position with any required backreferencing, and hides
    /// the internal backreference impl
    fn addBranch(self: *Self, ally: Allocator, sp: Pos) Allocator.Error!void {
        const res = try self.refs.getOrPut(ally, sp);
        if (res.found_existing) {
            // retrieve resolved ref or add unresolved ref
            switch (res.value_ptr.*) {
                .resolved => |ref| {
                    try self.addInst(ally, Inst.fromInt(ref.index));
                },
                .unresolved => |*list| {
                    try list.append(ally, self.here());
                    try self.addInst(ally, Inst.fromInt(0));
                },
            }
        } else {
            // add unresolved backref
            res.value_ptr.* = BackRef{ .unresolved = .{} };
            try res.value_ptr.unresolved.append(ally, self.here());
            try self.addInst(ally, Inst.fromInt(0));
        }
    }

    /// resolve a backreference to a real instref
    pub fn resolve(
        self: *Self,
        ally: Allocator,
        sp: Pos,
        to: InstRef
    ) Allocator.Error!void {
        const res = try self.refs.getOrPut(ally, sp);
        if (res.found_existing) {
            // can't resolve the same Pos twice
            std.debug.assert(res.value_ptr.* == .unresolved);

            // resolve backrefs
            const list = &res.value_ptr.unresolved;
            defer list.deinit(ally);

            for (list.items) |ref| {
                self.program.items[ref.index] = Inst.fromInt(to.index);
            }
        }

        // keep resolved value
        res.value_ptr.* = BackRef{ .resolved = to };
    }

    pub fn addInst(
        self: *Self,
        ally: Allocator,
        inst: Inst
    ) Allocator.Error!void {
        try self.program.append(ally, inst);
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
