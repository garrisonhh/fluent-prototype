//! manages paths through some kind of virtual tree and mapping values to them.

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const Symbol = @import("symbol.zig");

pub const NameError = Allocator.Error || error { NameTooLong, NameRedef };

/// names represent keys into a namemap. operations with names are generally
/// done through their namemap.
///
/// names own their symbol list but none of the symbols. all symbols are
/// owned by the namemap.
pub const Name = struct {
    const Self = @This();

    pub const MAX_LENGTH = 128;

    pub const ROOT = root: {
        const syms: []Symbol = &.{};
        break :root Self{
            .syms = syms,
            .hash = hashSyms(syms),
        };
    };

    syms: []Symbol,
    hash: u64,

    fn hashSyms(syms: []const Symbol) u64 {
        var wyhash = std.hash.Wyhash.init(0);
        for (syms) |sym| {
            wyhash.update(std.mem.asBytes(&sym.hash));
        }

        return wyhash.final();
    }

    fn initOwned(syms: []const Symbol) NameError!Self {
        if (syms.len > MAX_LENGTH) {
            return error.NameTooLong;
        }

        return Self{
            .syms = syms,
            .hash = hashSyms(syms),
        };
    }

    fn init(ally: Allocator, syms: []const Symbol) NameError!Self {
        return try Self.initOwned(try ally.dupe(Symbol, syms));
    }

    fn initAppend(ally: Allocator, ns: Name, sym: Symbol) NameError!Self {
        const syms = try ally.alloc(Symbol, ns.syms.len + 1);
        std.mem.copy(Symbol, syms, ns.syms);
        syms[syms.len - 1] = sym;

        return try Self.initOwned(syms);
    }

    fn deinit(self: Self, ally: Allocator) void {
        ally.free(self.syms);
    }

    pub fn eql(self: Self, other: Self) bool {
        if (self.hash != other.hash) return false;

        for (self.syms) |sym, i| {
            if (!sym.eql(other.syms[i])) {
                return false;
            }
        }

        return true;
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        _ = fmt;
        _ = options;

        for (self.syms) |sym, i| {
            if (i > 0) try writer.writeAll("::");
            try writer.writeAll(sym.str);
        }
    }

    const HashMapContext = struct {
        pub fn hash(ctx: @This(), key: Self) u64 {
            _ = ctx;
            return key.hash;
        }

        pub fn eql(ctx: @This(), a: Self, b: Self) bool {
            _ = ctx;
            return a.eql(b);
        }
    };

    const max_load = std.hash_map.default_max_load_percentage;

    // fn HashMap(comptime V: type) type {
        // return std.HashMap(Self, V, HashMapContext, max_load);
    // }

    fn HashMapUnmanaged(comptime V: type) type {
        return std.HashMapUnmanaged(Self, V, HashMapContext, max_load);
    }
};

pub fn NameMap(comptime V: type) type {
    return struct {
        const Self = @This();

        /// set of owned symbols
        syms: Symbol.HashMapUnmanaged(void) = .{},
        /// set of owned names
        names: Name.HashMapUnmanaged(void) = .{},
        /// maps names to values
        map: Name.HashMapUnmanaged(V) = .{},

        pub fn deinit(self: *Self, ally: Allocator) void {
            // deinit syms
            var syms = self.syms.keyIterator();
            while (syms.next()) |sym| {
                ally.free(sym.str);
            }

            self.syms.deinit(ally);

            // deinit names
            var names = self.names.keyIterator();
            while (names.next()) |name| {
                name.deinit(ally);
            }

            self.names.deinit(ally);
            self.map.deinit(ally);
        }

        /// given an unowned symbol, gets an owned symbol mapped to the symbol
        /// set
        fn acquireSym(
            self: *Self,
            ally: Allocator,
            sym: Symbol
        ) NameError!Symbol {
            const res = try self.syms.getOrPut(ally, sym);
            if (!res.found_existing) {
                res.key_ptr.* = try sym.clone(ally);
            }
            return res.key_ptr.*;
        }

        /// puts a value into the table and returns its unique name
        ///
        /// given an owned name and an unowned symbol, create a new name which
        /// represents this symbol inside the name as a namespace
        pub fn put(
            self: *Self,
            ally: Allocator,
            ns: Name,
            sym: Symbol,
            value: V
        ) NameError!Name {
            const owned = try self.acquireSym(ally, sym);
            const name = try Name.initAppend(ally, ns, owned);

            const res = try self.names.getOrPut(ally, name);
            if (res.found_existing) {
                return error.NameRedef;
            } else {
                res.key_ptr.* = name;
            }

            try self.map.put(ally, name, value);

            return res.key_ptr.*;
        }

        pub fn get(self: *Self, name: Name) V {
            // since `put` is the only way to acquire a name, `get` should never
            // be able to fail
            return self.map.get(name).?;
        }

        pub fn getWithin(self: *Self, ns: Name, sym: Symbol) ?V {
            var buf: [Name.MAX_LENGTH]Symbol = undefined;
            std.mem.copy(Symbol);
            buf[ns.syms.len] = sym;

            var syms = buf[0..ns.syms.len + 1];
            while (syms.len > 0) {
                const name = Name.initOwned(syms);

                if (self.map.get(name)) |value| {
                    return value;
                }

                // iter
                const new_len = syms.len - 1;
                syms = syms[0..new_len];
                syms[new_len - 1] = sym;
            }

            // none found
            return null;
        }
    };
}

test "random namemap" {
    const ally = std.testing.allocator;

    var rng = std.rand.DefaultPrng.init(1);
    const random = rng.random();

    const V = i32;
    const KV = struct { k: Name, v: V };

    var kvs = std.ArrayList(KV).init(ally);
    defer kvs.deinit();

    var nmap = NameMap(V){};
    defer nmap.deinit(ally);

    try kvs.append(.{ .k = Name.root, .v = 0 });

    var i: usize = 0;
    while (i < 20) : (i += 1) {
        // random sym
        var buf: [5]u8 = undefined;
        for (buf) |*ch| {
            ch.* = random.intRangeAtMost(u8, 'a', 'z');
        }

        const sym = Symbol.init(&buf);

        // random ns
        const index = random.intRangeLessThan(usize, 0, kvs.items.len);
        const ns = kvs.items[index].k;

        // create key + random value
        const k = try nmap.into(ally, ns, sym);
        const v = random.int(V);

        // store
        if (!nmap.map.contains(k)) {
            try nmap.put(ally, k, v);
            try kvs.append(.{ .k = k, .v = v });
        }
    }

    for (kvs.items[1..]) |pair| {
        try std.testing.expectEqual(pair.v, nmap.get(pair.k));
    }
}