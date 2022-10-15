//! prehashed string type. no ownership of memory, everything is comptime-able

const std = @import("std");

const Self = @This();

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
pub fn HashMap(comptime V: type) type {
    return std.HashMap(Self, V, HashMapContext, max_load);
}
pub fn HashMapUnmanaged(comptime V: type) type {
    return std.HashMapUnmanaged(Self, V, HashMapContext, max_load);
}

hash: u64,
str: []const u8,

pub fn init(str: []const u8) Self {
    return Self{
        .hash = hashStr(str),
        .str = str
    };
}

pub fn hashStr(str: []const u8) u64 {
    const SEED = comptime std.hash.Wyhash.hash(0, "Fluent");
    return std.hash.Wyhash.hash(SEED, str);
}

pub fn eql(self: Self, sym: Self) bool {
    return self.hash == sym.hash and std.mem.eql(u8, self.str, sym.str);
}

pub fn clone(self: Self, ally: std.mem.Allocator) std.mem.Allocator.Error!Self {
    return Self{
        .hash = self.hash,
        .str = try ally.dupe(u8, self.str),
    };
}