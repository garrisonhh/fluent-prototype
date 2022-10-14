//! prehashed string type. no ownership of memory, everything is comptime-able

const std = @import("std");

const Self = @This();

pub const Hash = struct { value: u64 };

hash: Hash,
str: []const u8,

pub fn init(str: []const u8) Self {
    return Self{
        .hash = hash(str),
        .str = str
    };
}

pub fn hash(str: []const u8) Hash {
    const SEED = comptime std.hash.Wyhash.hash(0, "Fluent");
    return Hash{ .value = std.hash.Wyhash.hash(SEED, str) };
}

pub fn eql(self: Self, sym: Self) bool {
    return self.hash == sym.hash and std.mem.eql(u8, self.str, sym.str);
}