const std = @import("std");

/// a monadic result type modeled off of ocaml
pub fn Result(comptime T: type, comptime E: type) type {
    return union(enum) {
        const Self = @This();

        ok: T,
        err: E,

        pub fn ok(value: T) Self {
            return Self{ .ok = value };
        }

        pub fn err(e: E) Self {
            return Self{ .err = e };
        }

        pub fn get(self: Self) ?T {
            return if (self == .ok) self.ok else null;
        }

        pub fn getErr(self: Self) ?E {
            return if (self == .err) self.err else null;
        }

        /// given an `err`, coerce to a result of a different ok type
        pub fn cast(self: Self, comptime Tp: type) Result(Tp, E) {
            return Result(Tp, E).err(self.err);
        }

        /// given an `ok`, coerce to a result of a different err type
        pub fn castErr(self: Self, comptime Ep: type) Result(T, Ep) {
            return Result(T, Ep).ok(self.ok);
        }

        pub fn map(self: Self, f: fn(T) T) Self {
            return switch (self) {
                .ok => |val| Self.ok(f(val)),
                .err => self
            };
        }

        pub fn bind(self: Self, f: fn(T) Self) Self {
            return switch (self) {
                .ok => |val| f(val),
                .err => self
            };
        }
    };
}