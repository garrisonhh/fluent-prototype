//! geometric primitives

const std = @import("std");

pub const Pos = @Vector(2, usize);
pub const Offset = @Vector(2, isize);

pub fn toOffset(pos: Pos) Offset {
    return Offset{ @intCast(isize, pos[0]), @intCast(isize, pos[1]) };
}

pub fn toPos(offset: Offset) Pos {
    return Pos{
        @intCast(usize, offset[0]),
        @intCast(usize, offset[1]),
    };
}

pub const Rect = struct {
    const Self = @This();

    offset: Offset,
    size: Pos,

    pub fn init(offset: Offset, size: Pos) Self {
        return Self{
            .offset = offset,
            .size = size,
        };
    }

    pub fn intersectionWith(self: Self, other: Self) ?Self {
        const offset = @max(self.offset, other.offset);

        const size_self = self.offset + toOffset(self.size) - offset;
        const size_other = other.offset + toOffset(other.size) - offset;
        const isect_size = @min(size_self, size_other);

        // find intersection
        if (isect_size[0] < 0 or isect_size[1] < 0) {
            return null;
        }

        return Self{
            .offset = offset,
            .size = toPos(isect_size),
        };
    }

    pub fn unionWith(self: Self, other: Self) Self {
        const offset = @min(self.offset, other.offset);

        const size_self = self.offset + toOffset(self.size) - offset;
        const size_other = other.offset + toOffset(other.size) - offset;
        const union_size = @max(size_self, size_other);

        return Self{
            .offset = offset,
            .size = toPos(union_size),
        };
    }
};
