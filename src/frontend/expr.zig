const std = @import("std");
const util = @import("../util/util.zig");
const context = @import("../context.zig");

const Allocator = std.mem.Allocator;

pub const Expr = struct {
    const Self = @This();

    const Tag = enum {
        number,
        string,
        symbol,

        call,

        def,
        let,
        @"fn",
        @"if",
    };

    tag: Tag,
    loc: context.Loc,
    slice: []const u8, // owned by context
    children: []Expr,
};