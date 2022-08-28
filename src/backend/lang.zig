//! defining the base Fluent language.

const std = @import("std");
const fluent = @import("fluent.zig");
const plumbing = @import("../plumbing.zig");
const Env = @import("env.zig");

const Allocator = std.mem.Allocator;
const SType = fluent.SType;
const SExpr = fluent.SExpr;

pub fn create_global_env(ally: Allocator) !Env {
    var global = try Env.init_internal(ally, null);

    const def_val = global.define_value;
    try def_val(
        "type",
        SType{ .stype = {} },
        SExpr{ .stype = SType{ .stype = {} } }
    );
    try def_val(
        "int",
        SType{ .stype = {} },
        SExpr{ .stype = SType{ .int = {} } }
    );

    try global.display("global env", .{});

    return global;
}