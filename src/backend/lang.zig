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

    const def = global.define;
    try def("type", Env.Bound{
        .stype = SType{ .stype = {} },
        .value = SExpr{ .stype = SType{ .stype = {} } }
    });
    try def("int", Env.Bound{
        .stype = SType{ .stype = {} },
        .value = SExpr{ .stype = SType{ .int = {} } }
    });

    try global.display("global env", .{});

    return global;
}