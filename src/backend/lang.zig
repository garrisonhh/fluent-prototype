//! defining the base Fluent language.

const std = @import("std");
const fluent = @import("fluent.zig");
const plumbing = @import("../plumbing.zig");
const Env = @import("env.zig");

const Allocator = std.mem.Allocator;
const SType = fluent.SType;
const SExpr = fluent.SExpr;

/// fluent's builtin functions, used in `eval` and Env mappings
pub const Builtin = enum {
    func_type,

    iadd,
};

fn define_type(env: *Env, symbol: []const u8, stype: SType) !void {
    try env.define_value(symbol, SType{ .stype = {} }, SExpr{ .stype = stype });
}

pub fn create_global_env(ally: Allocator) !Env {
    var global = try Env.init(ally, null);

    var arena = std.heap.ArenaAllocator.init(ally);
    defer arena.deinit();
    const tmp = arena.allocator();

    try define_type(&global, "type", SType{ .stype = {} });
    try define_type(&global, "int", SType{ .int = {} });

    try global.define(
        "fn",
        Env.Bound{
            .stype = try SType.init_func(
                tmp,
                &.{
                    try SType.init_list(tmp, SType{ .stype = {} }),
                    SType{ .stype = {} }
                },
                SType{ .stype = {} }
            ),
            .data = .{ .builtin = .func_type }
        }
    );

    try global.display("global env", .{});

    return global;
}