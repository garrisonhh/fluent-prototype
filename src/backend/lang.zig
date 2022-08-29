//! defining the base Fluent language.

const std = @import("std");
const fluent = @import("fluent.zig");
const plumbing = @import("../plumbing.zig");
const Env = @import("env.zig");

const Allocator = std.mem.Allocator;
const SType = fluent.SType;
const SExpr = fluent.SExpr;

fn define_type(env: *Env, symbol: []const u8, stype: SType) !void {
    try env.define_value(symbol, SType{ .stype = {} }, SExpr{ .stype = stype });
}

pub fn create_global_env(ally: Allocator) !Env {
    var global = try Env.init(ally, null);

    try define_type(&global, "type", SType{ .stype = {} });
    try define_type(&global, "int", SType{ .int = {} });

    var type_stype = SType{ .stype = {} };
    var fn_params = [_]SType{
        SType{ .list = &type_stype },
        type_stype
    };

    try global.define(
        "fn",
        Env.Bound{
            .stype = SType{
                .func = .{
                    .params = &fn_params,
                    .returns = &type_stype
                }
            },
            .data = .{
                .builtin = {}
            }
        }
    );

    try global.display("global env", .{});

    return global;
}