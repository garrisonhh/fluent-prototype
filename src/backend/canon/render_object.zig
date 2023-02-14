const std = @import("std");
const Allocator = std.mem.Allocator;
const kz = @import("kritzler");
const Object = @import("object.zig");
const Env = @import("../env.zig");

pub fn render(obj: Object, ctx: *kz.Context, env: Env) Allocator.Error!kz.Ref {
    _ = obj;
    _ = env;

    return try ctx.print(.{}, "[TODO render Object]", .{});
}
