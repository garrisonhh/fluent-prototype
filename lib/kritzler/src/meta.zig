const std = @import("std");
const Allocator = std.mem.Allocator;
const Context = @import("context.zig");

/// render looks for a function with the type:
/// ```zig
/// const RenderError = Allocator.Error || @TypeOf(writer).Error;
/// pub fn render(self, ctx: *kz.Context, env) RenderError!kz.Ref
/// ```
/// and prints this object to a writer.
pub fn display(
    ally: Allocator,
    env: anytype,
    target: anytype,
    writer: anytype,
) err: {
    const render_fn_type = @TypeOf(@TypeOf(target).render);
    const return_type = @typeInfo(render_fn_type).Fn.return_type.?;
    const error_set = @typeInfo(return_type).ErrorUnion.error_set;
    break :err Allocator.Error || @TypeOf(writer).Error || error_set;
}!void {
    var ctx = Context.init(ally);
    defer ctx.deinit();

    const ref = try target.render(&ctx, env);

    try ctx.write(ref, writer);
}
