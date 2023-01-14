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
    writer: anytype
) (Allocator.Error || @TypeOf(writer).Error)!void {
    if (!@hasDecl(@TypeOf(target), "render")) {
        @compileError("target does not have a public render function.");
    }

    var ctx = Context.init(ally);
    defer ctx.deinit();

    const ref = try target.render(&ctx, env);

    try ctx.write(ref, writer);
}
