const std = @import("std");
const sema = @import("sema.zig");
const Expr = @import("fluent/expr.zig");
const FlFile = @import("file.zig");
const FlValue = @import("fluent/value.zig").FlValue;

const Allocator = std.mem.Allocator;

const Context = struct {
    const Self = @This();

    ctx: *FlFile.Context,
    scope: *const sema.TypeScope,

    ally: Allocator, // backing allocator
    arena: std.heap.ArenaAllocator, // use for memory scoped to context lifetime

    fn init(ctx: *FlFile.Context, scope: *const sema.TypeScope) Self {
        var ally = ctx.ally;
        return Self{
            .ctx = ctx,
            .scope = scope,
            .ally = ally,
            .arena = std.heap.ArenaAllocator.init(ally),
        };
    }

    fn deinit(self: *Self) void {
        self.arena.deinit();
    }
};

const FlOp = union(enum) {
    const Self = @This();

    comptime {
        std.debug.assert(@sizeOf(Self) <= 8);
    }

    // push an indexed const on the stack
    push: u32,
    // discard a value
    // TODO pop: u32,

    // math
    add,

    // push inst addr onto call stack and jump
    // TODO call: usize
    // pop inst addr from call stack
    // TODO ret,

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        _ = fmt;
        _ = options;

        // TODO can prob do this easier with reflection
        switch (self) {
            .push => |d| try writer.print("{s} {d}", .{@tagName(self), d}),
            .add => try writer.print("{s}", .{@tagName(self)}),
        }
    }
};

pub const FlProgram = struct {
    const Self = @This();

    const Builder = struct {
        constants: std.ArrayList(FlValue),
        ops: std.ArrayList(FlOp),

        fn init(ctx: *Context) Builder {
            return Builder{
                .constants = std.ArrayList(FlValue).init(ctx.ally),
                .ops = std.ArrayList(FlOp).init(ctx.ally),
            };
        }

        fn deinit(self: *Builder) void {
            self.constants.deinit();
            self.ops.deinit();
        }

        fn to_program(self: *Builder, ctx: *Context) Self {
            // TODO prune repeating constants

            return Self{
                .ally = ctx.ally,
                .constants = self.constants.toOwnedSlice(),
                .ops = self.ops.toOwnedSlice(),
            };
        }
    };

    ally: Allocator,
    constants: []const FlValue,
    ops: []const FlOp,

    /// `init` with Builder.to_program
    pub fn deinit(self: *Self) void {
        self.ally.free(self.constants);
        self.ally.free(self.ops);
    }

    pub fn debug(self: *const Self) void {
        // TODO canvas version of this
        std.debug.print("FlProgram:\n", .{});

        std.debug.print("constants:\n", .{});
        for (self.constants) |*constant, i| {
            std.debug.print("{d:4}: {}\n", .{i, constant});
        }

        std.debug.print("ops:\n", .{});
        for (self.ops) |*op, i| {
            std.debug.print("{d:4}: {}\n", .{i, op});
        }

        std.debug.print("\n", .{});
    }
};

const build_builtin_fn = fn(*Context, *FlProgram.Builder) anyerror!void;
const builtin_fns = std.ComptimeStringMap(build_builtin_fn, .{
    .{"+", compile_add}
});

fn compile_add(ctx: *Context, builder: *FlProgram.Builder) !void {
    _ = ctx;
    try builder.ops.append(FlOp{ .add = {} });
}

fn compile_expr(
    ctx: *Context,
    builder: *FlProgram.Builder,
    expr: *const Expr
) anyerror!void {
    _ = ctx;

    if (expr.is_flat_literal()) {
        // easy literals like int, float, string etc
        const index = builder.constants.items.len;
        try builder.constants.append(try FlValue.from_literal(expr));
        try builder.ops.append(FlOp{ .push = @intCast(u32, index) });
    } else if (expr.etype == .call) {
        const children = expr.children.?;
        if (children.len == 0) @panic("TODO");

        for (children[1..]) |*child| try compile_expr(ctx, builder, child);

        const function = children[0];
        if (function.etype == .ident) {
            if (builtin_fns.get(function.slice)) |compile_fn| {
                try compile_fn(ctx, builder);
            } else {
                @panic("TODO ???");
            }
        } else {
            @panic("TODO ???");
        }
    } else {
        @panic("TODO");
    }
}

/// returns program allocated on context arena
pub fn compile(
    ally: Allocator,
    lfile: *const FlFile,
    scope: *const sema.TypeScope,
    ast: *const Expr
) !?FlProgram {
    var lfile_ctx = FlFile.Context.init(ally, lfile);
    defer lfile_ctx.deinit();

    var ctx = Context.init(&lfile_ctx, scope);
    defer ctx.deinit();

    var builder = FlProgram.Builder.init(&ctx);
    defer builder.deinit();

    try compile_expr(&ctx, &builder, ast);

    if (lfile_ctx.err) {
        try lfile_ctx.print_messages();
        return null;
    }

    return builder.to_program(&ctx);
}