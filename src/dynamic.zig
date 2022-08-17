const std = @import("std");
const builtin = @import("builtin");
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

/// used to represent in/out value number requirements for stack ops
pub const FlStackDiff = struct {
    const Self = @This();

    in: usize,
    out: usize,

    fn init(in: usize, out: usize) Self {
        return Self{
            .in = in,
            .out = out,
        };
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        _ = fmt;
        _ = options;

        try writer.print("{} -> {}", .{self.in, self.out});
    }
};

// TODO an FlOp assembler
const FlOp = union(enum) {
    const Self = @This();
    const Enum = @typeInfo(Self).Union.tag_type.?;

    // used in place of usize in order to keep @sizeof(FlOp) <= 8 bytes
    const Size = u32;

    // I want this type to stay a 64-bit word
    comptime {
        std.debug.assert(@sizeOf(Self) <= 8);
    }

    // can use these arrays to check stack size
    const diffs = blk: {
        var map = std.EnumArray(Enum, FlStackDiff)
            .initFill(FlStackDiff.init(0, 0));

        map.set(.push, FlStackDiff.init(0, 1));
        map.set(.drop, FlStackDiff.init(1, 0));

        map.set(.iadd, FlStackDiff.init(2, 1));
        map.set(.isub, FlStackDiff.init(2, 1));

        break :blk map;
    };
    // maximum input args of all FlOps
    const max_args = blk: {
        var args: usize = 0;
        for (FlOp.diffs.values) |diff|  args = @maximum(args, diff.in);
        break :blk args;
    };

    // print the stack
    debug,

    // direct stack manipulation
    push: Size, // push an indexed const on the stack
    drop, // discard a value

    // math
    iadd,
    isub,

    // TODO call: Size, // push inst addr onto call stack and jump
    // TODO ret, // pop inst addr from call stack



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
            else => try writer.print("{s}", .{@tagName(self)}),
        }
    }
};

/// the structured representation of a fluent program compiled for dynamic
/// execution
pub const FlBlock = struct {
    // TODO being able to combining multiple blocks together would make
    // compiling functions super easy I think

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

        /// generates a block and clears builder
        fn to_block(self: *Builder, ctx: *Context) Self {
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

    fn find_total_diff(self: *const Self) FlStackDiff {
        var total: isize = 0;
        var min_diff: isize = 0;
        for (self.ops) |op| {
            const diff = FlOp.diffs.get(op);
            total += @intCast(isize, diff.out);
            total -= @intCast(isize, diff.in);
            min_diff = @minimum(total, min_diff);
        }

        return FlStackDiff.init(
            @intCast(usize, -min_diff),
            @intCast(usize, total)
        );
    }

    pub fn debug(self: *const Self) void {
        // TODO canvas version of this
        std.debug.print("FlBlock (diff {}):\n", .{self.find_total_diff()});

        std.debug.print("constants:\n", .{});
        for (self.constants) |*constant, i| {
            std.debug.print(
                "{d:4}: {}\n",
                .{i, constant.fmt(.{ .typed = true })}
            );
        }

        std.debug.print("ops:\n", .{});
        for (self.ops) |*op, i| {
            std.debug.print("{d:4}: {}\n", .{i, op});
        }

        std.debug.print("\n", .{});
    }
};

pub const FlVm = struct {
    const Self = @This();

    stack: std.ArrayList(FlValue),

    pub fn init(ally: Allocator) Self {
        return Self{
            .stack = std.ArrayList(FlValue).init(ally),
        };
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit();
    }

    fn push(self: *Self, value: FlValue) !void {
        try self.stack.append(value);
    }

    pub fn exec(self: *Self, block: *const FlBlock) !void {
        if (comptime builtin.mode == .Debug) {
            const diff = block.find_total_diff();

            if (self.stack.items.len < @intCast(usize, diff.in)) {
                return error.StackTooSmallForBlock;
            }
        }

        var popped: [FlOp.max_args]FlValue = undefined;
        var i: usize = 0;
        while (i < block.ops.len) : (i += 1) {
            const op = block.ops[i];

            // store popped values in popped ptr (slight optimization over
            // individual `pop()`s
            const to_pop = FlOp.diffs.get(op).in;
            const popped_size = self.stack.items.len - to_pop;
            std.mem.copy(FlValue, &popped, self.stack.items[popped_size..]);
            try self.stack.resize(popped_size);

            // perform operation
            switch (op) {
                .debug => self.debug(),
                .push => |constant| try self.push(block.constants[constant]),
                .drop => try self.stack.resize(self.stack.items.len - 1),
                .iadd => try self.push(FlValue{
                    .int = popped[0].int + popped[1].int
                }),
                .isub => try self.push(FlValue{
                    .int = popped[0].int - popped[1].int
                }),
            }
        }
    }

    // TODO canvas version
    pub fn debug(self: *const Self) void {
        std.debug.print("FlVm state:\n", .{});
        for (self.stack.items) |value, i| {
            std.debug.print("{d:4}: {}\n", .{i, value.fmt(.{ .typed = true })});
        }

        std.debug.print("\n", .{});
    }
};

// TODO get rid of this and somehow integrate this information with scopes,
// probably by assembling stack vm code and storing that alongside the
// function type information
const build_builtin_fn = fn(*Context, *FlBlock.Builder) anyerror!void;
const builtin_fns = std.ComptimeStringMap(build_builtin_fn, .{
    .{"+", emit_fn(&[_]FlOp{FlOp{ .iadd = {} }})},
});

// generates a function which simply emits the ops given
fn emit_fn(comptime ops: []const FlOp) build_builtin_fn {
    const Closure = struct {
        pub fn emit_ops(
            ctx: *Context,
            builder: *FlBlock.Builder
        ) anyerror!void {
            _ = ctx;
            for (ops) |op| {
                try builder.ops.append(op);
            }
        }
    };
    return Closure.emit_ops;
}

fn compile_expr(
    ctx: *Context,
    builder: *FlBlock.Builder,
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
) !?FlBlock {
    var lfile_ctx = FlFile.Context.init(ally, lfile);
    defer lfile_ctx.deinit();

    var ctx = Context.init(&lfile_ctx, scope);
    defer ctx.deinit();

    var builder = FlBlock.Builder.init(&ctx);
    defer builder.deinit();

    try compile_expr(&ctx, &builder, ast);

    if (lfile_ctx.err) {
        try lfile_ctx.print_messages();
        return null;
    }

    return builder.to_block(&ctx);
}