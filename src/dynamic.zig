const std = @import("std");
const builtin = @import("builtin");
const parse = @import("parse.zig");
const sema = @import("sema.zig");
const Expr = @import("fluent/expr.zig");
const FlFile = @import("file.zig");
const FlValue = @import("fluent/value.zig").FlValue;

const Scope = sema.Scope;
const Allocator = std.mem.Allocator;

// TODO split up this file

const Context = struct {
    const Self = @This();

    ctx: *FlFile.Context,
    scope: *const Scope,

    ally: Allocator, // backing allocator
    arena: std.heap.ArenaAllocator, // use for memory scoped to context lifetime

    fn init(ctx: *FlFile.Context, scope: *const Scope) Self {
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
        for (FlOp.diffs.values) |diff| args = @maximum(args, diff.in);
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

        fn append(self: *Builder, block: *const FlBlock) !void {
            const constants_len = block.constants.len;
            const ops_len = self.ops.items.len;
            try self.constants.appendSlice(block.constants);
            try self.ops.appendSlice(block.ops);

            offset_constant_ops(self.ops.items[ops_len..], constants_len);
        }

        /// generates a block and clears builder
        fn to_block(self: *Builder, ally: Allocator) !FlBlock {
            const block = FlBlock{
                .constants = try ally.dupe(FlValue, self.constants.items),
                .ops = try ally.dupe(FlOp, self.ops.items),
            };

            self.constants.clearAndFree();
            self.ops.clearAndFree();

            return block;
        }
    };

    constants: []const FlValue,
    ops: []const FlOp,

    pub fn deinit(self: *Self, ally: Allocator) void {
        ally.free(self.constants);
        ally.free(self.ops);
    }

    /// allows you to generate blocks from 'assembly'
    /// syntax is simple, follows the same format as debug output: op name
    /// followed by an optional unsigned number, separated by newlines
    ///
    /// copies constant array onto the allocator
    pub fn assemble(
        ally: Allocator,
        constants: []const FlValue,
        text: []const u8
    ) !FlBlock {
        var ops = std.ArrayList(FlOp).init(ally);
        defer ops.deinit();

        var lines = std.mem.split(u8, text, "\n");
        while (lines.next()) |line| {
            var words = std.mem.tokenize(u8, line, " ");
            if (words.next()) |tag_name| {
                const tag = std.meta.stringToEnum(FlOp.Enum, tag_name).?;
                const num =
                    if (words.next()) |word|
                        try std.fmt.parseUnsigned(FlOp.Size, word, 10)
                    else
                        0;

                const op = switch(tag) {
                    .debug => FlOp{ .debug = {} },
                    .push => FlOp{ .push = num },
                    .drop => FlOp{ .drop = {} },
                    .iadd => FlOp{ .iadd = {} },
                    .isub => FlOp{ .isub = {} },
                };

                try ops.append(op);
            }
        }

        return Self{
            .constants = try ally.dupe(FlValue, constants), // TODO deepcopy?
            .ops = ops.toOwnedSlice(),
        };
    }

    /// used by append + concat
    fn offset_constant_ops(
        ops: []FlOp,
        offset: usize
    ) void {
        const offset_size = @intCast(FlOp.Size, offset);
        for (ops) |*op| {
            switch (op.*) {
                .push => op.push += offset_size,
                else => {}
            }
        }
    }

    /// creates a third block which is the concatenation of two blocks
    pub fn concat(ally: Allocator, a: *const Self, b: *const Self) !Self {
        const constants = try std.mem.concat(
            ally,
            &[_][]const FlValue{a.constants, b.constants}
        );
        var ops = try std.mem.concat(
            ally,
            &[_][]const FlOp{a.ops, b.ops}
        );

        offset_constant_ops(ops[a.ops.len], a.constants.len);

        return Self{
            .constants = constants,
            .ops = ops
        };
    }

    fn find_total_diff(self: *const Self) FlStackDiff {
        var total: isize = 0;
        var min_diff: isize = 0;

        for (self.ops) |op| {
            const diff = FlOp.diffs.get(op);
            total -= @intCast(isize, diff.in);
            min_diff = @minimum(total, min_diff);
            total += @intCast(isize, diff.out);
        }

        return FlStackDiff.init(
            @intCast(usize, -min_diff),
            @intCast(usize, total - min_diff)
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

    pub fn execute_block(self: *Self, block: *const FlBlock) !void {
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
        // function calls
        const children = expr.children.?;
        if (children.len == 0) @panic("TODO empty function call");

        for (children[1..]) |*child| try compile_expr(ctx, builder, child);

        const function = children[0];
        if (function.etype == .ident) {
            if (ctx.scope.get(function.slice)) |binding| {
                // append compiled block
                if (binding.block) |*block| {
                    try builder.append(block);
                } else {
                    @panic("TODO function binding doesn't contain a block");
                }
            } else {
                @panic("TODO didn't find function ident in scope");
            }
        } else {
            @panic("TODO called something that isn't an ident");
        }
    } else {
        @panic("TODO encountered expr I can't compile yet");
    }
}

/// returns program allocated on ally
pub fn compile(
    ally: Allocator,
    scope: *const Scope,
    lfile: *const FlFile,
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

    return try builder.to_block(ally);
}

/// evaluates an expression from start to finish. returns 'null' and prints
/// out error messages if any stage of compilation fails.
pub fn evaluate(
    ally: Allocator,
    scope: *Scope,
    name: []const u8,
    text: []const u8
) !?FlValue {
    var lfile = try FlFile.init(ally, name, text);
    defer lfile.deinit(ally);

    var ast = (try parse.parse(ally, scope, &lfile, .expr)) orelse return null;
    defer ast.deinit();

    var block =
        (try compile(ally, scope, &lfile, &ast.root)) orelse return null;
    defer block.deinit(ally);

    if (comptime builtin.mode == .Debug) {
        const diff = block.find_total_diff();
        std.debug.assert(diff.in == 0 and diff.out == 1);
    }

    var vm = FlVm.init(ally);
    defer vm.deinit();

    try vm.execute_block(&block);
    std.debug.assert(vm.stack.items.len == 1);

    return vm.stack.items[0];
}