const std = @import("std");
const builtin = @import("builtin");
const util = @import("util/util.zig");
const parse = @import("parse.zig");
const sema = @import("sema.zig");
const Expr = @import("fluent/expr.zig");
const FlFile = @import("file.zig");
const FlValue = @import("fluent/value.zig").FlValue;
const FlType = @import("fluent/type.zig").FlType;

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
/// TODO modify this to include FlValue.Enum values to type check FlBlocks
pub const FlStackDiff = struct {
    const Self = @This();

    in: usize,
    out: usize,

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
            .initFill(.{ .in = 0, .out = 0 });

        const set = (struct {
            fn add_diff(tag: Enum, in: usize, out: usize) void {
                map.set(tag, .{ .in = in, .out = out });
            }
        }).add_diff;

        set(.push, 0, 1);
        set(.drop, 1, 0);
        set(.swap, 2, 2);

        set(.alloc, 1, 1);
        set(.list_set, 3, 2);
        set(.list_get, 2, 3);

        set(.iinc, 1, 1);
        set(.idec, 1, 1);
        set(.iadd, 2, 1);
        set(.isub, 2, 1);

        set(.fn_type, 2, 1);
        set(.list_type, 1, 1);

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

    // stack manipulation
    push: Size, // push an indexed const on the stack
    drop, // discard a value
    swap, // (a, b -> b, a)

    // memory manipulation (TODO raw pointers?)
    alloc, // pop int, alloc an empty list of that size
    list_set, // sets list value (list, int, val -> list, int)
    list_get, // gets list value (list, int -> list, int, val)

    // math
    iinc,
    idec,

    iadd,
    isub,

    // types
    fn_type,
    list_type,

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

        fn append(self: *Builder, block: *const FlBlock) Allocator.Error!void {
            const constants_len = self.constants.items.len;
            const ops_len = self.ops.items.len;
            try self.constants.appendSlice(block.constants);
            try self.ops.appendSlice(block.ops);

            offset_constant_ops(self.ops.items[ops_len..], constants_len);
        }

        fn append_assembly(self: *Builder, comptime text: []const u8) !void {
            try self.ops.appendSlice(FlBlock.assemble_ops(text));
        }

        /// generates a block by copying current contents of builder
        fn to_block(self: *Builder, ally: Allocator) Allocator.Error!FlBlock {
            return FlBlock{
                .constants = try ally.dupe(FlValue, self.constants.items),
                .ops = try ally.dupe(FlOp, self.ops.items),
            };
        }

        fn clear(self: *Builder) void {
            self.constants.clearAndFree();
            self.ops.clearAndFree();
        }
    };

    // constants relies on unchanging FlValues. never make these mutable
    constants: []const FlValue,
    ops: []const FlOp,

    pub fn deinit(self: *Self, ally: Allocator) void {
        ally.free(self.constants);
        ally.free(self.ops);
    }

    pub fn clone(self: *const Self, ally: Allocator) !Self {
        return Self{
            .constants = try ally.dupe(FlValue, self.constants),
            .ops = try ally.dupe(FlOp, self.ops)
        };
    }

    /// assembles ops at comptime
    pub fn assemble_ops(comptime text: []const u8) []FlOp {
        return comptime blk: {
            // allocate buffer
            const buf_size = std.mem.count(u8, text, "\n") + 1;
            var buf: [buf_size]FlOp = undefined;

            // fill buffer
            var i: usize = 0;
            var lines = std.mem.split(u8, text, "\n");
            while (lines.next()) |line| {
                var words = std.mem.tokenize(u8, line, " ");
                if (words.next()) |tag_name| {
                    if (words.next()) |num_tok| {
                        // tag with data
                        const n =
                            try std.fmt.parseUnsigned(FlOp.Size, num_tok, 10);

                        buf[i] = @unionInit(FlOp, tag_name, n);
                        i += 1;
                    } else {
                        // tag without data
                        buf[i] = @unionInit(FlOp, tag_name, {});
                        i += 1;
                    }
                }
            }

            break :blk buf[0..i];
        };
    }

    /// allows you to generate blocks from 'assembly'
    /// syntax is simple, follows the same format as debug output: op name
    /// followed by an optional unsigned number, separated by newlines
    ///
    /// copies constant array onto the allocator
    pub fn assemble(
        ally: Allocator,
        constants: []const FlValue,
        comptime text: []const u8
    ) !FlBlock {
        return Self{
            .constants = try ally.dupe(FlValue, constants),
            .ops = try ally.dupe(FlOp, FlBlock.assemble_ops(text)),
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

        return .{
            .in = @intCast(usize, -min_diff),
            .out = @intCast(usize, total - min_diff)
        };
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

    ally: Allocator,
    stack: std.ArrayList(FlValue),

    pub fn init(ally: Allocator) Self {
        return Self{
            .ally = ally,
            .stack = std.ArrayList(FlValue).init(ally),
        };
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit();
    }

    fn push(self: *Self, value: FlValue) !void {
        try self.stack.append(value);
    }

    fn push_clone(self: *Self, value: FlValue) !void {
        try self.push(try value.clone(self.ally));
    }

    fn push_clone_slice(self: *Self, values: []const FlValue) !void {
        for (values) |value| try self.push_clone(value);
    }

    fn execute_op(
        self: *Self,
        block: *const FlBlock,
        popped: []const FlValue,
        op: FlOp
    ) !void {
        switch (op) {
            .debug => self.debug(),
            .push => |constant| try self.push(block.constants[constant]),
            .drop => {},
            .swap => {
                try self.push_clone(popped[1]);
                try self.push_clone(popped[0]);
            },
            .alloc => {
                const n = @intCast(usize, popped[0].int);
                try self.push(try FlValue.init_list(self.ally, n));
            },
            .list_set => {
                const list = popped[0].list;
                const index = @intCast(usize, popped[1].int);
                list[index] = try popped[2].clone(self.ally);

                // TODO some understanding of retaining values in FlStackDiff
                // would prevent a lot of memory allocs + copying
                try self.push_clone_slice(popped[0..2]);
            },
            .list_get => {
                const list = popped[0].list;
                const index = @intCast(usize, popped[1].int);

                try self.push_clone_slice(popped[0..2]);
                try self.push_clone(list[index]);
            },
            .iinc => try self.push(.{ .int = popped[0].int + 1}),
            .idec => try self.push(.{ .int = popped[0].int - 1}),
            .iadd => try self.push(.{ .int = popped[0].int + popped[1].int }),
            .isub => try self.push(.{ .int = popped[0].int - popped[1].int }),
            .fn_type => {
                const param_vals = popped[0].list;
                const params = try self.ally.alloc(FlType, param_vals.len);
                defer self.ally.free(params);

                for (param_vals) |val, i| params[i] = val.ltype;

                try self.push(FlValue{
                    .ltype = try FlType.init_function(
                        self.ally,
                        params,
                        &popped[1].ltype
                    )
                });
            },
            .list_type => @panic("TODO list_type")
        }
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

            defer for (popped[0..to_pop]) |*val| val.deinit(self.ally);

            // perform operation
            try @call(
                .{ .modifier = .always_inline },
                execute_op,
                .{self, block, popped[0..to_pop], op}
            );
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
    const constants = &builder.constants;

    switch (expr.etype) {
        .int, .float, .string => {
            // easy literals
            const index = constants.items.len;
            const value = try FlValue.from_literal(expr);
            try constants.append(value);
            try builder.ops.append(FlOp{ .push = @intCast(u32, index) });
        },
        .ident => {
            const binding = ctx.scope.get(expr.slice) orelse {
                @panic("TODO didn't find ident in scope");
            };
            try builder.append(&binding.block);
        },
        .call => {
            const children = expr.children.?;
            if (children.len == 0) @panic("TODO empty function call");

            for (children[1..]) |*child| try compile_expr(ctx, builder, child);
            try compile_expr(ctx, builder, &children[0]);
        },
        .list => {
            const children = expr.children.?;

            // allocate list and push 0 to start a loop
            try builder.append(&FlBlock{
                .constants = &[_]FlValue{
                    FlValue{ .int = @intCast(i64, children.len) },
                    FlValue{ .int = 0 },
                },
                .ops = FlBlock.assemble_ops(
                    \\ push 0
                    \\ alloc
                    \\ push 1
                )
            });

            // set each child
            for (children) |*child| {
                try compile_expr(ctx, builder, child);
                try builder.append_assembly(
                    \\ list_set
                    \\ iinc
                );
            }

            // drop index
            try builder.append_assembly("drop");
        },
        else => @panic("TODO encountered expr I can't compile yet")
    }
}

/// compiles an ast to an FlBlock allocated on ally
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

/// compiles an ast and runs it
pub fn compile_run(
    ally: Allocator,
    scope: *const Scope,
    lfile: *const FlFile,
    ast: *const Expr
) !?FlValue {
    var block =
        (try compile(ally, scope, lfile, ast)) orelse return null;
    defer block.deinit(ally);

    if (comptime builtin.mode == .Debug) {
        std.debug.print("compiled block:\n", .{});
        block.debug();
    }

    if (comptime builtin.mode == .Debug) {
        const diff = block.find_total_diff();
        std.debug.assert(diff.in == 0 and diff.out == 1);
    }

    var vm = FlVm.init(ally);
    defer vm.deinit();

    try vm.execute_block(&block);
    std.debug.assert(vm.stack.items.len == 1);

    return try vm.stack.items[0].clone(ally);
}

/// evaluates an expression from start to finish. returns 'null' and prints
/// out error messages if any stage of compilation fails.
pub fn eval(
    ally: Allocator,
    scope: *Scope,
    name: []const u8,
    text: []const u8
) !?FlValue {
    var lfile = try FlFile.init(ally, name, text);
    defer lfile.deinit(ally);

    var ast = (try parse.parse(ally, scope, &lfile, .expr)) orelse return null;
    defer ast.deinit();

    return compile_run(ally, scope, &lfile, &ast.root);
}

/// panics on eval error. only for usage inside the compiler.
pub fn internal_eval(
    ally: Allocator,
    scope: *Scope,
    comptime name: []const u8,
    comptime text: []const u8
) !FlValue {
    return (try eval(ally, scope, name, text)).?;
}