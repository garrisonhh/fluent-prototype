const std = @import("std");
const builtin = @import("builtin");
const parse = @import("parse.zig");
const Expr = @import("ast/expr.zig");
const LispType = @import("ast/type.zig").LispType;
const LispFile = @import("file.zig");

const Allocator = std.mem.Allocator;

pub const Error = Allocator.Error;

pub const SemaContext = struct {
    const Self = @This();

    ally: Allocator,
    ctx: *LispFile.Context,
    global: *const TypeScope,

    pub fn init(ctx: *LispFile.Context, global: *const TypeScope) Self {
        return Self{
            .ally = ctx.ally,
            .ctx = ctx,
            .global = global,
        };
    }
};

pub const TypeScope = struct {
    const Self = @This();

    // maps {ident: ltype}
    map: std.StringHashMap(LispType),

    pub fn init(ally: Allocator) Self {
        return Self{
            .map = std.StringHashMap(LispType).init(ally),
        };
    }

    /// initializes a scope with all of the necessary typing builtins
    /// TODO can I do this at compile time? like with ComptimeStringMap?
    pub fn init_global(ally: Allocator) !Self {
        var global = Self.init(ally);

        // only 'ltype' and 'fn' must be created from scratch in order to
        // compile the other types (this is a mindfuck lmao)
        const ltype_ltype = LispType{ .ltype = {} };
        try global.bind("type", ltype_ltype);

        const fn_param_arr = [_]LispType{LispType.init_list(&ltype_ltype)};
        const fn_params = try ally.dupe(LispType, fn_param_arr[0..]);
        const fn_ltype = LispType.init_function(fn_params, &ltype_ltype);
        try global.bind("fn", fn_ltype);

        // TODO add other builtins
        const builtin_types = [_][2][]const u8{
            .{"int", "type"},
            .{"float", "type"},
            .{"string", "type"},
        };

        _ = builtin_types;
        // for (builtin_types) |kv| {
        //     const name = kv[0];
        //     const ltype_expr = kv[1];
        //
        //     _ = name;
        //     _ = ltype_expr;
        // }

        if (comptime builtin.mode == .Debug) {
            const canvas = @import("util/canvas.zig");
            const stderr = std.io.getStdErr().writer();

            var tc = canvas.TextCanvas.init(ally);
            defer tc.deinit();
            const tmp_ally = tc.temp_allocator();

            try tc.add_box(canvas.TextBox.init(
                .{ .x = 0, .y = -1 },
                "created global scope:",
                canvas.ConsoleColor{ .fg = .cyan }
            ));

            var entries = global.map.iterator();
            var i: usize = 0;
            while (entries.next()) |entry| : (i += 1) {
                const key = entry.key_ptr.*;
                const value = entry.value_ptr;

                // print key
                try tc.add_box(canvas.TextBox.init(
                    .{
                        .x = -@intCast(i32, key.len + 2),
                        .y = @intCast(i32, i)
                    },
                    try std.fmt.allocPrint(tmp_ally, "{s}: ", .{key}),
                    canvas.ConsoleColor{ .fmt = .bold }
                ));

                // print binding
                try tc.add_box(canvas.TextBox.init(
                    .{ .x = 0, .y = @intCast(i32, i) },
                    try std.fmt.allocPrint(tmp_ally, "{}", .{value}),
                    canvas.ConsoleColor{}
                ));
            }

            tc.print(stderr) catch unreachable;
            stderr.writeAll("\n") catch unreachable;
        }

        return global;
    }

    pub fn deinit(self: *Self) void {
        self.map.deinit();
    }

    pub fn bind(
        self: *Self,
        key: []const u8,
        value: LispType
    ) Allocator.Error!void {
        try self.map.put(key, value);
    }

    pub fn get(self: *const Self, key: []const u8) ?*const LispType {
        return if (self.map.get(key)) |value| &value else null;
    }
};

// inference is generally bottom-up, only bindings are top-down
pub fn type_check_and_infer(
    ctx: *SemaContext,
    ast: *const Expr
) Error!?LispType {
    return switch (ast.etype) {
        .file => LispType{ .nil = {} },
        .int => LispType{ .int = {} },
        .float => LispType{ .float = {} },
        .string => LispType{ .string = {} },
        .ltype => LispType{ .ltype = {} },
        .ident => infer_ident: {
            if (ctx.global.get(ast.slice)) |binding| {
                break :infer_ident try binding.clone(ctx.ally);
            } else {
                try ctx.ctx.add_message(.err, "unknown identifier", ast.slice);
                break :infer_ident null;
            }
        },
        .call => infer_call: {
            const children = ast.children.?;
            if (children.len == 0) {
                try ctx.ctx.add_message(
                    .err,
                    "function call without function",
                    ast.slice
                );
                break :infer_call null;
            }

            const fn_expr = children[0];
            if (fn_expr.ltype != .function) {
                try ctx.ctx.add_message(
                    .err,
                    "attempted to call non-function",
                    ast.slice
                );
            }

            const function = fn_expr.ltype.function;
            break :infer_call try function.returns.clone(ctx.ally);
        },
        .list => infer_list: {
            const children = ast.children.?;
            if (children.len == 0) {
                const unk = LispType{ .unknown = {} };
                break :infer_list LispType.init_list(&unk);
            }

            const fst = children[0];
            for (children[1..]) |child| {
                if (!child.ltype.eql(&fst.ltype)) {
                    const tmp_ally = ctx.ctx.temp_allocator();

                    try ctx.ctx.add_message(
                        .err,
                        "list contains mismatched types:",
                        ast.slice
                    );

                    const fst_note = try std.fmt.allocPrint(
                        tmp_ally,
                        "this is {}",
                        .{fst.ltype}
                    );
                    try ctx.ctx.add_message(.note, fst_note, fst.slice);

                    const child_note = try std.fmt.allocPrint(
                        tmp_ally,
                        "but this is {}",
                        .{child.ltype}
                    );
                    try ctx.ctx.add_message(.note, child_note, child.slice);

                    break :infer_list null;
                }
            }

            const subtype = try fst.ltype.create_clone(ctx.ally);
            break :infer_list LispType.init_list(subtype);
        }
    };
}