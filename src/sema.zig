const std = @import("std");
const builtin = @import("builtin");
const parse = @import("parse.zig");
const FlFile = @import("file.zig");
const Expr = @import("fluent/expr.zig");
const FlType = @import("fluent/type.zig").FlType;

const Allocator = std.mem.Allocator;

pub const Error = Allocator.Error;

pub const Context = struct {
    const Self = @This();

    // ally: Allocator,
    ctx: *FlFile.Context,
    global: *const TypeScope,

    pub fn init(ctx: *FlFile.Context, global: *const TypeScope) Self {
        return Self{
            .ctx = ctx,
            .global = global,
        };
    }
};

pub const TypeScope = struct {
    const Self = @This();

    arena: std.heap.ArenaAllocator,

    // maps {ident: ltype}
    // I think eventually I will need to store Exprs or FlValues associated with
    // idents here, but for builtins I think it will always be unnecessary
    map: std.StringHashMap(FlType),

    pub fn init(ally: Allocator) Self {
        return Self{
            .arena = std.heap.ArenaAllocator.init(ally),
            .map = std.StringHashMap(FlType).init(ally),
        };
    }

    /// initializes a scope with all of the necessary typing builtins
    /// TODO can I do this at compile time? like with ComptimeStringMap?
    pub fn init_global(ally: Allocator) !Self {
        var global = Self.init(ally);
        const scope_ally = global.allocator();

        // some types have to be created from scratch, like `type` and `fn`
        const type_ltype = FlType{ .ltype = {} };
        try global.bind("type", type_ltype);

        const fn_param_arr = [_]FlType{FlType.init_list(&type_ltype)};
        const fn_params = try scope_ally.dupe(FlType, fn_param_arr[0..]);
        const fn_ltype = FlType.init_function(fn_params, &type_ltype);
        try global.bind("fn", fn_ltype);

        // TODO add these through dynamic exec
        const int_ltype = FlType{ .int = {} };
        const add_param_arr = [_]FlType{int_ltype, int_ltype};
        const add_params = try scope_ally.dupe(FlType, add_param_arr[0..]);
        const add_ltype = FlType.init_function(add_params, &int_ltype);
        try global.bind("+", add_ltype);

        // TODO add other builtins through dynamic exec
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

    fn allocator(self: *Self) Allocator {
        return self.arena.allocator();
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
        self.map.deinit();
    }

    pub fn bind(
        self: *Self,
        key: []const u8,
        value: FlType
    ) Allocator.Error!void {
        try self.map.put(key, value);
    }

    pub fn get(self: *const Self, key: []const u8) ?*const FlType {
        return if (self.map.get(key)) |value| &value else null;
    }
};

// inference is generally bottom-up, only bindings are top-down
pub fn type_check_and_infer(
    ctx: *Context,
    ast: *parse.Ast,
    expr: *const Expr
) Error!?FlType {
    const ast_ally = ast.allocator();
    const msg_ally = ctx.ctx.temp_allocator();

    return switch (expr.etype) {
        .file => FlType{ .nil = {} },
        .int => FlType{ .int = {} },
        .float => FlType{ .float = {} },
        .string => FlType{ .string = {} },
        .ltype => FlType{ .ltype = {} },
        .ident => infer_ident: {
            if (ctx.global.get(expr.slice)) |binding| {
                break :infer_ident try binding.clone(ast_ally);
            } else {
                try ctx.ctx.add_message(.err, "unknown identifier", expr.slice);
                break :infer_ident null;
            }
        },
        .call => infer_call: {
            const children = expr.children.?;
            if (children.len == 0) {
                try ctx.ctx.add_message(
                    .err,
                    "function call without function",
                    expr.slice
                );
                break :infer_call null;
            }

            const fn_expr = children[0];
            if (fn_expr.ltype != .function) {
                try ctx.ctx.add_message(
                    .err,
                    "attempted to call non-function",
                    expr.slice
                );
                break :infer_call null;
            }

            // type check parameters
            var bad_params = false;
            const function = fn_expr.ltype.function;
            const params = children[1..];

            var i: usize = 0;
            while (i < @minimum(function.params.len, params.len)) : (i += 1) {
                const expected = &function.params[i];
                const actual = &params[i].ltype;
                if (!actual.eql(expected)) {
                    bad_params = true;

                    const msg = try std.fmt.allocPrint(
                        msg_ally,
                        "wrong parameter type: expected {}, found {}",
                        .{expected, actual}
                    );
                    try ctx.ctx.add_message(.err, msg, params[i].slice);
                }
            }

            if (params.len != function.params.len) {
                bad_params = true;

                const cmp_text =
                    if (params.len > function.params.len) "many"
                    else "few";
                const msg = try std.fmt.allocPrint(
                    msg_ally,
                    "too {s} parameters: expected {d}, found {d}",
                    .{cmp_text, function.params.len, params.len}
                );
                try ctx.ctx.add_message(.err, msg, expr.slice);
            }

            if (bad_params) break :infer_call null;

            break :infer_call try function.returns.clone(ast_ally);
        },
        .list => infer_list: {
            const children = expr.children.?;
            if (children.len == 0) {
                const unk = FlType{ .unknown = {} };
                break :infer_list FlType.init_list(&unk);
            }

            const fst = children[0];
            for (children[1..]) |child| {
                if (!child.ltype.eql(&fst.ltype)) {
                    try ctx.ctx.add_message(
                        .err,
                        "list contains mismatched types:",
                        expr.slice
                    );

                    const fst_note = try std.fmt.allocPrint(
                        msg_ally,
                        "this is {}",
                        .{fst.ltype}
                    );
                    try ctx.ctx.add_message(.note, fst_note, fst.slice);

                    const child_note = try std.fmt.allocPrint(
                        msg_ally,
                        "but this is {}",
                        .{child.ltype}
                    );
                    try ctx.ctx.add_message(.note, child_note, child.slice);

                    break :infer_list null;
                }
            }

            const subtype = try fst.ltype.create_clone(ast_ally);
            break :infer_list FlType.init_list(subtype);
        }
    };
}