const std = @import("std");
const builtin = @import("builtin");
const util = @import("util/util.zig");
const fluent = @import("fluent.zig");
const backend = @import("backend.zig");
const frontend = @import("frontend.zig");
const FlFile = @import("file.zig");

const Allocator = std.mem.Allocator;
const Context = FlFile.Context;
const FlType = fluent.FlType;
const FlValue = fluent.FlValue;
const FlBlock = backend.FlBlock;

const Self = @This();

const Binding = struct {
    // all bindings are typed
    ltype: FlType,

    // this thing as compiled expr, currently using this for builtins. in
    // the future I will probably also want bindings that store Exprs and/or
    // other info
    block: FlBlock,
};

arena: std.heap.ArenaAllocator,

// maps {ident: ltype}
// I think eventually I will need to store Exprs or FlValues associated with
// idents here, but for builtins I think it will always be unnecessary
map: std.StringHashMap(*const Binding),

pub fn init(ally: Allocator) Self {
    return Self{
        .arena = std.heap.ArenaAllocator.init(ally),
        .map = std.StringHashMap(*const Binding).init(ally),
    };
}

/// initializes a scope with all of the necessary typing builtins
pub fn init_global(ally: Allocator) !Self {
    var self = Self.init(ally);
    const scope_ally = self.allocator();

    // some types have to be created from scratch, like `type` and `fn`
    // once those are created, the rest can be compiled!
    const type_ltype = FlType{ .ltype = {} };

    try self.bind("type", Binding{
        .ltype = type_ltype,
        .block = FlBlock{
            .constants = &.{ FlValue{ .ltype = FlType{ .ltype = {} } } },
            .ops = FlBlock.assemble_ops("push 0")
        }
    });

    try self.bind("fn", Binding{
        .ltype = try FlType.init_function(
            scope_ally,
            &[_]FlType{
                try FlType.init_list(scope_ally, &type_ltype),
                type_ltype
            },
            &type_ltype
        ),
        .block = FlBlock{
            .constants = &.{},
            .ops = FlBlock.assemble_ops("fn_type"),
        }
    });

    // TODO add other builtins through dynamic exec
    const compilable_builtins = comptime [_][2][]const u8{
        .{"int", "type"},
        .{"float", "type"},
    };

    inline for (compilable_builtins) |kv| {
        try self.compile_bind(ally, kv[0], kv[1]);
    }

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

        var entries = self.map.iterator();
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
                try std.fmt.allocPrint(tmp_ally, "<{}>", .{value.*.ltype}),
                canvas.ConsoleColor{}
            ));
        }

        tc.print(stderr) catch unreachable;
        stderr.writeAll("\n") catch unreachable;
    }

    return self;
}

pub fn deinit(self: *Self) void {
    self.arena.deinit();
    self.map.deinit();
}

fn allocator(self: *Self) Allocator {
    return self.arena.allocator();
}

pub fn bind(
    self: *Self,
    name: []const u8,
    binding: Binding
) Allocator.Error!void {
    const ally = self.allocator();
    var ptr = try ally.create(Binding);
    ptr.* = binding;

    try self.map.put(name, ptr);
}

pub fn assemble_bind(
    self: *Self,
    ally: Allocator,
    name: []const u8,
    comptime ltype_expr: []const u8,
    comptime constants: []const []const u8,
    comptime assembly: []const u8
) !void {
    const scope_ally = self.allocator();
    const eval = backend.internal_eval;

    var ltype = (try eval(ally, self, "builtin", ltype_expr)).ltype;

    var const_vals: [constants.len]FlValue = undefined;
    inline for (constants) |text, i| {
        const_vals[i] = try eval(ally, self, "builtin constant", text);
    }

    const block = try FlBlock.assemble(scope_ally, &const_vals, assembly);
    try self.bind(name, Binding{ .ltype = ltype, .block = block });
}

// TODO make pipelines and use a compilation pipeline here
pub fn compile_bind(
    self: *Self,
    ally: Allocator,
    comptime name: []const u8,
    comptime text: []const u8
) !void {
    const scope_ally = self.allocator();

    var lfile = try FlFile.init(ally, name, text);
    defer lfile.deinit(ally);

    var ctx = Context.init(ally, &lfile);
    defer ctx.deinit();

    var ast = frontend.parse(&ctx, .expr) catch |e| {
        if (e == util.CompilationFailed) try ctx.print_messages();
        return e;
    };
    defer ast.deinit();

    try frontend.analyze(&ctx, self, ast.allocator(), &ast.root);

    var block = backend.compile(ally, self, &lfile, &ast.root) catch |e| {
        if (e == util.CompilationFailed) try ctx.print_messages();
        return e;
    };
    defer block.deinit(ally);

    try self.bind(name, Binding{
        .ltype = try ast.root.ltype.clone(scope_ally),
        .block = try block.clone(scope_ally),
    });
}

pub fn get(self: *const Self, key: []const u8) ?*const Binding {
    return self.map.get(key);
}