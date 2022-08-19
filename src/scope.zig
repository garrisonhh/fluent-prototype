const std = @import("std");
const builtin = @import("builtin");
const util = @import("util/util.zig");
const canvas = @import("util/canvas.zig");
const fluent = @import("fluent.zig");
const backend = @import("backend.zig");
const plumbing = @import("plumbing.zig");
const FlFile = @import("file.zig");

const Allocator = std.mem.Allocator;
const Context = FlFile.Context;
const FlType = fluent.FlType;
const FlValue = fluent.FlValue;
const FlBlock = backend.FlBlock;
const stderr = std.io.getStdErr().writer();

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

    const type_ltype = FlType{ .ltype = {} };

    const Closure = struct {
        fn bind_prim(scope: *Self, name: []const u8, ltype: FlType) !void {
            const constants = [_]FlValue{ FlValue{ .ltype = ltype } };
            const block = FlBlock{
                .constants = &constants,
                .ops = FlBlock.assemble_ops("push 0")
            };

            try scope.bind(name, Binding{
                .ltype = type_ltype,
                .block = try block.clone(scope.allocator()),
            });
        }
    };
    const bind_prim = Closure.bind_prim;

    // primitives
    try bind_prim(&self, "type", type_ltype);
    try bind_prim(&self, "int", FlType{ .int = {} });
    try bind_prim(&self, "float", FlType{ .float = {} });

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

    if (comptime builtin.mode == .Debug) try self.display(ally, stderr);

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

pub fn compile_bind(
    self: *Self,
    ally: Allocator,
    comptime name: []const u8,
    comptime text: []const u8
) !void {
    var ltype: FlType = undefined;
    var block = try plumbing.compile_text(ally, self, name, text, &ltype);
    defer ltype.deinit(ally);
    defer block.deinit(ally);

    const scope_ally = self.allocator();
    try self.bind(name, Binding{
        .ltype = try ltype.clone(scope_ally),
        .block = try block.clone(scope_ally),
    });
}

pub fn get(self: *const Self, key: []const u8) ?*const Binding {
    return self.map.get(key);
}

pub fn display(self: *const Self, ally: Allocator, writer: anytype) !void {
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

    try tc.print(writer);
    try writer.writeAll("\n");
}