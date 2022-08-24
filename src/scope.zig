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
    ltype: FlType, // all bindings are typed
    data: union(enum) {
        block: FlBlock, // a compiled expr
        param: usize, // a function parameter index
    },
};

arena: std.heap.ArenaAllocator,

// maps {ident: ltype}
// I think eventually I will need to store Exprs or FlValues associated with
// idents here, but for builtins I think it will always be unnecessary
map: std.StringHashMap(*const Binding),

// when lexical scope is introduced in the ast, scopes may be stacked on top of
// each other
parent: ?*Self,

pub fn init(ally: Allocator, parent: ?*Self) Self {
    return Self{
        .arena = std.heap.ArenaAllocator.init(ally),
        .map = std.StringHashMap(*const Binding).init(ally),
        .parent = parent,
    };
}

/// initializes a scope with all of the necessary typing builtins
pub fn init_global(ally: Allocator) !Self {
    var self = Self.init(ally, null);
    const scope_ally = self.allocator();

    const type_ltype = FlType{ .ltype = {} };

    // primitives
    try self.bind_prim("type", type_ltype);
    try self.bind_prim("int", FlType{ .int = {} });
    try self.bind_prim("float", FlType{ .float = {} });
    try self.bind_prim("any", FlType{ .any = {} });

    // `fn` can't really take any shortcuts for binding
    try self.bind_block(
        "fn",
        try FlType.init_function(
            scope_ally,
            &.{
                try FlType.init_list(scope_ally, &type_ltype),
                type_ltype
            },
            &type_ltype
        ),
        FlBlock{
            .constants = &.{},
            .ops = FlBlock.assemble_ops("fn_type"),
        }
    );

    // compilable builtins (name, type expr, stack vm assembly)
    const compilable = [_][3][]const u8{
        .{"list", "(fn [type] type)", "list_type"},
        .{"+", "(fn [int int] int)", "iadd"},
        .{"-", "(fn [int int] int)", "isub"},
    };

    inline for (compilable) |elem| {
        const name = elem[0];
        const ltype = elem[1];
        const assembly = elem[2];

        try self.bind_assembly(ally, name, ltype, assembly);
    }

    if (comptime builtin.mode == .Debug) try self.display(ally, stderr);

    return self;
}

pub fn deinit(self: *Self) void {
    self.arena.deinit();
    self.map.deinit();
}

pub fn allocator(self: *Self) Allocator {
    return self.arena.allocator();
}

/// copies binding onto hashmap
fn bind(self: *Self, name: []const u8, binding: Binding) !void {
    const ally = self.allocator();
    var ptr = try ally.create(Binding);
    ptr.* = binding;

    try self.map.put(name, ptr);
}

fn bind_block(
    self: *Self,
    name: []const u8,
    ltype: FlType,
    block: FlBlock
) Allocator.Error!void {
    try self.bind(name, Binding{ .ltype = ltype, .data = .{ .block = block } });
}

pub fn bind_param(
    self: *Self,
    name: []const u8,
    ltype: FlType,
    param: usize
) Allocator.Error!void {
    try self.bind(name, Binding{ .ltype = ltype, .data = .{ .param = param } });
}

fn bind_prim(self: *Self, name: []const u8, ltype: FlType) !void {
    const constants = [_]FlValue{ FlValue{ .ltype = ltype } };
    const block = FlBlock{
        .constants = &constants,
        .ops = FlBlock.assemble_ops("push 0")
    };

    try self.bind_block(
        name,
        FlType{ .ltype = {} },
        try block.clone(self.allocator())
    );
}

fn bind_assembly(
    self: *Self,
    ally: Allocator,
    comptime name: []const u8,
    comptime ltype_expr: []const u8,
    comptime text: []const u8
) !void {
    var ltype_val =
        try plumbing.evaluate_text(ally, self, name, ltype_expr, null);
    defer ltype_val.deinit(ally);

    try self.bind_block(
        name,
        try ltype_val.ltype.clone(self.allocator()),
        plumbing.assemble_simple(text)
    );
}

pub fn get(self: *const Self, key: []const u8) ?*const Binding {
    return if (self.map.get(key)) |got| got
           else if (self.parent) |parent| parent.get(key)
           else null;
}

pub fn display(self: *const Self, ally: Allocator, writer: anytype) !void {
    var tc = canvas.TextCanvas.init(ally);
    defer tc.deinit();
    const tmp_ally = tc.temp_allocator();

    try tc.add_box(canvas.TextBox.init(
        .{ .x = 0, .y = -1 },
        "scope:",
        canvas.ConsoleColor{ .fg = .cyan }
    ));

    // nicely organize entries
    const Entry = @TypeOf(self.map).Entry;
    const Closure = struct {
        /// -1 if a < b
        fn type_cmp(a: *const FlType, b: *const FlType) i32 {
            const tag_priority = comptime blk: {
                var arr = std.EnumArray(FlType.Enum, i32).initFill(0);
                arr.set(.unknown, -2);
                arr.set(.any, -1);
                arr.set(.ltype, 1);
                arr.set(.function, 2);

                break :blk arr;
            };

            return tag_priority.get(a.*) - tag_priority.get(b.*);
        }

        /// order by type first and then alphabetically
        pub fn entry_less_than(ctx: void, a: Entry, b: Entry) bool {
            _ = ctx;

            const cmp = type_cmp(&a.value_ptr.*.ltype, &b.value_ptr.*.ltype);
            return if (cmp == 0) std.mem.lessThan(u8, a.key_ptr.*, b.key_ptr.*)
                   else cmp < 0;
        }
    };
    const entry_less_than = Closure.entry_less_than;

    var entries = std.ArrayList(Entry).init(ally);
    defer entries.deinit();

    var entry_iter = self.map.iterator();
    while (entry_iter.next()) |entry| try entries.append(entry);

    std.sort.sort(Entry, entries.items, {}, entry_less_than);

    // display entries
    for (entries.items) |*entry, i| {
        const key = entry.key_ptr.*;
        const value = entry.value_ptr.*;

        try tc.add_box(canvas.TextBox.init(
            .{
                .x = -@intCast(i32, key.len + 2),
                .y = @intCast(i32, i)
            },
            try std.fmt.allocPrint(tmp_ally, "{s}: ", .{key}),
            canvas.ConsoleColor{ .fmt = .bold }
        ));

        try tc.add_box(canvas.TextBox.init(
            .{ .x = 0, .y = @intCast(i32, i) },
            try std.fmt.allocPrint(tmp_ally, "<{}>", .{value.ltype}),
            canvas.ConsoleColor{}
        ));
    }

    try tc.print(writer);
    try writer.writeAll("\n");
}