//! Env(ironment) is where everything related to a program is stored. it also
//! acts as a scoping mechanism.

const std = @import("std");
const kz = @import("kritzler");
const util = @import("../util/util.zig");
const fluent = @import("fluent.zig");
const sema = @import("sema.zig");
const ir = @import("ir.zig");
const Builtin = @import("lang.zig").Builtin;

const stdout = std.io.getStdOut().writer();
const Allocator = std.mem.Allocator;
const SExpr = fluent.SExpr;
const SType = fluent.SType;
const TypedExpr = sema.TypedExpr;
const Block = ir.Block;

const Self = @This();

/// bound types and values must be typed, but only in certain circumstances do
/// they need a value
pub const Bound = struct {
    pub const Data = union(enum) {
        local: usize, // function locals
        constant: SExpr, // raw value
        block: usize, // index of a block
        builtin: Builtin,
    };

    stype: SType,
    data: Data,
};
const Map = std.StringHashMap(Bound);

ally: Allocator,
map: Map,
parent: ?*const Self,
blocks: std.ArrayList(Block), // managed by the uppermost parent

pub fn init(ally: Allocator, parent: ?*const Self) Self {
    const blocks = if (parent) |env| env.blocks
                   else std.ArrayList(Block).init(ally);

    return Self{
        .ally = ally,
        .map = Map.init(ally),
        .parent = parent,
        .blocks = blocks,
    };
}

pub fn deinit(self: Self) void {
    // free symbols and deinit values
    var iter = self.map.iterator();
    while (iter.next()) |entry| {
        self.ally.free(entry.key_ptr.*);

        const bound = entry.value_ptr;
        bound.stype.deinit(self.ally);

        switch (bound.data) {
            .local, .builtin, .block => {},
            .constant => |value| value.deinit(self.ally),
        }
    }

    // deinit blocks
    if (self.parent == null) {
        self.blocks.deinit();
    }
}

/// dupes symbol, assumes you already allocated or cloned binding onto the env
/// allocator. errors on redefinition of a binding.
pub fn define_raw(self: *Self, symbol: []const u8, to: Bound) !void {
    if (self.get(symbol) != null) return error.SymbolAlreadyDefined;
    try self.map.put(try self.ally.dupe(u8, symbol), to);
}

/// clones bound and defines it.
pub fn define(self: *Self, symbol: []const u8, to: Bound) !void {
    const cloned = Bound{
        .stype = try to.stype.clone(self.ally),
        .data = switch (to.data) {
            .local, .builtin, .block => to.data,
            .constant => |val| Bound.Data{
                .constant = try val.clone(self.ally)
            },
        }
    };

    try self.define_raw(symbol, cloned);
}

/// define() helper
pub fn define_local(
    self: *Self,
    symbol: []const u8,
    stype: SType,
    index: usize
) !void {
    try self.define(symbol, Bound{
        .stype = stype,
        .data = .{ .local = index }
    });
}

/// define() helper
pub fn define_builtin(
    self: *Self,
    symbol: []const u8,
    stype: SType,
    builtin: Builtin
) !void {
    try self.define(symbol, Bound{
        .stype = stype,
        .data = .{ .builtin = builtin }
    });
}

/// define() helper
pub fn define_constant(
    self: *Self,
    symbol: []const u8,
    stype: SType,
    constant: SExpr
) !void {
    try self.define(symbol, Bound{
        .stype = stype,
        .data = .{ .constant = constant }
    });
}

/// define() helper
pub fn define_type(self: *Self, symbol: []const u8, stype: SType) !void {
    try self.define_constant(
        symbol,
        SType{ .stype = {} },
        SExpr{ .stype = stype }
    );
}

fn get(self: Self, symbol: []const u8) ?Bound {
    return if (self.map.get(symbol)) |bound| bound
           else if (self.parent) |parent| parent.get(symbol)
           else null;
}

pub fn contains(self: Self, symbol: []const u8) bool {
    return self.get(symbol) != null;
}

pub fn get_type(self: Self, symbol: []const u8) ?SType {
    return if (self.get(symbol)) |bound| bound.stype else null;
}

pub fn get_data(self: Self, symbol: []const u8) ?Bound.Data {
    return if (self.get(symbol)) |bound| bound.data else null;
}

/// performs an operation
fn execute_op(
    self: Self,
    ally: Allocator,
    block: Block,
    locals: []SExpr,
    op: ir.Op
) Allocator.Error!void {
    _ = self;

    const a = op.a;
    const b = op.b;

    // op behavior
    const res: ?SExpr = switch (op.code) {
        .@"const" => try block.consts[a].clone(ally),
        .copy => try locals[a].clone(ally),

        .peek => try locals[a].ptr.to.clone(ally),
        .poke => poke: {
            locals[a].ptr.to.* = try locals[b].clone(ally);
            break :poke null;
        },
        .pinc => SExpr{
            .ptr = .{
                .owns = false,
                .to = &@ptrCast([*]SExpr, locals[a].ptr.to)[1]
            }
        },
        .padd => padd: {
            const offset = @intCast(usize, locals[b].int);

            break :padd SExpr{
                .ptr = .{
                    .owns = false,
                    .to = &@ptrCast([*]SExpr, locals[a].ptr.to)[offset]
                }
            };
        },

        .alloc => alloc: {
            // TODO stype (locals[a]) is unused here, what do I do with it?
            const size = @intCast(usize, locals[b].int);
            const list = try ally.alloc(SExpr, size);

            for (list) |*elem| elem.* = SExpr{ .undef = {} };

            break :alloc SExpr{ .list = list };
        },
        .index => SExpr{
            .ptr = .{
                .owns = false,
                .to = &locals[a].list[@intCast(usize, locals[b].int)]
            }
        },
        .list_ptr => SExpr{
            .ptr = .{ .owns = false, .to = &locals[a].list[0] }
        },

        .@"fn" => @"fn": {
            const params = try ally.alloc(SType, locals[a].list.len);
            for (locals[a].list) |expr, i| {
                params[i] = try expr.stype.clone(ally);
            }

            const returns = try util.place_on(
                ally,
                try locals[b].stype.clone(ally)
            );

            break :@"fn" SExpr{
                .stype = SType{
                    .func = .{ .params = params, .returns = returns }
                }
            };
        },

        .iadd => SExpr{ .int = locals[a].int + locals[b].int },
        .isub => SExpr{ .int = locals[a].int - locals[b].int },
        .imul => SExpr{ .int = locals[a].int * locals[b].int },
        .idiv => SExpr{ .int = @divTrunc(locals[a].int, locals[b].int) },
        .imod => SExpr{ .int = @rem(locals[a].int, locals[b].int) },

        else => |code| std.debug.panic("TODO do op {s}", .{@tagName(code)})
    };

    // cleanup local if replaced
    if (res) |val| {
        if (locals[op.to] != .undef) locals[op.to].deinit(ally);
        locals[op.to] = val;
    }
}

/// execute a block with inputs
pub fn execute(
    self: Self,
    ally: Allocator,
    block: Block,
    inputs: []const SExpr
) Allocator.Error!SExpr {
    std.debug.assert(inputs.len == block.inputs);

    // allocate locals
    var locals = try ally.alloc(SExpr, block.locals.len);
    defer {
        for (locals) |local| local.deinit(ally);
        ally.free(locals);
    }

    for (inputs) |input, i| locals[i] = try input.clone(ally);

    // run program
    for (block.ops) |op| {
        try @call(
            .{ .modifier = .always_inline },
            self.execute_op,
            .{ally, block, locals, op}
        );
    }

    // return output
    return try locals[block.output].clone(ally);
}

pub fn display(
    self: Self,
    comptime label_fmt: []const u8,
    label_args: anytype
) !void {
    var table = try kz.Table(&.{
        .{ .title = "symbol", .fmt = "{s}", .color = kz.Color{ .fg = .red } },
        .{ .title = "type", .fmt = "<{}>", .color = kz.Color{ .fg = .green } },
        .{ .title = "data", .fmt = "{s}" },
    }).init(self.ally, label_fmt, label_args);

    // collect and order env's variables
    const EnvVar = Map.Unmanaged.Entry;
    const Closure = struct {
        // sorts first by bound type, then alphabetically
        fn ev_less_than(ctx: void, a: EnvVar, b: EnvVar) bool {
            _ = ctx;

            const cmp = @intCast(isize, @enumToInt(b.value_ptr.data))
                      - @intCast(isize, @enumToInt(a.value_ptr.data));

            if (cmp > 0) return true
            else if (cmp < 0) return false
            else return std.mem.lessThan(u8, a.key_ptr.*, b.key_ptr.*);
        }
    };

    var env_vars = try self.ally.alloc(EnvVar, self.map.count());
    defer self.ally.free(env_vars);

    var i: usize = 0;
    var iter = self.map.iterator();
    while (iter.next()) |entry| : (i += 1) env_vars[i] = entry;

    std.sort.sort(EnvVar, env_vars, {}, Closure.ev_less_than);

    // add to table and flush
    for (env_vars) |ev| {
        const symbol = ev.key_ptr.*;
        const bound = ev.value_ptr;

        const stype = &bound.stype;
        const data = switch (bound.data) {
            .local => |index| try table.print("local {}", .{index}),
            .block => |index| try table.print("block {}", .{index}),
            .constant => |value| try table.print("{}", .{value}),
            .builtin => |builtin| try table.print("{}", .{builtin}),
        };

        try table.add_row(.{ symbol, stype, data });
    }

    try table.flush(stdout);
}
