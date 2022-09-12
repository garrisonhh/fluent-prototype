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
const Type = fluent.Type;
const Value = fluent.Value;
const TypedExpr = sema.TypedExpr;
const Block = ir.Block;

const Self = @This();

/// bound types and values must be typed, but only in certain circumstances do
/// they need a value
pub const Bound = struct {
    pub const Data = union(enum) {
        local: usize, // function locals
        value: Value, // raw value
    };

    stype: Type,
    data: Data,
};
const Map = std.StringHashMap(Bound);

ally: Allocator,
map: Map,
parent: ?*const Self,
blocks: std.ArrayList(Block), // managed by the uppermost parent

anon_counter: usize = 0,

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
            .local => {},
            .value => |value| value.deinit(self.ally),
        }
    }

    // deinit blocks
    if (self.parent == null) {
        for (self.blocks.items) |block| block.deinit(self.ally);
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
            .local => to.data,
            .value => |val| Bound.Data{
                .value = try val.clone(self.ally)
            },
        }
    };

    try self.define_raw(symbol, cloned);
}

/// define() helper
pub fn define_local(
    self: *Self,
    symbol: []const u8,
    stype: Type,
    index: usize
) !void {
    try self.define(symbol, Bound{
        .stype = stype,
        .data = .{ .local = index }
    });
}

/// define() helper
pub fn define_value(
    self: *Self,
    symbol: []const u8,
    stype: Type,
    value: Value
) !void {
    try self.define(symbol, Bound{
        .stype = stype,
        .data = .{ .value = value }
    });
}

/// define() helper
pub fn define_type(self: *Self, symbol: []const u8, stype: Type) !void {
    try self.define_value(
        symbol,
        Type{ .stype = {} },
        Value{ .stype = stype }
    );
}

/// define() helper
/// returns block index
pub fn define_block(
    self: *Self,
    symbol: []const u8,
    stype: Type,
    block: Block
) !usize {
    const index = self.blocks.items.len;
    try self.blocks.append(try block.clone(self.ally));

    try self.define(symbol, Bound{
        .stype = stype,
        .data = .{ .value = Value{ .func = index } }
    });

    return index;
}

fn get(self: Self, symbol: []const u8) ?Bound {
    return if (self.map.get(symbol)) |bound| bound
           else if (self.parent) |parent| parent.get(symbol)
           else null;
}

pub fn contains(self: Self, symbol: []const u8) bool {
    return self.get(symbol) != null;
}

pub fn get_type(self: Self, symbol: []const u8) ?Type {
    return if (self.get(symbol)) |bound| bound.stype else null;
}

pub fn get_data(self: Self, symbol: []const u8) ?Bound.Data {
    return if (self.get(symbol)) |bound| bound.data else null;
}

/// returns a unique anonymous function name
/// *returned symbol is owned by caller*
pub fn next_anon_func_name(
    self: *Self
) (std.fmt.AllocPrintError || Allocator.Error)![]const u8 {
    const sym = try std.fmt.allocPrint(
        self.ally,
        ".anonymous{}",
        .{self.anon_counter}
    );
    self.anon_counter += 1;

    return sym;
}

/// state management for execution
const Process = struct {
    ally: Allocator,
    blocks: []const Block,

    // used by the `param` instruction to temporarily cache parameters
    // before call frame is allocated
    param_buf: [256]Value = undefined,

    fn init(ally: Allocator, blocks: []const Block) Process {
        return Process{
            .ally = ally,
            .blocks = blocks
        };
    }

    fn deinit(self: Process) void {
        _ = self;
    }
};

/// performs an operation
fn execute_op(
    state: *Process,
    block: Block,
    frame: []Value,
    op: ir.Op
) Allocator.Error!void {
    const ally = state.ally;

    const a = op.a;
    const b = op.b;

    // op behavior
    const res: ?Value = switch (op.code) {
        .@"const" => try block.consts[a].clone(ally),
        .copy => try frame[a].clone(ally),
        .param => param: {
            state.param_buf[a] = try frame[b].clone(ally);
            break :param null;
        },
        .call => call: {
            const call_block = state.blocks[frame[op.a].func];

            // alloc frame
            const num_locals = call_block.locals.len;
            const call_frame = try state.ally.alloc(Value, num_locals);
            defer {
                for (call_frame) |val| val.deinit(state.ally);
                state.ally.free(call_frame);
            }

            // set up params
            std.mem.copy(
                Value,
                call_frame,
                state.param_buf[0..call_block.inputs]
            );

            // execute block ops
            for (call_block.ops) |call_op| {
                try execute_op(state, call_block, call_frame, call_op);
            }

            // return
            break :call try call_frame[call_block.output].clone(ally);
        },

        .peek => try frame[a].ptr.to.clone(ally),
        .poke => poke: {
            frame[a].ptr.to.* = try frame[b].clone(ally);
            break :poke null;
        },
        .pinc => Value{
            .ptr = .{
                .owns = false,
                .to = &@ptrCast([*]Value, frame[a].ptr.to)[1]
            }
        },
        .padd => padd: {
            const offset = @intCast(usize, frame[b].int);

            break :padd Value{
                .ptr = .{
                    .owns = false,
                    .to = &@ptrCast([*]Value, frame[a].ptr.to)[offset]
                }
            };
        },

        .alloc => alloc: {
            // TODO stype (locals[a]) is unused here, what do I do with it?
            const size = @intCast(usize, frame[b].int);
            const list = try ally.alloc(Value, size);

            for (list) |*elem| elem.* = Value{ .undef = {} };

            break :alloc Value{ .list = list };
        },
        .index => Value{
            .ptr = .{
                .owns = false,
                .to = &frame[a].list[@intCast(usize, frame[b].int)]
            }
        },
        .list_ptr => Value{
            .ptr = .{ .owns = false, .to = &frame[a].list[0] }
        },

        .@"fn" => @"fn": {
            const params = try ally.alloc(Type, frame[a].list.len);
            for (frame[a].list) |expr, i| {
                params[i] = try expr.stype.clone(ally);
            }

            const returns = try util.place_on(
                ally,
                try frame[b].stype.clone(ally)
            );

            break :@"fn" Value{
                .stype = Type{
                    .func = .{ .params = params, .returns = returns }
                }
            };
        },

        .iadd => Value{ .int = frame[a].int + frame[b].int },
        .isub => Value{ .int = frame[a].int - frame[b].int },
        .imul => Value{ .int = frame[a].int * frame[b].int },
        .idiv => Value{ .int = @divTrunc(frame[a].int, frame[b].int) },
        .imod => Value{ .int = @rem(frame[a].int, frame[b].int) },

        else => |code| std.debug.panic("TODO do op {s}", .{@tagName(code)})
    };

    // cleanup local if replaced
    if (res) |val| {
        if (frame[op.to] != .undef) frame[op.to].deinit(ally);
        frame[op.to] = val;
    }
}

/// execute a block with inputs
pub fn execute(
    self: Self,
    ally: Allocator,
    block: Block,
    inputs: []const Value
) Allocator.Error!Value {
    std.debug.assert(inputs.len == block.inputs);

    // allocate locals
    var locals = try ally.alloc(Value, block.locals.len);
    defer {
        for (locals) |local| local.deinit(ally);
        ally.free(locals);
    }

    for (inputs) |input, i| locals[i] = try input.clone(ally);

    // run program
    var process = Process.init(ally, self.blocks.items);
    defer process.deinit();

    for (block.ops) |op| {
        try execute_op(&process, block, locals, op);
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
            .value => |value| try table.print("{}", .{value}),
        };

        try table.add_row(.{ symbol, stype, data });
    }

    try table.flush(stdout);
}
