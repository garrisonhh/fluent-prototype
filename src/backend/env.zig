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
        value: SExpr, // raw value
        block: usize, // index of a block
    };

    stype: SType,
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
            .local, .block => {},
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
            .local, .block => to.data,
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
    stype: SType,
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
    stype: SType,
    value: SExpr
) !void {
    try self.define(symbol, Bound{
        .stype = stype,
        .data = .{ .value = value }
    });
}

/// define() helper
pub fn define_type(self: *Self, symbol: []const u8, stype: SType) !void {
    try self.define_value(
        symbol,
        SType{ .stype = {} },
        SExpr{ .stype = stype }
    );
}

/// define() helper
/// returns block index
pub fn define_block(
    self: *Self,
    symbol: []const u8,
    stype: SType,
    block: Block
) !usize {
    const index = self.blocks.items.len;
    try self.blocks.append(try block.clone(self.ally));

    try self.define(symbol, Bound{
        .stype = stype,
        .data = .{ .block = index }
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

pub fn get_type(self: Self, symbol: []const u8) ?SType {
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
    frames: std.ArrayList([]SExpr),
    top_frame: []SExpr,

    fn init(ally: Allocator, blocks: []const Block) Process {
        return Process{
            .ally = ally,
            .blocks = blocks,
            .frames = std.ArrayList([]SExpr).init(ally),
            .top_frame = undefined
        };
    }

    fn deinit(self: Process) void {
        self.frames.deinit();
    }

    fn push_frame(self: *Process, size: usize) Allocator.Error!void {
        self.top_frame = try self.ally.alloc(SExpr, size);
        std.mem.set(SExpr, self.top_frame, SExpr{ .undef = {} });

        try self.frames.append(self.top_frame);
    }

    fn drop_frame(self: *Process) void {
        std.debug.assert(self.frames.items.len > 0);

        // free values in frame
        for (self.top_frame) |value| value.deinit(self.ally);

        // free frame
        self.ally.free(self.frames.pop());

        const num_frames = self.frames.items.len;
        if (num_frames > 0) self.top_frame = self.frames.items[num_frames - 1];
    }
};

/// performs an operation
fn execute_op(
    state: *Process,
    block: Block,
    frame: []SExpr,
    op: ir.Op
) Allocator.Error!void {
    const ally = state.ally;

    const a = op.a;
    const b = op.b;

    // op behavior
    const res: ?SExpr = switch (op.code) {
        .@"const" => try block.consts[a].clone(ally),
        .copy => try frame[a].clone(ally),
        .frame => frame: {
            try state.push_frame(a);
            break :frame null;
        },
        .param => param: {
            state.top_frame[a] = try frame[b].clone(ally);
            break :param null;
        },
        .call => call: {
            // run instructions
            const call_frame = state.top_frame;
            const call_block = state.blocks[op.a];

            for (call_block.ops) |call_op| {
                try execute_op(state, call_block, call_frame, call_op);
            }

            // get returned value and deallocate
            const returned = try call_frame[call_block.output].clone(ally);
            state.drop_frame();

            break :call returned;
        },

        .peek => try frame[a].ptr.to.clone(ally),
        .poke => poke: {
            frame[a].ptr.to.* = try frame[b].clone(ally);
            break :poke null;
        },
        .pinc => SExpr{
            .ptr = .{
                .owns = false,
                .to = &@ptrCast([*]SExpr, frame[a].ptr.to)[1]
            }
        },
        .padd => padd: {
            const offset = @intCast(usize, frame[b].int);

            break :padd SExpr{
                .ptr = .{
                    .owns = false,
                    .to = &@ptrCast([*]SExpr, frame[a].ptr.to)[offset]
                }
            };
        },

        .alloc => alloc: {
            // TODO stype (locals[a]) is unused here, what do I do with it?
            const size = @intCast(usize, frame[b].int);
            const list = try ally.alloc(SExpr, size);

            for (list) |*elem| elem.* = SExpr{ .undef = {} };

            break :alloc SExpr{ .list = list };
        },
        .index => SExpr{
            .ptr = .{
                .owns = false,
                .to = &frame[a].list[@intCast(usize, frame[b].int)]
            }
        },
        .list_ptr => SExpr{
            .ptr = .{ .owns = false, .to = &frame[a].list[0] }
        },

        .@"fn" => @"fn": {
            const params = try ally.alloc(SType, frame[a].list.len);
            for (frame[a].list) |expr, i| {
                params[i] = try expr.stype.clone(ally);
            }

            const returns = try util.place_on(
                ally,
                try frame[b].stype.clone(ally)
            );

            break :@"fn" SExpr{
                .stype = SType{
                    .func = .{ .params = params, .returns = returns }
                }
            };
        },

        .iadd => SExpr{ .int = frame[a].int + frame[b].int },
        .isub => SExpr{ .int = frame[a].int - frame[b].int },
        .imul => SExpr{ .int = frame[a].int * frame[b].int },
        .idiv => SExpr{ .int = @divTrunc(frame[a].int, frame[b].int) },
        .imod => SExpr{ .int = @rem(frame[a].int, frame[b].int) },

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
    inputs: []const SExpr
) Allocator.Error!SExpr {
    std.debug.assert(inputs.len == block.inputs);

    stdout.print("executing block:\n", .{}) catch {};
    block.display(ally) catch {};

    // allocate locals
    var locals = try ally.alloc(SExpr, block.locals.len);
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
            .block => |index| try table.print("block {}", .{index}),
            .value => |value| try table.print("{}", .{value}),
        };

        try table.add_row(.{ symbol, stype, data });
    }

    try table.flush(stdout);
}
