const std = @import("std");
const kz = @import("kritzler");
const fluent = @import("../fluent.zig");
const ops = @import("ops.zig");

const Allocator = std.mem.Allocator;
const Type = fluent.Type;
const Value = fluent.Value;
const Op = ops.Op;
const OpCode = ops.OpCode;
const stdout = std.io.getStdOut().writer();

/// basic representation of linearized code. every block represents a procedure,
/// with some number of parameters (including zero) and returning one value.
///
/// blocks are meant to be manipulated indirectly through Mason
pub const Block = struct {
    const Self = @This();

    consts: []const Value,
    locals: []const Type,
    labels: []const usize,
    ops: []const Op,

    inputs: usize, // locals[0..input] are the parameters for the block
    output: Op.UInt, // index of the output local

    pub fn deinit(self: Self, ally: Allocator) void {
        ally.free(self.consts);
        ally.free(self.locals);
        ally.free(self.labels);
        ally.free(self.ops);
    }

    pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
        return Self{
            .consts = try Value.clone_slice(ally, self.consts),
            .locals = try Type.clone_slice(ally, self.locals),
            .labels = try ally.dupe(usize, self.labels),
            .ops = try ally.dupe(Op, self.ops),
            .inputs = self.inputs,
            .output = self.output
        };
    }

    /// returns const ref to output type
    pub fn output_type(self: Self) *const Type {
        return &self.locals[self.output];
    }

    pub fn display(
        self: Self,
        ally: Allocator,
        comptime title_fmt: []const u8,
        title_args: anytype
    ) (Allocator.Error || @TypeOf(stdout).Error)!void {
        var canvas = kz.Canvas.init(ally);
        defer canvas.deinit();
        const tmp = canvas.arena.allocator();

        const title_color = kz.Color{ .fg = .red };
        const header_color = kz.Color{ .fg = .cyan };
        const const_color = kz.Color{ .fg = .magenta };
        const type_color = kz.Color{ .fg = .green };
        const label_color = kz.Color{ .fmt = .bold };
        const op_color = kz.Color{};

        var pos = kz.Vec2{0, 0};
        const indent: isize = 2;
        const up = kz.Vec2{indent, 1};
        const down = kz.Vec2{-indent, 0};

        // title
        try canvas.scribble(pos, title_color, title_fmt, title_args);
        pos += up;

        // typing
        var tpos = pos;
        for (self.locals[0..self.inputs]) |stype| {
            const text = try std.fmt.allocPrint(tmp, "{s} ", .{stype});
            try canvas.scribble(tpos, type_color, "{s}", .{text});
            tpos[0] += @intCast(isize, text.len) + 1;
        }

        if (self.inputs > 0) {
            try canvas.scribble(tpos, type_color, "-> ", .{});
            tpos[0] += 3;
        }

        try canvas.scribble(tpos, type_color, "{}", .{self.output_type()});
        pos[1] += 2;

        // consts
        try canvas.scribble(pos, header_color, "consts", .{});
        pos += up;

        for (self.consts) |val| {
            try canvas.scribble(pos, const_color, "{}", .{val});
            pos[1] += 1;
        }

        pos += down;

        // locals
        try canvas.scribble(pos, header_color, "locals", .{});
        pos += up;

        for (self.locals) |stype| {
            try canvas.scribble(pos, type_color, "<{}>", .{stype});
            pos[1] += 1;
        }

        pos += down;

        // prepare labels
        // maps op index -> handle
        var labels = std.AutoHashMap(usize, usize).init(ally);
        defer labels.deinit();

        for (self.labels) |lbl, i| try labels.put(lbl, i);

        // draw ops + labels
        try canvas.scribble(pos, header_color, "ops", .{});
        pos += up;

        for (self.ops) |op, i| {
            if (labels.get(i)) |name| {
                pos[0] -= 1;
                try canvas.scribble(pos, label_color, "@{}:", .{name});
                pos += kz.Vec2{1, 1};
            }

            try canvas.scribble(pos, op_color, "{}", .{op});
            pos[1] += 1;
        }

        pos += down;

        try canvas.flush(stdout);
    }
};

/// for building blocks :)
pub const Mason = struct {
    const Self = @This();

    ally: Allocator,
    consts: std.ArrayList(Value),
    locals: std.ArrayList(Type),
    labels: std.ArrayList(usize),
    ops: std.ArrayList(Op),

    name: []const u8,
    inputs: usize,

    pub fn init(
        ally: Allocator,
        name: []const u8,
        inputs: []const Type
    ) Allocator.Error!Self {
        var locals = std.ArrayList(Type).init(ally);

        try locals.appendSlice(try Type.clone_slice(ally, inputs));

        return Self{
            .ally = ally,
            .consts = std.ArrayList(Value).init(ally),
            .locals = locals,
            .labels = std.ArrayList(usize).init(ally),
            .ops = std.ArrayList(Op).init(ally),
            .name = try ally.dupe(u8, name),
            .inputs = inputs.len,
        };
    }

    /// take all owned memory and turn it into a block
    pub fn build(self: *Self, output: Op.UInt) Block {
        return Block{
            .consts = self.consts.toOwnedSlice(),
            .locals = self.locals.toOwnedSlice(),
            .labels = self.labels.toOwnedSlice(),
            .ops = self.ops.toOwnedSlice(),
            .inputs = self.inputs,
            .output = output
        };
    }

    /// inlines a block given a slice of locals to use as parameters.
    /// returns adjusted output ref.
    ///
    /// TODO use this again for single-call functions
    pub fn inline_block(
        self: *Self,
        block: Block,
        params: []Op.UInt
    ) Allocator.Error!Op.UInt {
        std.debug.assert(params.len == block.inputs);

        const const_offset = @intCast(Op.UInt, self.consts.items.len);
        const local_offset = @intCast(Op.UInt, self.locals.items.len);

        // copy constants and locals
        for (block.consts) |@"const"| {
            try self.consts.append(try @"const".clone(self.ally));
        }

        for (block.locals) |local| {
            try self.locals.append(try local.clone(self.ally));
        }

        // add copy ops
        for (params) |index, i| {
            try self.ops.append(Op{
                .code = .copy,
                .a = index,
                .to = @intCast(Op.UInt, i) + local_offset
            });
        }

        // copy ops and adjusting references along the way
        for (block.ops) |op| {
            const adjusted = switch (op.code.get_metadata()) {
                .@"const" => Op{
                    .code = .@"const",
                    .a = op.a + const_offset,
                    .to = op.to + local_offset
                },
                .unary => Op{
                    .code = op.code,
                    .a = op.a + local_offset,
                    .to = op.to + local_offset
                },
                .binary => Op{
                    .code = op.code,
                    .a = op.a + local_offset,
                    .b = op.b + local_offset,
                    .to = op.to + local_offset
                },
                .unary_effect => Op{
                    .code = op.code,
                    .a = op.a + local_offset
                },
                .binary_effect => Op{
                    .code = op.code,
                    .a = op.a + local_offset,
                    .b = op.b + local_offset
                },
            };

            try self.ops.append(adjusted);
        }

        // TODO labels

        return block.output + local_offset;
    }

    /// returns index of constant
    /// TODO would be more intuitive if this defensively copied
    pub fn add_const(self: *Self, @"const": Value) Allocator.Error!Op.UInt {
        const index = @intCast(Op.UInt, self.consts.items.len);
        try self.consts.append(@"const");

        return index;
    }

    /// returns ref to local
    /// TODO would be more intuitive if this defensively copied
    pub fn add_local(self: *Self, local: Type) Allocator.Error!Op.UInt {
        const index = @intCast(Op.UInt, self.locals.items.len);
        try self.locals.append(local);

        return index;
    }

    /// returns 'name' of label
    pub fn add_label(self: *Self) Allocator.Error!Op.UInt {
        const name = @intCast(Op.UInt, self.labels.items.len);
        try self.labels.append(self.ops.items.len);

        return name;
    }

    /// returns ref to local output of op
    pub fn add_op(self: *Self, op: Op) Allocator.Error!Op.UInt {
        try self.ops.append(op);
        return op.to;
    }

    /// adds an op and returns index of op
    pub fn add_backref(self: *Self, op: Op) Allocator.Error!usize {
        const backref = self.ops.items.len;
        _ = try self.add_op(op);

        return backref;
    }
};