//! evaluating fluent SExprs dynamically.
//! SExprs are compiled to Ops and then executed on a virtual machine.
//!
//! Ops are the 'quadruple' format of three address code. every Block contains
//! some number of statically known local variables, which ops reference in
//! their addressing.

const std = @import("std");
const kz = @import("kritzler");
const util = @import("../util/util.zig");
const fluent = @import("fluent.zig");

const Allocator = std.mem.Allocator;
const FlatType = fluent.Type;
const SType = fluent.SType;
const SExpr = fluent.SExpr;
const stdout = std.io.getStdOut().writer();

pub const OpCode = enum {
    @"const", // load a constant

    // math
    iadd,
    isub,
    imul,
    idiv,
    imod,

    pub const Class = enum {
        unary,
        binary,
    };

    pub const IOTypes = struct {
        a: FlatType,
        b: FlatType = .nil,
        to: FlatType,

        fn init_unary(a: FlatType, to: FlatType) IOTypes {
            return IOTypes{
                .a = a,
                .to = to
            };
        }

        fn init_binary(a: FlatType, b: FlatType, to: FlatType) IOTypes {
            return IOTypes{
                .a = a,
                .b = b,
                .to = to
            };
        }
    };

    // maps code -> iotype
    pub const iotypes = blk: {
        const unary = IOTypes.init_unary;
        const binary = IOTypes.init_binary;

        const Table = util.EnumTable(OpCode, IOTypes);

        break :blk Table.init(.{
            .{ .@"const", unary(.undef, .undef) },

            // math
            .{ .iadd, binary(.int, .int, .int) },
            .{ .isub, binary(.int, .int, .int) },
            .{ .imul, binary(.int, .int, .int) },
            .{ .idiv, binary(.int, .int, .int) },
            .{ .imod, binary(.int, .int, .int) },
        });
    };
};

/// ops are the three-address-code representation of dynamic fluent code
pub const Op = struct {
    const Self = @This();

    // used for the 4 fields, including backing int for enum
    pub const UInt = u16;

    // keep opcode in a 64-bit word
    comptime {
        std.debug.assert(@sizeOf(Self) <= 8);
    }

    code: OpCode, // operation to perform
    a: UInt,
    b: UInt = 0,
    to: UInt, // where result is stored

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        _ = fmt;
        _ = options;

        switch (self.code) {
            .@"const" => try writer.print("{} = const {}", .{self.to, self.a}),
            .iadd, .isub, .imul, .idiv, .imod => try writer.print(
                "{} = {s} {} {}",
                .{self.to, @tagName(self.code), self.a, self.b}
            ),
        }
    }
};

/// basic representation of linearized code
pub const Block = struct {
    const Self = @This();

    constants: []const SExpr,
    locals: []const SType,
    ops: []const Op,

    pub fn deinit(self: Self, ally: Allocator) void {
        ally.free(self.constants);
        ally.free(self.locals);
        ally.free(self.ops);
    }

    pub fn display(
        self: Self,
        ally: Allocator,
        comptime label_fmt: []const u8,
        label_args: anytype
    ) (Allocator.Error || @TypeOf(stdout).Error)!void {
        try stdout.print("{}", .{kz.Color{ .fg = .cyan }});
        try stdout.print(label_fmt, label_args);
        try stdout.print("{}\n\n", .{kz.Color{}});

        try kz.forms.fast_list(
            ally,
            .{ .title = "constants" },
            self.constants,
            stdout
        );

        try kz.forms.fast_list(
            ally,
            .{
                .title = "locals",
                .fmt = "<{}>",
                .color = kz.Color{ .fg = .green }
            },
            self.locals,
            stdout
        );

        try kz.forms.fast_list(
            ally,
            .{ .title = "ops" },
            self.ops,
            stdout
        );
    }
};

/// for building blocks :)
const Mason = struct {
    const Self = @This();

    ally: Allocator,
    constants: std.ArrayList(SExpr),
    locals: std.ArrayList(SType),
    ops: std.ArrayList(Op),

    fn init(ally: Allocator) Self {
        return Self{
            .ally = ally,
            .constants = std.ArrayList(SExpr).init(ally),
            .locals = std.ArrayList(SType).init(ally),
            .ops = std.ArrayList(Op).init(ally),
        };
    }

    /// take all owned memory and turn it into a block
    fn consume(self: *Self) Block {
        return Block{
            .constants = self.constants.toOwnedSlice(),
            .locals = self.locals.toOwnedSlice(),
            .ops = self.ops.toOwnedSlice(),
        };
    }

    fn append_block(self: *Self, block: Block) Allocator.Error!void {
        // dupe inputs
        const constants = try SExpr.clone_slice(self.ally, block.constants);
        const locals = try SType.clone_slice(self.ally, block.locals);
        const ops = try self.ally.dupe(Op, block.ops);

        // adjust references
        const constant_offset = @intCast(Op.UInt, self.constants.items.len);
        const local_offset = @intCast(Op.UInt, self.locals.items.len);
        _  = constant_offset;
        _ = local_offset;

        for (ops) |*op| {
            _ = op;
            // TODO
        }

        // add to lists
        try self.constants.appendSlice(constants);
        try self.locals.appendSlice(locals);
        try self.ops.appendSlice(block.ops);
    }
};

// TODO remove
pub fn test_masonry(ally: Allocator) !void {
    var mason = Mason.init(ally);

    try mason.append_block(.{
        .constants = &.{
            SExpr{ .int = 1234 },
            SExpr{ .int = 1020 },
        },
        .locals = &.{
            SType{ .int = {} },
            SType{ .int = {} },
        },
        .ops = &.{
            Op{ .code = .@"const", .a = 0, .to = 0 },
            Op{ .code = .@"const", .a = 1, .to = 1 },
            Op{ .code = .iadd, .a = 0, .b = 1, .to = 0 },
        },
    });

    var block = mason.consume();
    defer block.deinit(ally);

    try block.display(ally, "testing masonry", .{});
}