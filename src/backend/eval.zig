//! evaluating fluent SExprs dynamically.
//! SExprs are compiled to Ops and then executed on a virtual machine.
//!
//! Ops are the 'quadruple' format of three address code. every Block contains
//! some number of statically known local variables, which ops reference in
//! their addressing.

const std = @import("std");
const kz = @import("kritzler");
const fluent = @import("fluent.zig");

const Allocator = std.mem.Allocator;
const FlatType = fluent.Type;
const SType = fluent.SType;
const SExpr = fluent.SExpr;
const stdout = std.io.getStdOut().writer();

/// ops are the three-address-code representation of dynamic fluent code
pub const Op = struct {
    const Self = @This();

    // metaprogrammed table used to centralize changing data about opcodes
    const opcodes = blk: {
        const OpcodeData = struct {
            name: []const u8,
            a: FlatType,
            b: FlatType,
            out: FlatType,

            fn init(
                name: []const u8,
                a: FlatType,
                b: FlatType,
                out: FlatType
            ) @This() {
                return @This(){
                    .name = name,
                    .a = a,
                    .b = b,
                    .out = out
                };
            }
        };

        // 'nil' encodes 'no data'
        // 'undef' encodes 'unchecked type'
        const x = OpcodeData.init;
        const table = [_]OpcodeData{
            // loads a constant
            x("constant", .undef, .nil, .undef),
            x("iadd", .int, .int, .int),
        };

        break :blk table[0..];
    };

    /// constructs enum from table
    pub const Code = blk: {
        const TypeInfo = std.builtin.TypeInfo;

        var fields: [opcodes.len]TypeInfo.EnumField = undefined;
        inline for (opcodes) |opcode, i| {
            fields[i] = TypeInfo.EnumField{
                .name = opcode.name,
                .value = i
            };
        }

        const enum_type = TypeInfo.Enum{
            .layout = .Auto,
            .tag_type = UInt,
            .fields = &fields,
            .decls = &.{},
            .is_exhaustive = true,
        };

        break :blk @Type(TypeInfo{ .Enum = enum_type });
    };

    // used for the 4 fields, including backing int for enum
    pub const UInt = u16;

    // keep opcode in a 64-bit word
    comptime {
        std.debug.assert(@sizeOf(Self) <= 8);
    }

    code: Code, // operation to perform
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
            .constant => try writer.print("{} = const {}", .{self.to, self.a}),
            .iadd => try writer.print(
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
        const constants = try SExpr.clone_slice(self.ally, block.constants);
        const locals = try SType.clone_slice(self.ally, block.locals);

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
            Op{ .code = .constant, .a = 0, .to = 0 },
            Op{ .code = .constant, .a = 1, .to = 1 },
            Op{ .code = .iadd, .a = 0, .b = 1, .to = 0 },
        },
    });

    var block = mason.consume();
    defer block.deinit(ally);

    try block.display(ally, "testing masonry", .{});
}