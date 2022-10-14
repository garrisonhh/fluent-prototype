const std = @import("std");
const util = @import("util");
const fluent = @import("../fluent.zig");

pub const OpCode = enum {
    const Self = @This();

    // unique
    @"const", // load a constant
    copy, // copy a local to another local
    param, // load a local to a param slot
    call, // call a function with loaded params

    // control flow
    jmp, // unconditional jump to a label
    skip_if, // jump if condition is false

    // ptr ops
    // TODO create,
    // TODO destroy,
    peek, // load
    poke, // store
    pinc,
    padd,

    // list ops
    alloc, // takes type, size; allocates list
    // TODO free,
    index, // takes list, index; gets ptr to index
    list_ptr, // cast list to ptr

    // types
    @"fn",

    // math
    iinc,
    idec,
    iadd,
    isub,
    imul,
    idiv,
    imod,

    // logic
    land,
    lor,
    lnot,

    // conditions
    ieq,
    ilt,

    const OpMeta = union(enum) {
        // writes data to a local
        unary,
        binary,

        // has no direct output
        unary_effect,
        binary_effect,
    };

    fn get_metadata(self: Self) OpMeta {
        const flow_table = comptime blk: {
            break :blk util.EnumTable(Self, OpMeta).init(.{
                .{.@"const", .unary},
                .{.copy, .unary},
                .{.param, .binary_effect},
                .{.call, .unary},

                .{.jmp, .unary_effect},
                .{.skip_if, .binary_effect},

                .{.peek, .unary},
                .{.poke, .binary_effect},
                .{.pinc, .unary},
                .{.padd, .binary},

                .{.alloc, .binary},
                .{.index, .binary},
                .{.list_ptr, .unary},

                .{.@"fn", .binary},

                .{.iinc, .unary},
                .{.idec, .unary},
                .{.iadd, .binary},
                .{.isub, .binary},
                .{.imul, .binary},
                .{.idiv, .binary},
                .{.imod, .binary},

                .{.land, .binary},
                .{.lor, .binary},
                .{.lnot, .unary},

                .{.ieq, .binary},
                .{.ilt, .binary},
            });
        };

        return flow_table.get(self);
    }
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
    b: UInt = undefined,
    to: UInt = undefined, // where result is stored

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        _ = fmt;
        _ = options;

        switch (self.code) {
            .@"const" => try writer.print("l{} = c{}", .{self.to, self.a}),
            .copy => try writer.print("l{} = copy l{}", .{self.to, self.a}),
            .param => try writer.print("param {} = l{}", .{self.a, self.b}),
            .call => try writer.print("l{} = call l{}", .{self.to, self.a}),
            .jmp => try writer.print("jmp @{}", .{self.a}),
            .skip_if => try writer.print("skip_if l{} @{}", .{self.a, self.b}),
            else => switch (self.code.get_metadata()) {
                .unary => try writer.print(
                    "l{} = {s} l{}",
                    .{self.to, @tagName(self.code), self.a}
                ),
                .binary => try writer.print(
                    "l{} = {s} l{} l{}",
                    .{self.to, @tagName(self.code), self.a, self.b}
                ),
                .unary_effect => try writer.print(
                    "{s} l{}",
                    .{@tagName(self.code), self.a}
                ),
                .binary_effect => try writer.print(
                    "{s} l{} l{}",
                    .{@tagName(self.code), self.a, self.b}
                ),
            }
        }
    }
};