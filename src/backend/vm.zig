//! register-based virtual machine for executing Fluent ops

const std = @import("std");
const util = @import("../util/util.zig");
const fluent = @import("fluent.zig");
const ir = @import("ir.zig");
const Env = @import("env.zig");

const Allocator = std.mem.Allocator;
const SType = fluent.SType;
const SExpr = fluent.SExpr;
const Op = ir.Op;
const Block = ir.Block;
const stdout = std.io.getStdOut().writer();

// TODO absorb this into `Env`
pub const Vm = struct {
    const Self = @This();

    ally: Allocator,

    pub fn init(ally: Allocator) Self {
        return Self{
            .ally = ally,
        };
    }

    /// performs an operation
    fn do(
        self: Self,
        block: Block,
        locals: []SExpr,
        op: Op
    ) Allocator.Error!void {
        const a = op.a;
        const b = op.b;

        // op behavior
        const res: ?SExpr = switch (op.code) {
            .@"const" => try block.consts[a].clone(self.ally),
            .copy => try locals[a].clone(self.ally),

            .peek => try locals[a].ptr.to.clone(self.ally),
            .poke => poke: {
                locals[a].ptr.to.* = try locals[b].clone(self.ally);
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
                const list = try self.ally.alloc(SExpr, size);

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
                const params = try self.ally.alloc(SType, locals[a].list.len);
                for (locals[a].list) |expr, i| {
                    params[i] = try expr.stype.clone(self.ally);
                }

                const returns = try util.place_on(
                    self.ally,
                    try locals[b].stype.clone(self.ally)
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
            if (locals[op.to] != .undef) locals[op.to].deinit(self.ally);
            locals[op.to] = val;
        }
    }

    /// execute a block with inputs
    pub fn execute(
        self: Self,
        block: Block,
        inputs: []const SExpr
    ) Allocator.Error!SExpr {
        std.debug.assert(inputs.len == block.inputs);

        // TODO remove vvv
        stdout.print("executing block:\n", .{}) catch {};
        block.display(self.ally) catch {};

        // allocate locals
        var locals = try self.ally.alloc(SExpr, block.locals.len);
        defer {
            for (locals) |local| local.deinit(self.ally);
            self.ally.free(locals);
        }

        for (inputs) |input, i| locals[i] = try input.clone(self.ally);

        // run program
        for (block.ops) |op| {
            try @call(
                .{ .modifier = .always_inline },
                self.do,
                .{block, locals, op}
            );
        }

        // return output
        return try locals[block.output].clone(self.ally);
    }
};