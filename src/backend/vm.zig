//! register-based virtual machine for executing Fluent ops

const std = @import("std");
const fluent = @import("fluent.zig");
const ir = @import("ir.zig");
const Env = @import("env.zig");

const Allocator = std.mem.Allocator;
const SType = fluent.SType;
const SExpr = fluent.SExpr;
const Op = ir.Op;
const Block = ir.Block;
const stdout = std.io.getStdOut().writer();

pub const Vm = struct {
    const Self = @This();

    ally: Allocator,

    pub fn init(ally: Allocator) Self {
        return Self{
            .ally = ally,
        };
    }

    fn do_op(
        self: Self,
        block: Block,
        locals: []SExpr,
        op: Op
    ) Allocator.Error!void {
        const a = op.a;
        const b = op.b;

        locals[op.to] = switch (op.code) {
            .@"const" => try block.consts[a].clone(self.ally),
            .copy => try locals[a].clone(self.ally),

            .iadd => SExpr{ .int = locals[a].int + locals[b].int },
            .isub => SExpr{ .int = locals[a].int - locals[b].int },
            .imul => SExpr{ .int = locals[a].int * locals[b].int },
            .idiv => SExpr{ .int = @divTrunc(locals[a].int, locals[b].int) },
            .imod => SExpr{ .int = @rem(locals[a].int, locals[b].int) },

            else => |code| std.debug.panic("TODO do op {s}", .{@tagName(code)})
        };
    }

    /// execute a block with inputs
    pub fn execute(
        self: Self,
        block: Block,
        inputs: []const SExpr
    ) Allocator.Error!SExpr {
        std.debug.assert(inputs.len == block.inputs);

        // allocate locals
        var locals = try self.ally.alloc(SExpr, block.locals.len);
        defer {
            for (locals) |local| local.deinit(self.ally);
            self.ally.free(locals);
        }

        for (inputs) |input, i| locals[i] = try input.clone(self.ally);

        // run program
        for (block.ops) |op| try self.do_op(block, locals, op);

        // return output
        return try locals[block.output].clone(self.ally);
    }
};