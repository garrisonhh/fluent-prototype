//! contains functionality to perform optimizations on fluent ir.

const std = @import("std");
const ops = @import("ops.zig");
const blocks = @import("blocks.zig");
const Env = @import("../env.zig");

const Allocator = std.mem.Allocator;
const OpCode = ops.OpCode;
const Op = ops.Op;
const Block = blocks.Block;
const Mason = blocks.Mason;

pub fn optimize(ally: Allocator, env: Env, block: Block) !Block {
    _ = ally;
    _ = env;
    _ = block;

    @panic("TODO");
}