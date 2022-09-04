//! run contains the high-level implementation of running a fluent program
//! from its first SExprs to its last

const std = @import("std");
const fluent = @import("fluent.zig");
const sema = @import("sema.zig");
const ir = @import("ir.zig");
const Vm = @import("vm.zig").Vm;
const Env = @import("env.zig");

const Allocator = std.mem.Allocator;
const SExpr = fluent.SExpr;

pub fn run(
    ally: Allocator,
    env: *Env,
    program: []const SExpr
) !SExpr {
    // generate typed ast
    const ast = try sema.analyze(ally, env.*, program);
    defer ast.deinit();

    for (ast.exprs) |expr| try expr.display(ally, "sema produced", .{});

    // TODO dependency solver phase here

    // cyclically lower ast to IR
    var vm = Vm.init(ally);

    var last_value = SExpr{ .unit = {} };

    for (ast.exprs) |expr| {
        if (expr == .def) {
            // generate code for type
            const label =
                try std.fmt.allocPrint(ally, "type of {s}", .{expr.def.symbol});
            defer ally.free(label);

            const type_block =
                try ir.lower_expr(ally, env.*, label, expr.def.anno.*);
            defer type_block.deinit(ally);

            try type_block.display(ally);

            // execute type
            const stype = (try vm.execute(type_block, &.{})).stype;

            _ = stype;
            @panic("TODO code gen for def body");

            // TODO define
        } else {
            // lower expr
            const block = try ir.lower_expr(ally, env.*, "expr", expr);
            defer block.deinit(ally);

            try block.display(ally);

            // execute
            last_value.deinit(ally);
            last_value = try vm.execute(block, &.{});
        }
    }

    return last_value;
}