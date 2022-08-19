//! plumbing is where the pipelines are. lol

const std = @import("std");
const util = @import("util/util.zig");
const frontend = @import("frontend.zig");
const backend = @import("backend.zig");
const fluent = @import("fluent.zig");
const FlFile = @import("file.zig");
const Scope = @import("scope.zig");

const FlValue = fluent.FlValue;
const FlType = fluent.FlType;
const Context = FlFile.Context;
const Expr = frontend.Expr;
const Allocator = std.mem.Allocator;

/// compiles an expression from start to finish
/// writes ltype of entire ast to out_ltype if requested
pub fn compile(
    ctx: *Context,
    scope: *Scope,
    out_ltype: ?*FlType
) !backend.FlBlock {
    // lex + parse
    var ast = try ctx.wrap_stage(frontend.parse(ctx, .expr));
    defer ast.deinit();

    // sema
    const ast_ally = ast.allocator();
    try ctx.wrap_stage(frontend.analyze(ctx, scope, ast_ally, &ast.root));

    // write ltype to out param
    if (out_ltype) |ptr| ptr.* = try ast.root.ltype.clone(ctx.ally);

    // lower
    return try ctx.wrap_stage(backend.lower_ast(ctx, scope, &ast.root));
}

// compiles an expression from text
pub fn compile_text(
    ally: Allocator,
    scope: *Scope,
    name: []const u8,
    text: []const u8,
    out_ltype: ?*FlType
) !backend.FlBlock {
    // create context
    var lfile = try FlFile.init(ally, name, text);
    defer lfile.deinit(ally);

    var ctx = FlFile.Context.init(ally, &lfile);
    defer ctx.deinit();

    // compile
    return try compile(&ctx, scope, out_ltype);
}

/// compiles and runs an expression
pub fn evaluate(
    ctx: *Context,
    scope: *Scope,
    out_ltype: ?*FlType
) !FlValue {
    // compile
    var block = try compile(ctx, scope, out_ltype);
    defer block.deinit(ctx.ally);

    // create vm and run it
    var vm = backend.FlVm.init(ctx.ally);
    defer vm.deinit();

    try vm.execute_block(&block);

    // ensure that only one value is left on stack
    const stack = vm.stack.items;
    if (stack.len != 1) {
        try ctx.add_message(
            .err,
            "after evaluating, this expression left two values on the stack.",
            ctx.lfile.text
        );
        try ctx.print_messages();

        return util.CompilationFailed;
    }

    return stack[0];
}

/// compiles and runs an expression from text
pub fn evaluate_text(
    ally: Allocator,
    scope: *Scope,
    name: []const u8,
    text: []const u8,
    out_ltype: ?*FlType
) !FlValue {
    // create context
    var lfile = try FlFile.init(ally, name, text);
    defer lfile.deinit(ally);

    var ctx = FlFile.Context.init(ally, &lfile);
    defer ctx.deinit();

    // evaluate
    return try evaluate(&ctx, scope, out_ltype);
}
