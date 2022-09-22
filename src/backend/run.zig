//! run contains the high-level implementation of running a fluent program
//! from its first SExprs to its last

const std = @import("std");
const fluent = @import("fluent.zig");
const frontend = @import("../frontend.zig");
const sema = @import("sema.zig");
const ir = @import("ir.zig");
const Env = @import("env.zig");

const Allocator = std.mem.Allocator;
const TypedExpr = sema.TypedExpr;
const Type = fluent.Type;
const Value = fluent.Value;
const AstExpr = frontend.AstExpr;

/// in-progress data structure and functionality for name resolution
const NameResolver = struct {
    const Self = @This();
    const SymbolSet = std.StringHashMapUnmanaged(void);

    /// data for a def as it is in-progress
    const Def = union(enum) {
        untyped: struct {
            symbol: []const u8,
            type_expr: *const AstExpr,
            body_expr: *const AstExpr,
        },
        typed: struct {
            symbol: []const u8,
            stype: Type, // owned
            body_expr: *const AstExpr,
        },
    };

    const DependentDef = struct {
        def: Def,
        deps: SymbolSet
    };

    ally: Allocator,
    ns: SymbolSet,
    queue: std.StringHashMapUnmanaged(DependentDef),
    ready: std.ArrayListUnmanaged(Def),

    fn init(ally: Allocator) Self {
        return Self{
            .ally = ally,
            .ns = .{},
            .queue = .{},
            .ready = .{},
        };
    }

    fn deinit(self: *Self) void {
        self.ns.deinit(self.ally);
        self.queue.deinit(self.ally);
        self.ready.deinit(self.ally);
    }

    fn fill_deps(
        self: Self,
        ast_expr: AstExpr,
        deps: *SymbolSet
    ) anyerror!void {
        if (ast_expr.etype == .symbol) {
            if (self.ns.contains(ast_expr.slice)) {
                try deps.put(self.ally, ast_expr.slice, .{});
            }
        } else if (ast_expr.children) |children| {
            for (children) |child| try self.fill_deps(child, deps);
        }
    }

    /// first step after initialization
    /// add all the defs, return the rest
    fn filter_add_exprs(
        self: *Self,
        unfiltered_exprs: []const AstExpr
    ) ![]AstExpr {
        var non_defs = std.ArrayListUnmanaged(AstExpr){};

        for (unfiltered_exprs) |ast_expr| {
            var is_def: bool = false;
            if (ast_expr.etype == .call) {
                const children = ast_expr.children.?;
                if (children.len > 0 and children[0].etype == .symbol
                and std.mem.eql(u8, "def", children[0].slice)) {
                    is_def = true;
                }
            }

            if (is_def) {
                const children = ast_expr.children.?;
                if (children.len != 4 or children[1].etype != .symbol) {
                    return error.BadDef;
                }

                // transform def
                const symbol = children[1].slice;
                try self.queue.put(self.ally, symbol, DependentDef{
                    .def = Def{
                        .untyped = .{
                            .symbol = symbol,
                            .type_expr = &children[2],
                            .body_expr = &children[3],
                        }
                    },
                    .deps = .{}
                });

                // add to namespace as well
                try self.ns.put(self.ally, symbol, .{});
            } else {
                try non_defs.append(self.ally, ast_expr);
            }
        }

        // determine dependencies on type exprs
        var ddef_iter = self.queue.valueIterator();
        while (ddef_iter.next()) |ddef| {
            try self.fill_deps(ddef.def.untyped.type_expr.*, &ddef.deps);
        }

        return non_defs.toOwnedSlice(self.ally);
    }

    /// move defs with fully defined dependencies from the dependent queue to
    /// the ready queue
    fn find_independent_defs(self: *Self) !void {
        // find independent defs
        var independent = std.ArrayListUnmanaged([]const u8){};
        defer independent.deinit(self.ally);

        var queue_iter = self.queue.iterator();
        while (queue_iter.next()) |entry| {
            if (entry.value_ptr.deps.count() == 0) {
                try independent.append(self.ally, entry.key_ptr.*);
            }
        }

        // move to ready queue
        for (independent.items) |symbol| {
            var entry = self.queue.fetchRemove(symbol).?;
            const ddef = &entry.value;

            ddef.deps.deinit(self.ally);
            try self.ready.append(self.ally, ddef.def);
        }
    }

    /// figure out what to do next with independent defs
    /// values can go straight to the env, typedefs require 2 rounds of solving
    fn flush_ready_defs(self: *Self, env: *Env) !void {
        for (self.ready.items) |def| {
            switch (def) {
                .untyped => |untyped| {
                    // evaluate type
                    const stype_val = try evaluate(
                        self.ally,
                        env,
                        untyped.type_expr.*,
                        Type{ .stype = {}}
                    );
                    defer stype_val.deinit(self.ally);

                    const stype = stype_val.stype;

                    // determine body deps and place back in queue as typed
                    var new_deps = SymbolSet{};
                    try self.fill_deps(untyped.body_expr.*, &new_deps);

                    try self.queue.put(
                        self.ally,
                        untyped.symbol,
                        DependentDef{
                            .def = Def{
                                .typed = .{
                                    .symbol = untyped.symbol,
                                    .stype = try stype.clone(self.ally),
                                    .body_expr = untyped.body_expr
                                }
                            },
                            .deps = new_deps
                        }
                    );
                },
                .typed => |typed| {
                    const symbol = typed.symbol;

                    // once it's reached this point the type can be executed and
                    // defined
                    const value = try evaluate(
                        self.ally,
                        env,
                        typed.body_expr.*,
                        typed.stype,
                    );
                    defer value.deinit(self.ally);

                    std.debug.print(
                        "solved {s}: {} = {}\n",
                        .{symbol, typed.stype, value}
                    );

                    // define the value
                    try env.define_value(symbol, typed.stype, value);
                    typed.stype.deinit(self.ally);

                    // remove the symbol from the namespace
                    _ = self.ns.remove(symbol);

                    var ddef_iter = self.queue.valueIterator();
                    while (ddef_iter.next()) |ddef| {
                        _ = ddef.deps.remove(symbol);
                    }
                },
            }
        }

        self.ready.shrinkAndFree(self.ally, 0);
    }

    fn solve(self: *Self, env: *Env) !void {
        while (self.queue.count() > 0) {
            try self.find_independent_defs();

            if (self.ready.items.len == 0) return error.CircularDeps;

            try self.flush_ready_defs(env);
        }
    }
};

/// the 'first pass' of semantic analysis. solves type dependencies and defines
/// all defs in the env.
///
/// returns a 'sanitized' program without any defs that can be run sequentially,
/// allocated on ally.
fn resolve_names(
    ally: Allocator,
    env: *Env,
    ast_exprs: []const AstExpr
) ![]AstExpr {
    var solver = NameResolver.init(ally);
    defer solver.deinit();
    const non_defs = solver.filter_add_exprs(ast_exprs);

    try solver.solve(env);

    return non_defs;
}

/// lowers and executes a single immediate expression
pub fn evaluate(
    ally: Allocator,
    env: *Env,
    ast_expr: AstExpr,
    expects: ?Type
) !Value {
    std.debug.assert(ast_expr.etype != .program);

    const expr = try sema.analyze(ally, env.*, ast_expr, expects);

    var block = try ir.lower_expr(ally, env, "expr", expr);
    defer block.deinit(ally);

    return try env.execute(ally, block, &.{});
}

/// runs a program from start to finish, with both a type/name resolution pass/
/// and an evaluation pass
///
/// returns value allocated on ally
pub fn run(
    ally: Allocator,
    env: *Env,
    program: AstExpr
) !Value {
    // function should be fed direct output from frontend
    std.debug.assert(program.etype == .program);

    // do first pass to define all symbols
    const sanitized = try resolve_names(ally, env, program.children.?);
    defer {
        for (sanitized) |ast_expr| ast_expr.deinit(ally);
        ally.free(sanitized);
    }

    // eval sanitized exprs
    var final_value = Value{ .unit = {} };
    for (sanitized) |ast_expr| {
        final_value.deinit(ally);
        final_value = try evaluate(ally, env, ast_expr, null);
    }

    return final_value;
}