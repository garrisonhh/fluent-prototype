//! env manages the state of fluent's dynamic backend runtime.
//!
//! fluent's backend runtime consists of two components:
//! - the TypeWelt
//!   - this is the context for managing fluent types. there are two canonical
//!     representations a fluent type can take, the Type and the TypeId.
//!     - Type is a tagged union as you would expect, a structured type repr
//!     - TypeId is a handle which acts like a `*const Type` when paired with
//!       the TypeWelt
//!   - TypeWelt is the entire set of Types that have and will ever exist in a
//!     fluent program
//!     - it also stores some metadata, like typenames
//! - the NameMap
//!   - this is an associative map including all of the names that have and will
//!     ever exist in a fluent program

const std = @import("std");
const Allocator = std.mem.Allocator;
const util = @import("util");
const Symbol = util.Symbol;
const Name = util.Name;
const NameMap = util.NameMap;
const kz = @import("kritzler");
const types = @import("types.zig");
const TypeWelt = types.TypeWelt;
const TypeId = types.TypeId;
const Type = types.Type;
const TExpr = @import("texpr.zig");
const ssa = @import("ssa.zig");
const Program = ssa.Program;

const Self = @This();

pub const ROOT = Name.ROOT;

ally: Allocator,
tw: TypeWelt = .{},
// env owns everything in every binding
nmap: NameMap(TExpr) = .{},
// stores lowered functions
prog: Program = .{},

pub fn init(ally: Allocator) Allocator.Error!Self {
    return Self{
        .ally = ally,
    };
}

pub fn deinit(self: *Self) void {
    self.tw.deinit(self.ally);
    self.nmap.deinit(self.ally);
    self.prog.deinit(self.ally);
}

pub fn identify(self: *Self, ty: Type) Allocator.Error!TypeId {
    return self.tw.identify(self.ally, ty);
}

// accessors ===================================================================

/// searches up through the namespace for a symbol
pub fn seek(self: *Self, scope: Name, sym: Symbol, out_name: ?*Name) ?TExpr {
    return self.nmap.seek(scope, sym, out_name);
}

/// searches for an exact name
pub fn get(self: *Self, name: Name) TExpr {
    return self.nmap.get(name);
}

// definitions =================================================================

pub const DefError = util.NameError || TypeWelt.RenameError;

/// expects value to be owned
pub fn def(self: *Self, scope: Name, sym: Symbol, value: TExpr) DefError!Name {
    return self.nmap.put(self.ally, scope, sym, value);
}

/// expects value to be owned
pub fn redef(self: *Self, name: Name, value: TExpr) DefError!void {
    const old = try self.nmap.reput(self.ally, name, value);
    old.deinit(self.ally);
}

pub fn defNamespace(self: *Self, scope: Name, sym: Symbol) DefError!Name {
    const nsty = try self.identify(Type{ .namespace = {} });
    // TODO what the fuck is stored in the namespace's value?
    const expr = TExpr.init(null, nsty, .{ .unit = {} });
    return self.def(scope, sym, expr);
}

pub fn defType(
    self: *Self,
    scope: Name,
    sym: Symbol,
    value: TypeId
) DefError!Name {
    const tyty = try self.identify(Type{ .ty = {} });
    const expr = TExpr.init(null, tyty, .{ .ty = value });
    const name = try self.def(scope, sym, expr);
    try self.tw.setName(self.ally, value, name);

    return name;
}

// debugging ===================================================================

fn renderName(ctx: *kz.Context, name: Name) !kz.Ref {
    var list = std.ArrayList(kz.Ref).init(ctx.ally);
    defer list.deinit();

    const into = try ctx.print(.{}, "::", .{});
    defer ctx.drop(into);

    for (name.syms) |sym, i| {
        if (i > 0) try list.append(try ctx.clone(into));

        const sym_ref = try ctx.print(.{ .fg = .red }, "{}", .{sym});
        try list.append(sym_ref);
    }

    return try ctx.stack(list.items, .right, .{});
}

pub fn dump(self: *Self, ally: Allocator, writer: anytype) !void {
    var ctx = kz.Context.init(ally);
    defer ctx.deinit();

    // render entries in order
    var decls = std.ArrayList(kz.Ref).init(ally);
    defer decls.deinit();

    const eq = try ctx.print(.{}, " = ", .{});
    defer ctx.drop(eq);

    const entries = try self.nmap.getSortedEntries(ally);
    defer ally.free(entries);
    for (entries) |entry| {
        const name = try renderName(&ctx, entry.key.*);
        const pred = try ctx.slap(name, try ctx.clone(eq), .right, .{});
        const value = try entry.value.render(&ctx, self.tw);
        const decl = try ctx.slap(pred, value, .right, .{});

        try decls.append(decl);
    }

    // stack and dump it
    const full_env = try ctx.stack(decls.items, .bottom, .{});
    try ctx.write(full_env, writer);
}