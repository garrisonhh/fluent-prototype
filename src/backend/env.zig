//! env manages the state of fluent's dynamic backend runtime.
//!
//! fluent's backend runtime consists of several components:
//! - TypeWelt
//!   - this is the context for managing fluent types. there are two canonical
//!     representations a fluent type can take, the Type and the TypeId.
//!     - Type is a tagged union as you would expect, a structured type repr
//!     - TypeId is a handle which acts like a `*const Type` when paired with
//!       the TypeWelt
//!   - TypeWelt is the entire set of Types that have and will ever exist in a
//!     fluent program
//!     - it also stores some metadata, like typenames
//! - ReprWelt
//!   - pairs with TypeWelt, providing a lower-level unified understanding of
//!     fluent's data representation
//! - NameMap
//!   - this is an associative map including all of the names that have ever
//!     existed in a fluent program

const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");
const Symbol = com.Symbol;
const Name = com.Name;
const NameMap = com.NameMap;
const Loc = com.Loc;
const kz = @import("kritzler");
// const ssa = @import("ssa/ssa.zig");
// const SsaProgram = ssa.Program;
// const SsaRef = ssa.FuncRef;
// const compile = @import("compile.zig");
// const BcBuilder = compile.Builder;
// const BcRef = compile.InstRef;
// const Vm = @import("bytecode/vm.zig");
// const Program = @import("bytecode/bytecode.zig").Program;
const canon = @import("canon.zig");
const Object = canon.Object;
const Type = canon.Type;
const TypeId = canon.TypeId;
const TypeWelt = canon.TypeWelt;
const Repr = canon.Repr;
const ReprId = canon.ReprId;
const ReprWelt = canon.ReprWelt;

const Self = @This();

pub const ROOT = Name.ROOT;

const VM_STACK_SIZE = 32 * 1024;

ally: Allocator,
tw: TypeWelt = .{},
rw: ReprWelt = .{},
/// env owns everything in every binding
nmap: NameMap(Object) = .{},
// these store the lowered + compiled forms of functions
// prog: SsaProgram = .{},
// bc: BcBuilder = .{},
// vm: Vm,
// mapping ssa to bytecode and back
// compiled: std.AutoHashMapUnmanaged(SsaRef, BcRef) = .{},
// lowered: std.AutoHashMapUnmanaged(BcRef, SsaRef) = .{},

pub fn init(ally: Allocator) Allocator.Error!Self {
    return Self{
        .ally = ally,
        // .vm = try Vm.init(ally, VM_STACK_SIZE),
    };
}

pub fn deinit(self: *Self) void {
    var objects = self.nmap.map.valueIterator();
    while (objects.next()) |obj| obj.deinit(self);

    self.tw.deinit(self.ally);
    self.rw.deinit(self.ally);
    self.nmap.deinit(self.ally);
    // self.prog.deinit(self.ally);
    // self.bc.deinit(self.ally);
    // self.vm.deinit(self.ally);
    // self.compiled.deinit(self.ally);
    // self.lowered.deinit(self.ally);
}

// objects =====================================================================

/// searches up through the namespace for a symbol
pub fn seek(self: *Self, scope: Name, sym: Symbol, out_name: ?*Name) ?Object {
    return self.nmap.seek(scope, sym, out_name);
}

/// searches for an exact name
pub fn get(self: *Self, name: Name) Object {
    return self.nmap.get(name);
}

// types and reprs =============================================================

pub fn identify(self: *Self, ty: Type) Allocator.Error!TypeId {
    const id = try self.tw.identify(self.ally, ty);

    _ = self.rw.reprOf(self.ally, self.tw, id) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        else => {},
    };

    return id;
}

pub fn reprOf(self: *Self, ty: TypeId) ReprWelt.Error!ReprId {
    return try self.rw.reprOf(self.ally, self.tw, ty);
}

pub fn sizeOf(self: Self, ty: TypeId) ReprWelt.QualError!usize {
    return try self.rw.sizeOf(self.rw.converts.get(ty).?);
}

// low-level IRs ===============================================================

/// invalidated by adding a new function to ssa
// pub fn getFunc(self: *Self, ref: SsaRef) *ssa.Func {
// return self.prog.get(ref);
// }

/// invalidated by adding a new function to ssa
// pub fn getFuncConst(self: Self, ref: SsaRef) *const ssa.Func {
// return &self.prog.funcs.items[ref.index];
// }

// pub fn removeFunc(self: *Self, ref: SsaRef) Allocator.Error!void {
// try self.bc.removeFunc(self.ally, ref);
// try self.prog.remove(self.ally, ref);
// }

/// calls the compile pipeline for an ssa function and then ties them
// pub fn compileSsa(self: *Self, ref: SsaRef) compile.Error!BcRef {
// std.debug.assert(!self.compiled.contains(ref));

// try compile.compile(self, ref);
// const entry = self.bc.getResolved(ssa.Pos.ofEntry(ref));
// try self.tieLLRefs(ref, entry);

// return entry;
// }

/// if function is not compiled, compiles it. otherwise returns previous BcRef.
// pub fn ensureCompiled(self: *Self, ref: SsaRef) compile.Error!BcRef {
// return if (self.compiled.get(ref)) |bc| bc else try self.compileSsa(ref);
// }

/// ties the ssa and bytecode IR forms together
// pub fn tieLLRefs(
// self: *Self,
// ssa_ref: SsaRef,
// bc_ref: BcRef,
// ) Allocator.Error!void {
// try self.compiled.put(self.ally, ssa_ref, bc_ref);
// try self.lowered.put(self.ally, bc_ref, ssa_ref);
// }

// execution ===================================================================

// pub fn run(
// self: *Self,
// prog: Program,
// loc: ?Loc,
// ty: TypeId,
// ) RunError!Object {
// // find ret_bytes
// const repr = try self.reprOf(ty);
// const conv = self.rw.getConv(repr);

// const ret_bytes: ?usize = switch (conv) {
// .by_value => null,
// .by_ref => try self.rw.sizeOf(repr),
// };

// // exec
// try self.vm.execute(self, prog, ret_bytes);

// // resurrect return value
// const value = canon.intoValue(&self.vm.scratch[Vm.RETURN.n]);

// const ret_ty = switch (conv) {
// .by_value => ty,
// .by_ref => try self.identify(Type.initPtr(.single, ty)),
// };

// const expr = try canon.resurrect(self.*, value, self.vm.stack, loc, ret_ty);

// return switch (conv) {
// .by_value => expr,
// .by_ref => deref: {
// const inner = expr.data.ptr.*;
// self.ally.destroy(expr.data.ptr);

// break :deref inner;
// },
// };
// }

// definitions =================================================================

pub const DefError = com.NameError || TypeWelt.RenameError || Object.InitError;

/// expects value to be owned
pub fn def(self: *Self, scope: Name, sym: Symbol, obj: Object) DefError!Name {
    return self.nmap.put(self.ally, scope, sym, obj);
}

/// expects value to be owned
pub fn redef(self: *Self, name: Name, obj: Object) DefError!void {
    const old = try self.nmap.reput(self.ally, name, obj);
    old.deinit(self.ally);
}

pub fn defNamespace(self: *Self, scope: Name, sym: Symbol) DefError!Name {
    const obj = try Object.fromUnit();
    return self.def(scope, sym, obj);
}

pub fn defType(
    self: *Self,
    scope: Name,
    sym: Symbol,
    value: TypeId,
) DefError!Name {
    const obj = try Object.fromType(self, value);
    const name = try self.def(scope, sym, obj);
    try self.tw.setName(self.ally, value, name);

    return name;
}

// debugging ===================================================================

fn renderName(ctx: *kz.Context, name: Name) !kz.Ref {
    const red = kz.Style{ .fg = .red };

    var list = std.ArrayList(kz.Ref).init(ctx.ally);
    defer list.deinit();

    for (name.syms) |sym| {
        try list.append(try ctx.print(red, "{}", .{sym}));
    }

    const dot = try ctx.print(.{}, ".", .{});
    return try ctx.sep(dot, list.items, .right, .{});
}

pub fn dump(self: *Self, ally: Allocator, writer: anytype) !void {
    var ctx = kz.Context.init(ally);
    defer ctx.deinit();

    // render entries in order
    var decls = std.ArrayList(kz.Ref).init(ally);
    defer decls.deinit();

    const eq = try ctx.print(.{}, ":=", .{});
    defer ctx.drop(eq);

    const entries = try self.nmap.getSortedEntries(ally);
    defer ally.free(entries);
    for (entries) |entry| {
        const decl = try ctx.stack(
            &.{
                try renderName(&ctx, entry.key.*),
                try ctx.clone(eq),
                try entry.value.render(&ctx, self.*),
            },
            .right,
            .{ .space = 1 },
        );

        try decls.append(decl);
    }

    // stack and dump it
    const full_env = try ctx.stack(decls.items, .bottom, .{});
    try ctx.write(full_env, writer);
}
