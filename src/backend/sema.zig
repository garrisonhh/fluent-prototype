const std = @import("std");
const context = @import("../context.zig");
const types = @import("types.zig");
const Env = @import("env.zig");
const SExpr = @import("sexpr.zig");

const Allocator = std.mem.Allocator;
const Loc = context.Loc;
const TypeId = types.TypeId;
const Type = types.Type;
const Pattern = types.Pattern;

/// really just SExpr with a TypeId.
/// owns all data.
pub const TExpr = struct {
    const Self = @This();

    pub const Tag = SExpr.Tag;

    data: SExpr.Data,
    loc: Loc,
    ty: TypeId,

    fn init(loc: Loc, ty: TypeId, data: SExpr.Data) Self {
        return Self{
            .data = data,
            .loc = loc,
            .ty = ty
        };
    }

    fn from(
        ally: Allocator,
        ty: TypeId,
        sexpr: SExpr
    ) Allocator.Error!Self {
        return Self{
            .data = try sexpr.data.clone(ally),
            .loc = sexpr.loc,
            .ty = ty
        };
    }

    pub fn deinit(self: Self, ally: Allocator) void {
        switch (self.data) {
            .call => |children| {
                for (children) |child| child.deinit(ally);
                ally.free(children);
            },
            .string, .symbol => |sym| ally.free(sym.str),
            else => {}
        }
    }

    pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
        return Self{
            .data = try self.data.clone(ally),
            .loc = self.loc,
            .ty = self.ty,
        };
    }
};

pub const SemaError =
    Allocator.Error;

// TODO with expectations
fn typeOfNumber(env: *Env, num: SExpr.Number) SemaError!TypeId {
    return try env.typeset.unify(env.ally, Type{
        .number = .{
            .bits = num.bits orelse 64,
            .layout = switch(num.data) {
                .int => .int,
                .float => .float,
            },
        }
    });
}

pub fn analyze(env: *Env, expr: SExpr, expects: Pattern) SemaError!TExpr {
    _ = expects;

    const right_prop = switch (expr.data) {
        .number => try typeOfNumber(env, expr.data.number),
        else => @panic("TODO more analysis")
    };

    // TODO expect type to match pattern

    return try TExpr.from(env.ally, right_prop, expr);
}
