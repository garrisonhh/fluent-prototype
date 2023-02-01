//! 'translation' is the process of turning RawExprs from the frontend into
//! well-formed SExprs in the backend.

const std = @import("std");
const Allocator = std.mem.Allocator;
const util = @import("util");
const Symbol = util.Symbol;
const Project = util.Project;
const Message = util.Message;
const RawExpr = @import("frontend.zig").RawExpr;
const SExpr = @import("backend.zig").SExpr;

const Result = Message.Result(SExpr);

pub fn translate(
    ally: Allocator,
    proj: Project,
    rexpr: RawExpr,
) Allocator.Error!Result {
    const loc = rexpr.loc;
    const exprs = rexpr.exprs;

    const sexpr = switch (rexpr.form) {
        // straightforward translation
        .unit => SExpr.of(loc, .{ .call = &.{} }),
        .call => call: {
            const translated = try ally.alloc();
            errdefer {
                for (translated) |child| child.deinit(ally);
                ally.free(translated);
            }

            for (exprs) |child, i| {
                translated[i] = try translate(ally, proj, child);
            }

            break :call SExpr.of(loc, .{ .call = translated });
        },
        .string => SExpr.of(
            loc,
            .{ .string = try ally.dupe(u8, loc.slice(proj)) },
        ),
        .symbol => SExpr.of(
            loc,
            .{ .symbol = try Symbol.init(loc.slice(proj)).clone(ally) },
        ),

        // translate to call with head
        .array,
        .add, .sub, .mul, .div, .mod,
        .eq, .gt, .lt, .ge, .le,
        => |tag| {
            _ = tag;
            @panic("TODO");
        },

        // sugar should have been desugared
        .parens, .comma, .kv, .dict, .stmt => unreachable,
    };

    return Result.ok(sexpr);
}
