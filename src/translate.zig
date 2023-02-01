//! 'translation' is the process of turning RawExprs from the frontend into
//! well-formed SExprs in the backend.

const std = @import("std");
const Allocator = std.mem.Allocator;
const util = @import("util");
const Symbol = util.Symbol;
const Project = util.Project;
const Message = util.Message;
const Loc = util.Loc;
const RawExpr = @import("frontend.zig").RawExpr;
const SExpr = @import("backend.zig").SExpr;

const Result = Message.Result(SExpr);

fn numberError(ally: Allocator, loc: Loc) Allocator.Error!Result {
    return try Message.err(ally, SExpr, loc, "malformed number literal", .{});
}

fn translateNumber(
    ally: Allocator,
    loc: Loc,
    slice: []const u8,
) Allocator.Error!Result {
    const num = util.parseNumber(ally, slice) catch |e| switch (e) {
        Allocator.Error.OutOfMemory => return Allocator.Error.OutOfMemory,
        else => return try numberError(ally, loc)
    };
    defer num.deinit(ally);

    // validate bits
    if (num.bits) |bits| {
        const integral = num.layout.? != .float;
        const valid_bits: []const u8 =
            if (integral) &[_]u8{8, 16, 32, 64} else &[_]u8{32, 64};

        for (valid_bits) |valid| {
            if (bits == valid) break;
        } else {
            const text = "invalid number width";
            return try Message.err(ally, SExpr, loc, text, .{});
        }
    }

    const sexpr_num = SExpr.Number.from(num) catch {
        return try numberError(ally, loc);
    };

    return Result.ok(SExpr{
        .loc = loc,
        .data = .{ .number = sexpr_num },
    });
}

fn translateCall(
    ally: Allocator,
    proj: Project,
    loc: Loc,
    head: ?SExpr,
    rexprs: []const RawExpr,
) Allocator.Error!Result {
    var sexprs = try std.ArrayList(SExpr).initCapacity(
        ally,
        rexprs.len + @boolToInt(head != null),
    );
    errdefer {
        for (sexprs.items) |child| child.deinit(ally);
        sexprs.deinit();
    }

    if (head) |got| {
        sexprs.appendAssumeCapacity(got);
    }

    for (rexprs) |child| {
        switch (try translate(ally, proj, child)) {
            .ok => |got| sexprs.appendAssumeCapacity(got),
            .err => |msg| {
                for (sexprs.items) |sexpr| sexpr.deinit(ally);
                sexprs.deinit();

                return Result.err(msg);
            }
        }
    }

    return Result.ok(SExpr.init(loc, .{ .call = sexprs.toOwnedSlice() }));
}

pub fn translate(
    ally: Allocator,
    proj: Project,
    rexpr: RawExpr,
) Allocator.Error!Result {
    const loc = rexpr.loc;
    const exprs = rexpr.exprs;

    return switch (rexpr.form) {
        // straightforward translation
        .unit => Result.ok(SExpr.init(loc, .{ .call = &.{} })),
        inline .string, .symbol => |tag| s: {
            const slice = try ally.dupe(u8, loc.slice(proj));
            break :s Result.ok(SExpr{
                .loc = loc,
                .data = @unionInit(
                    SExpr.Data,
                    @tagName(tag),
                    Symbol.init(slice),
                ),
            });
        },
        .number => try translateNumber(ally, loc, loc.slice(proj)),
        .call => try translateCall(ally, proj, loc, null, exprs),

        // sugar should have been desugared
        .parens, .comma, .kv, .dict, .stmt => unreachable,

        // translate to call with head
        else => |tag| tagged: {
            const head_sym = tag.builtin() orelse {
                return Result.err(try Message.print(
                    ally,
                    .internal,
                    loc,
                    "attempted to get builtin for form `{s}`, but failed",
                    .{@tagName(tag)}
                ));
            };
            const head = SExpr.init(loc, .{
                .symbol = try head_sym.clone(ally)
            });

            break :tagged try translateCall(ally, proj, loc, head, exprs);
        },
    };
}