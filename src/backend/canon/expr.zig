const std = @import("std");
const Allocator = std.mem.Allocator;
const Object = @import("object.zig");
const TypeWelt = @import("typewelt.zig");
const TypeId = TypeWelt.TypeId;
const Repr = @import("repr.zig").Repr;
const Env = @import("../env.zig");
const prelude = @import("prelude.zig");
const Basic = prelude.Basic;

const FlExprData = Object.Interface(&.{
    // variant tag
    .{ .tag, usize },
    // variant values
    .{ .unit, void },
    .{ .bool, bool },
    .{ .type, TypeId },
    .{ .int, i64 },
    .{ .float, f64 },
});

const FlExpr = Object.Interface(&.{
    .{ .type, TypeId },
    .{ .data, opaque {} },
});

fn exprFields() []const Repr.Field {
    return prelude.EXPR_REPR_FIELDS;
}

fn exprDataFields() []const Repr.Field {
    return prelude.EXPR_DATA_REPR_FIELDS;
}

/// Fluent's AST represented as a Fluent data structure. this is actually a
/// pretty heavily metaprogrammed wrapper over a bunch of bytes organized
/// in the correct way.
pub const Expr = struct {
    const Self = @This();

    pub const Data = FlExprData.Variant;

    obj: Object,

    /// allocate a new object
    pub fn init(env: *Env, ty: TypeId, data: Data) Object.InitError!Self {
        const expr_ty = Basic.expr.get();
        const self = Self{ .obj = try Object.init(env, expr_ty) };

        FlExpr.set(self.base(), exprFields(), .type, ty);
        FlExprData.set(
            self.baseData(),
            exprDataFields(),
            .tag,
            @enumToInt(data),
        );

        switch (data) {
            inline else => |payload, tag| {
                FlExprData.set(
                    self.baseData(),
                    exprDataFields(),
                    @field(FlExprData.Tag, @tagName(tag)),
                    payload,
                );
            },
        }

        std.debug.print("initialized expr\n", .{});
        return self;
    }

    pub fn deinit(self: Self, env: *Env) void {
        self.obj.deinit(env);
    }

    pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
        return Self{ .obj = try self.obj.clone(ally) };
    }

    fn base(self: Self) *anyopaque {
        return self.obj.basePtr();
    }

    fn baseData(self: Self) *anyopaque {
        return FlExpr.getPtr(self.base(), exprFields(), .data);
    }

    pub fn getType(self: Self) TypeId {
        return FlExpr.get(self.base(), exprFields(), .type);
    }

    pub fn getData(self: Self) Data {
        return FlExprData.asVariant(self.baseData(), exprDataFields());
    }
};
