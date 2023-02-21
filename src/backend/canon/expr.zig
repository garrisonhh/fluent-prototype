const Object = @import("object.zig");
const TypeWelt = @import("typewelt.zig");
const TypeId = TypeWelt.TypeId;

/// Fluent's AST represented as a Fluent data structure
pub const Expr = Object.Wrapper(struct {
    type: TypeId,
    data: union {
        unit: void,
        bool: bool,
        type: TypeId,
        int: i64,
        float: f64,
    },
});
