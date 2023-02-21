const Object = @import("object.zig");
const TypeWelt = @import("typewelt.zig");
const TypeId = TypeWelt.TypeId;

pub const ExprTemplate = struct {
    type: TypeId,
    data: union(enum) {
        unit: void,
        bool: bool,
        type: TypeId,
        uint: u64,
        int: i64,
        float: f64,
    },
};

/// Fluent's AST represented as a Fluent data structure
pub const Expr = Object.Wrapper(ExprTemplate);
