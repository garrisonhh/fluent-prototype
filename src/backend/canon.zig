//! canon provides data structures and behavior relied upon throughout the
//! compiler runtime

pub const Number = @import("canon/number.zig");
pub const Builtin = @import("canon/builtins.zig").Builtin;
pub const TypeWelt = @import("canon/typewelt.zig");
pub const TypeId = TypeWelt.TypeId;
pub const Type = @import("canon/type.zig").Type;
pub const ReprWelt = @import("canon/reprwelt.zig");
pub const ReprId = ReprWelt.ReprId;
pub const Repr = @import("canon/repr.zig").Repr;
pub const Object = @import("canon/object.zig");
pub const Expr = @import("canon/expr.zig").Expr;
pub usingnamespace @import("canon/prelude.zig");
pub const Image = @import("canon/image.zig");
pub const Ptr = Image.Ptr;
