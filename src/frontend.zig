const frontend_parse = @import("frontend/parse.zig");
const sema = @import("frontend/sema.zig");
pub const Expr = @import("frontend/expr.zig");

pub const Ast = frontend_parse.Ast;
pub const Scope = sema.Scope;
pub const parse = frontend_parse.parse;
pub const analyze = sema.analyze;