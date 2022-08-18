const sema = @import("backend/sema.zig");

pub const SemaContext = sema.Context;
pub const SemaError = sema.Error;
pub const Scope = sema.Scope;
pub const type_infer = sema.type_infer;

const dynamic = @import("backend/dynamic.zig");

pub const FlBlock = dynamic.FlBlock;
pub const FlVm = dynamic.FlVm;
pub const compile = dynamic.compile;
pub const compile_run = dynamic.compile_run;
pub const eval = dynamic.eval;