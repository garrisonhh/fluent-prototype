pub const Env = @import("backend/env.zig");
usingnamespace @import("backend/lang.zig");
usingnamespace @import("backend/fluent.zig");
usingnamespace @import("backend/ir.zig");
usingnamespace @import("backend/sema.zig");

// TODO should be able to reduce many of the above imports to just this one
usingnamespace @import("backend/run.zig");