//! forwarding namespaces

pub var options = @import("src/options.zig"){};
pub const cli = @import("src/cli.zig");
pub const Symbol = @import("src/symbol.zig");
pub usingnamespace @import("src/result.zig");
pub usingnamespace @import("src/project.zig");
pub usingnamespace @import("src/namemap.zig");
pub usingnamespace @import("src/literals.zig");
pub usingnamespace @import("src/misc.zig");
pub usingnamespace @import("src/strings.zig");
