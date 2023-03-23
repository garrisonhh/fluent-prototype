//! forwarding namespaces, various globals

pub const DEBUG = @import("builtin").mode == .Debug;

pub var options = @import("src/options.zig"){};
pub const cli = @import("src/cli.zig");
pub const Symbol = @import("src/symbol.zig");
pub usingnamespace @import("src/idmap.zig");
pub usingnamespace @import("src/result.zig");
pub usingnamespace @import("src/project.zig");
pub usingnamespace @import("src/namemap.zig");
pub usingnamespace @import("src/literals.zig");
pub usingnamespace @import("src/misc.zig");
pub usingnamespace @import("src/strings.zig");
