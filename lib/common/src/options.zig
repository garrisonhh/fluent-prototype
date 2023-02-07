//! fluent specific cli options with global accessibility

log: struct {
    parse: bool = false,
    translate: bool = false,
    sema: bool = false,
    ssa: bool = false,
    bytecode: bool = false,
    eval: bool = false,
} = .{},
