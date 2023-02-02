//! fluent specific cli options with global accessibility

log: struct {
    translate: bool = false,
    sema: bool = false,
    ssa: bool = false,
    bytecode: bool = false,
    eval: bool = false,
} = .{},
