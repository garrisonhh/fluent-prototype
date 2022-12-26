pub const Builtin = enum {
    // pure syntax
    ns,

    // control flow
    do,
    @"if",

    // operators
    list,
    cast,

    add,
    sub,
    mul,
    div,
    mod,

    @"and",
    @"or",
    not,

    /// what this builtin is named as it exists in the prelude. returns null
    /// for operators that shouldn't exist in the prelude.
    pub fn getName(b: Builtin) []const u8 {
        return switch (b) {
            .add => "+",
            .sub => "-",
            .mul => "*",
            .div => "/",
            .mod => "%",
            // `as` is an explicit cast, casts are also created implicitly
            .cast => "as",
            inline else => |tag| @tagName(tag),
        };
    }
};
