pub const Builtin = enum {
    // this is a transient value that only exists during the first sema pass
    // over defs in a namespace (a.k.a while the pie crust is baking)
    pie_stone,

    // pure syntax
    ns,
    def,

    // control flow
    do,
    @"if",

    // operator
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
    pub fn getName(b: Builtin) ?[]const u8 {
        return switch (b) {
            .pie_stone, .list => null,
            .add => "+",
            .sub => "-",
            .mul => "*",
            .div => "/",
            .mod => "%",
            // `as` is an explicit cast, casts are also created implicitly
            .cast => "as",
            else => |tag| @tagName(tag),
        };
    }
};
