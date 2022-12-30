pub const Builtin = enum {
    // this is a transient value that only exists during the first sema pass
    // over defs in a namespace (a.k.a while the pie crust is baking)
    pie_stone,

    // pure syntax
    ns,
    def,

    // control flow
    @"fn",
    do,
    @"if",

    // value operators
    cast,

    add,
    sub,
    mul,
    div,
    mod,

    @"and",
    @"or",
    not,

    // type operators
    slice_ty,
    fn_ty,
};
