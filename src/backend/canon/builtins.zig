pub const Builtin = enum {
    // this is a transient value that only exists during the first sema pass
    // over defs in a namespace (a.k.a while the pie crust is baking)
    pie_stone,

    // pure syntax
    ns,
    def,

    // data
    array,
    tuple,

    // control flow
    lambda,
    // TODO remove this
    recur,
    do,
    @"if",

    // value operators
    cast,
    addr_of,
    access,

    eq,
    add,
    sub,
    mul,
    div,
    mod,
    shl,
    shr,

    @"and",
    @"or",
    not,

    // type operators
    slice_ty,
    fn_ty,
    tuple_ty,
};
