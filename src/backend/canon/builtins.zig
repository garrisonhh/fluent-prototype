/// individual values that have special semantics that the compiler needs to
/// understand during sema
pub const Builtin = enum(u64) {
    // pure syntax
    ns,
    def,

    // data constructors
    array,
    tuple,

    // data manipulation
    cast,
    addr_of,
    array_ptr_to_slice,
    access,

    // control flow
    lambda,
    do,
    @"if",

    // value operators
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
