const std = @import("std");
const builtin = @import("builtin");

pub const FmtError = std.os.WriteError
                  || std.fmt.AllocPrintError
                  || std.fmt.BufPrintError;

pub const Error = error {
    WrongOrderedBookendSlices,
};

/// given two slices that are the start and end of some memory block (such as
/// two tokens at the beginning and end of a file) return a slice that contains
/// both slices.
pub fn slice_from_bookends(a: anytype, b: @TypeOf(a)) Error!@TypeOf(a) {
    // ensure `a` is a slice
    comptime {
        const info = @typeInfo(@TypeOf(a));
        std.debug.assert(info == .Pointer and info.Pointer.size == .Slice);
    }

    if (comptime builtin.mode == .Debug) {
        if (@ptrToInt(b.ptr) < @ptrToInt(a.ptr)) {
            return Error.WrongOrderedBookendSlices;
        }
    }

    return a.ptr[0..@ptrToInt(b.ptr) - @ptrToInt(a.ptr) + b.len];
}