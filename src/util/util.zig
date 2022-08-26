const std = @import("std");
const builtin = @import("builtin");

const Allocator = std.mem.Allocator;

/// used by different stages of compiler to indicate stopping compilation and
/// displaying context messages
pub const CompileFailure = error {CompilationFailed};
pub const CompilationFailed = CompileFailure.CompilationFailed;

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

/// intersperses a list of formattable items with sep, and writes with bookends
pub fn write_join(
    start: []const u8,
    sep: []const u8,
    end: []const u8,
    list: anytype,
    writer: anytype
) @TypeOf(writer).Error!void {
    try writer.writeAll(start);
    if (list.len > 0) {
        try writer.print("{}", .{list[0]});
        for (list[1..]) |item| try writer.print("{s}{}", .{sep, item});
    }
    try writer.writeAll(end);
}

/// takes a value and shallow copies it onto an allocator
pub fn place_on(
    ally: Allocator,
    value: anytype
) Allocator.Error!*@TypeOf(value) {
    const ptr = try ally.create(@TypeOf(value));
    ptr.* = value;
    return ptr;
}