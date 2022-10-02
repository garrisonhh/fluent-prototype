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

/// coerces a tuple to a struct. for cautious use in metaprogramming.
/// example: `coerce_tuple(struct{ a: i32, b: i32, c: i32}, .{0, 1, 2})` works!
pub fn coerce_tuple(comptime S: type, tuple: anytype) S {
    const struct_fields = @typeInfo(S).Struct.fields;
    const tuple_info = @typeInfo(@TypeOf(tuple)).Struct;

    comptime {
        if (!tuple_info.is_tuple) {
            @compileError("coerce_tuple error: tuple arg is not a tuple.");
        } else if (struct_fields.len != tuple_info.fields.len) {
            @compileError("coerce_tuple error: tuple arg does not have the "
                       ++ "same number of fields as struct arg.");
        }
    }

    var s: S = undefined;
    inline for (struct_fields) |field, i| {
        @field(s, field.name) = @as(field.field_type, tuple[i]);
    }

    return s;
}

/// very similar to std.EnumArray, but works much more like an X table, which
/// is almost always what I want.
pub fn EnumTable(comptime E: type, comptime V: type) type {
    const enum_info = @typeInfo(E).Enum;
    const enum_size = enum_info.fields.len;

    comptime {
        if (!enum_info.is_exhaustive) {
            @compileError("EnumTable error: key enum must be exhaustive.");
        }
    }

    return struct {
        const Self = @This();

        pub const Entry = struct {
            key: E,
            value: V
        };

        values: [enum_size]V,

        /// expects an array of (key, value) tuples. fully comptime.
        pub fn init(comptime rows: anytype) Self {
            comptime {
                // coerce entries to an array of Self.Entry
                var entries: [rows.len]Entry = undefined;
                for (rows) |row, i| {
                    const value = if (@TypeOf(row[1]) == V) row[1]
                                  else coerce_tuple(V, row[1]);

                    entries[i] = Entry{
                        .key = row[0],
                        .value = value,
                    };
                }

                // verify entries is a pure set of keys
                var has_entry: [enum_size]bool = undefined;
                std.mem.set(bool, &has_entry, false);

                inline for (entries) |entry| {
                    const index = @enumToInt(entry.key);
                    if (has_entry[index]) {
                        @compileError("EnumTable error: key "
                                    ++ @tagName(entry.key)
                                    ++ " has duplicate entries.");
                    }

                    has_entry[index] = true;
                }

                for (has_entry) |exists, i| {
                    if (!exists) {
                        @compileError("EnumTable error: key "
                                   ++ @tagName(@intToEnum(E, i))
                                   ++ " does not have any entries.");
                    }
                }

                // fill table
                var self: Self = undefined;
                for (entries) |entry| {
                    self.values[@enumToInt(entry.key)] = entry.value;
                }

                return self;
            }
        }

        pub fn get(self: *const Self, key: E) V {
            return self.values[@enumToInt(key)];
        }
    };
}

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