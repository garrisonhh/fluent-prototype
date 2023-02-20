const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ZType = std.builtin.Type;
const Object = @import("object.zig");
const Repr = @import("repr.zig").Repr;
const ReprWelt = @import("reprwelt.zig");
const builtin = @import("builtin");

const InterfaceField = struct {
    @"0": @TypeOf(.EnumLiteral),
    @"1": type,
};

fn InterfaceEnum(comptime fields: []const InterfaceField) type {
    var tag_fields: [fields.len]ZType.EnumField = undefined;
    for (fields) |field, i| {
        tag_fields[i] = ZType.EnumField{
            .name = @tagName(field.@"0"),
            .value = i,
        };
    }

    return @Type(ZType{
        .Enum = ZType.Enum{
            .layout = .Auto,
            .tag_type = usize,
            .fields = &tag_fields,
            .decls = &.{},
            .is_exhaustive = true,
        },
    });
}

/// for constructing Variant views
fn InterfaceVariant(comptime fields: []const InterfaceField) type {
    const ZEnum = InterfaceEnum(fields[1..]);

    // variant fields
    var vfields: [fields.len - 1]ZType.UnionField = undefined;
    for (fields[1..]) |field, i| {
        vfields[i] = ZType.UnionField{
            .name = @tagName(field.@"0"),
            .field_type = field.@"1",
            .alignment = @alignOf(field.@"1"),
        };
    }

    return @Type(ZType{
        .Union = ZType.Union{
            .layout = .Auto,
            .tag_type = ZEnum,
            .fields = &vfields,
            .decls = &.{},
        },
    });
}

/// constructs a thin wrapper interface over Object collections
pub fn Interface(comptime fields: []const InterfaceField) type {
    return struct {
        const Self = @This();

        pub const Tag = InterfaceEnum(fields);
        pub fn Field(comptime tag: Tag) type {
            return fields[@enumToInt(tag)].@"1";
        }

        pub fn getPtr(
            base: *anyopaque,
            repr_fields: []const Repr.Field,
            comptime tag: Tag,
        ) *Field(tag) {
            const index = @enumToInt(tag);
            const offset = repr_fields[index].offset;
            const raw_addr = @ptrToInt(base) + offset;

            return @intToPtr(*Field(tag), raw_addr);
        }

        pub fn get(
            base: *anyopaque,
            repr_fields: []const Repr.Field,
            comptime tag: Tag,
        ) Field(tag) {
            return getPtr(base, repr_fields, tag).*;
        }

        pub fn set(
            base: *anyopaque,
            repr_fields: []const Repr.Field,
            comptime tag: Tag,
            to: Field(tag),
        ) void {
            getPtr(base, repr_fields, tag).* = to;
        }

        pub const Variant = InterfaceVariant(fields);

        /// interpret this object as a variant
        pub fn asVariant(
            base: [*]anyopaque,
            repr_fields: []const Repr.Field,
        ) Variant {
            const tag_index: usize = get(base, repr_fields, @intToEnum(Tag, 0));

            inline for (fields) |_, i| {
                if (i == tag_index) {
                    const tag = @intToEnum(Tag, tag_index + 1);
                    const tagname = @tagName(tag);
                    return @unionInit(
                        Variant,
                        tagname,
                        get(base, repr_fields, tag),
                    );
                }
            } else {
                std.debug.panic(
                    "attempted to nonexistant variant field {}",
                    .{tag_index},
                );
            }
        }
    };
}
