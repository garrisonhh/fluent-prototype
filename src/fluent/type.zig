const std = @import("std");
const util = @import("../util/util.zig");
const Allocator = std.mem.Allocator;
const WriteError = std.os.WriteError;

pub const FlType = union(enum) {
    const Self = @This();
    pub const Enum = @typeInfo(Self).Union.tag_type.?;

    pub const Function = struct {
        params: []Self,
        returns: *Self
    };

    pub const List = struct {
        subtype: *Self,
    };

    function: Function,
    list: List,
    ltype, // the type `type`
    int,
    float,
    string,
    nil,

    any,

    unknown,

    pub fn init_function(
        ally: Allocator,
        params: []const Self,
        returns: *const Self
    ) Allocator.Error!Self {
        var new_params = try ally.alloc(Self, params.len);
        for (params) |param, i| new_params[i] = try param.clone(ally);

        return Self{
            .function = Function{
                .params = new_params,
                .returns = try util.place_on(ally, try returns.clone(ally)),
            }
        };
    }

    pub fn init_list(
        ally: Allocator,
        subtype: *const Self
    ) Allocator.Error!Self {
        return Self{
            .list = List{
                .subtype = try util.place_on(ally, try subtype.clone(ally))
            }
        };
    }

    pub fn deinit(self: *Self, ally: Allocator) void {
        switch (self.*) {
            .function => |*function| {
                for (function.params) |*param| param.deinit(ally);
                ally.free(function.params);

                function.returns.deinit(ally);
            },
            .list => |list| list.subtype.deinit(ally),
            else => {}
        }
    }

    pub fn eql(self: *const Self, other: *const Self) bool {
        const tag_eql =
            std.meta.activeTag(self.*) == std.meta.activeTag(other.*);

        return tag_eql and switch (self.*) {
            .function => fn_eql: {
                if (self.function.params.len != other.function.params.len
                 or !self.function.returns.eql(other.function.returns)) {
                    break :fn_eql false;
                }

                for (self.function.params) |*param, i| {
                    if (!param.eql(&other.function.params[i])) {
                        break :fn_eql false;
                    }
                }

                break :fn_eql true;
            },
            .list => self.list.subtype.eql(other.list.subtype),
            else => true
        };
    }

    /// whether this type is a subset of the other type
    pub fn matches(self: *const Self, other: *const Self) bool {
        const tag = std.meta.activeTag(self.*);
        const other_tag = std.meta.activeTag(other.*);
        const tag_matches = tag == other_tag or other_tag == .any;

        return tag_matches and switch (self.*) {
            .function => fn_matches: {
                const fun = self.function;
                const other_fun = other.function;

                // check params
                if (fun.params.len != other_fun.params.len) return false;

                for (fun.params) |*param, i| {
                    if (!param.matches(&other_fun.params[i])) return false;
                }

                // check return types
                break :fn_matches fun.returns.matches(other_fun.returns);
            },
            .list => self.list.subtype.matches(other.list.subtype),
            else => true
        };
    }

    /// creates a deep copy of this type
    pub fn clone(self: *const Self, ally: Allocator) Allocator.Error!Self {
        return switch (self.*) {
            .function => |function| Self.init_function(
                ally,
                function.params,
                function.returns
            ),
            .list => |list| Self.init_list(ally, list.subtype),
            else => self.*
        };
    }

    pub fn format(
        self: *const Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        _ = fmt;
        _ = options;

        switch (self.*) {
            .function => |function| {
                try writer.writeAll("(fn [");
                for (function.params) |*param, i| {
                    if (i > 0) try writer.writeByte(' ');
                    try writer.print("{}", .{param});
                }
                try writer.print("] {})", .{function.returns});
            },
            .list => |list| {
                try writer.print("(list {})", .{list.subtype});
            },
            .ltype => try writer.writeAll("type"),
            else => |ltype| try writer.writeAll(@tagName(ltype))
        }
    }
};
