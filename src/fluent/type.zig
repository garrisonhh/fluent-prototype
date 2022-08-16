const std = @import("std");
const util = @import("../util/util.zig");
const Allocator = std.mem.Allocator;
const WriteError = std.os.WriteError;

pub const FlType = union(enum) {
    const Self = @This();

    pub const Function = struct {
        params: []const Self,
        returns: *const Self
    };

    pub const List = struct {
        subtype: *const Self,
    };

    function: Function,
    list: List,
    ltype, // the type of the expression is itself a type
    int,
    float,
    string,
    nil,

    builtin,
    unknown,

    pub fn init_function(params: []const Self, returns: *const Self) Self {
        return Self{
            .function = Function{
                .params = params,
                .returns = returns
            }
        };
    }

    pub fn init_list(subtype: *const Self) Self{
        return Self{
            .list = List{ .subtype = subtype }
        };
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

    /// creates a deep copy of this type
    pub fn clone(self: *const Self, ally: Allocator) Allocator.Error!Self {
        return switch (self.*) {
            .function => |function| clone_function: {
                var params = try ally.alloc(Self, function.params.len);
                for (function.params) |param, i| {
                    params[i] = try param.clone(ally);
                }

                const returns = try function.returns.create_clone(ally);
                break :clone_function Self.init_function(params, returns);
            },
            .list => |list| Self.init_list(try list.subtype.create_clone(ally)),
            else => self.*
        };
    }

    pub fn create_clone(self: *const Self, allocator: Allocator) !*Self {
        var copy = try allocator.create(Self);
        copy.* = try self.clone(allocator);

        return copy;
    }

    const Fmt = struct {
        ltype: *const FlType,

        fn of(ltype: *const FlType) Fmt {
            return Fmt{ .ltype = ltype };
        }

        pub fn format(
            fmt_r: *const Fmt,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype
        ) @TypeOf(writer).Error!void {
            _ = fmt;
            _ = options;

            switch (fmt_r.ltype.*) {
                .function => |function| {
                    try writer.writeAll("(fn [");
                    for (function.params) |*param, i| {
                        if (i > 0) try writer.writeByte(' ');
                        try writer.print("{}", .{Fmt.of(param)});
                    }
                    try writer.print("] {})", .{Fmt.of(function.returns)});
                },
                .list => |list| {
                    try writer.print("(list {})", .{Fmt.of(list.subtype)});
                },
                .ltype => try writer.writeAll("type"),
                else => |ltype| try writer.writeAll(@tagName(ltype))
            }
        }
    };

    pub fn format(
        self: *const Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        _ = fmt;
        _ = options;

        try std.fmt.format(writer, "<{}>", .{Fmt.of(self)});
    }
};
