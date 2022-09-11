//! contains the essential representations of Fluent data
//! most meaty function here is `SExpr.from_expr` which translates the frontend
//! AST into an SExpr tree.

const std = @import("std");
const util = @import("../util/util.zig");
const frontend = @import("../frontend.zig");
const FlFile = @import("../file.zig");
const Env = @import("env.zig");

const Allocator = std.mem.Allocator;
const Context = FlFile.Context;
const Expr = frontend.Expr;

/// flat type representation
pub const FlatType = enum {
    const Self = @This();

    // axiomatic
    undef,
    unit,

    // simple
    int,
    stype,
    ptr,

    // complex
    // TODO I need structs/tuples to get back to homoiconic properties
    list,
    func,
};

/// structured type representation
/// expects to own everything it references. no cycles, no shared subtrees
pub const Type = union(FlatType) {
    const Self = @This();

    undef,
    unit,

    int,
    stype,
    ptr: *Self,

    list: *Self,
    func: struct {
        params: []Self,
        returns: *Self,
    },

    pub fn init_ptr(ally: Allocator, subtype: Self) Allocator.Error!Self {
        return Self{ .ptr = try util.place_on(ally, try subtype.clone(ally)) };
    }

    pub fn init_list(ally: Allocator, subtype: Self) Allocator.Error!Self {
        return Self{ .list = try util.place_on(ally, try subtype.clone(ally)) };
    }

    pub fn init_func(
        ally: Allocator,
        params: []const Self,
        returns: Self
    ) Allocator.Error!Self{
        return Self{
            .func = .{
                .params = try clone_slice(ally, params),
                .returns = try util.place_on(ally, try returns.clone(ally))
            }
        };
    }

    pub fn deinit(self: Self, ally: Allocator) void {
        switch (self) {
            .ptr => |subtype| {
                subtype.deinit(ally);
                ally.destroy(subtype);
            },
            .list => |subtype| {
                subtype.deinit(ally);
                ally.destroy(subtype);
            },
            .func => |func| {
                for (func.params) |param| param.deinit(ally);
                ally.free(func.params);

                func.returns.deinit(ally);
            },
            else => {}
        }
    }

    pub fn clone_slice(
        ally: Allocator,
        slice: []const Self
    ) Allocator.Error![]Self {
        const cloned = try ally.alloc(Self, slice.len);
        for (slice) |child, i| cloned[i] = try child.clone(ally);

        return cloned;
    }

    pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
        return switch (self) {
            .ptr => |subtype| try Self.init_ptr(ally, subtype.*),
            .list => |subtype| try Self.init_list(ally, subtype.*),
            .func => |func|
                try Self.init_func(ally, func.params, func.returns.*),
            else => self
        };
    }

    /// check if this matches a pattern
    pub fn matches(self: Self, opt_pat: ?Pattern) bool {
        const pat = opt_pat orelse return true;

        return self == @as(FlatType, pat) and switch (self) {
            .undef, .unit, .int, .stype => true,
            .ptr => |to| to.matches(Pattern.unwrap(pat.ptr)),
            .list => |of| of.matches(Pattern.unwrap(pat.list)),
            .func => |func| func: {
                // match params
                if (pat.func.params) |param_pats| {
                    if (func.params.len != param_pats.len) {
                        break :func false;
                    }

                    for (func.params) |param, i| {
                        if (!param.matches(param_pats[i])) break :func false;
                    }
                }

                // match returns
                break :func self.func.returns.matches(
                    Pattern.unwrap(pat.func.returns)
                );
            },
        };
    }

    fn format_r(self: Self, writer: anytype) @TypeOf(writer).Error!void {
        switch (self) {
            .stype => try writer.writeAll("Type"),
            .ptr => |subtype| try writer.print("(Ptr {})", .{subtype}),
            .list => |subtype| try writer.print("(List {})", .{subtype}),
            .func => |func| {
                try util.write_join("(Fn [", " ", "]", func.params, writer);
                try writer.print(" {})", .{func.returns});
            },
            else => |tag| {
                const name = @tagName(tag);

                try writer.writeByte(std.ascii.toUpper(name[0]));
                try writer.writeAll(name[1..]);
            }
        }
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        _ = fmt;
        _ = options;

        try self.format_r(writer);
    }
};

/// matchable type patterns
pub const Pattern = union(FlatType) {
    const Self = @This();

    undef,
    unit,

    int,
    stype,
    ptr: ?*Self,

    list: ?*Self,
    func: struct {
        params: ?[]Self,
        returns: ?*Self,
    },

    pub fn init_ptr(ally: Allocator, subtype: ?Self) Allocator.Error!Self {
        return Self{ .ptr = try clone_opt(ally, subtype) };
    }

    pub fn init_list(ally: Allocator, subtype: ?Self) Allocator.Error!Self {
        return Self{ .list = try clone_opt(ally, subtype) };
    }

    pub fn init_func(
        ally: Allocator,
        params: ?[]const Self,
        returns: ?Self
    ) Allocator.Error!Self {
        return Self{
            .func = .{
                .params = try clone_opt_slice(ally, params),
                .returns = try clone_opt(ally, returns),
            }
        };
    }

    pub fn deinit(self: Self, ally: Allocator) void {
        switch (self) {
            .undef, .unit, .int, .stype => {},
            .ptr, .list => |of| if (of) |pat| pat.deinit(ally),
            .func => |func| {
                if (func.params) |params| {
                    for (params) |param| param.deinit(ally);
                }
                if (func.returns) |returns| returns.deinit(ally);
            }
        }
    }

    pub fn from_type(ally: Allocator, stype: Type) Allocator.Error!Self {
        return switch (stype) {
            .undef => Self{ .undef = {} },
            .unit => Self{ .unit = {} },
            .int => Self{ .int = {} },
            .stype => Self{ .stype = {} },
            .ptr => |to| Self{
                .ptr = try util.place_on(ally, try Self.from_type(ally, to.*))
            },
            .list => |of| Self{
                .list = try util.place_on(ally, try Self.from_type(ally, of.*))
            },
            .func => |func| func: {
                const params = try ally.alloc(Self, func.params.len);
                for (func.params) |param, i| {
                    params[i] = try Self.from_type(ally, param);
                }

                const returns = try util.place_on(
                    ally,
                    try Self.from_type(ally, func.returns.*)
                );

                break :func Self{
                    .func = .{ .params = params, .returns = returns }
                };
            },
        };
    }

    pub fn unwrap(opt_ptr: ?*Self) ?Self {
        return if (opt_ptr) |ptr| ptr.* else null;
    }

    pub fn clone_opt(
        ally: Allocator,
        opt: ?Self
    ) Allocator.Error!*?Self {
        if (opt) |pat| {
            return try util.place_on(ally, try pat.clone(ally));
        } else {
            return null;
        }
    }

    pub fn clone_opt_ptr(
        ally: Allocator,
        opt: *?Self
    ) Allocator.Error!?*Self {
        if (opt) |pat| {
            return try util.place_on(ally, pat.clone(ally));
        } else {
            return null;
        }
    }

    pub fn clone_opt_slice(
        ally: Allocator,
        opt: ?[]const Self
    ) Allocator.Error!?[]Self {
        if (opt) |pat| {
            return try util.place_on(ally, try pat.clone_slice(ally));
        } else {
            return null;
        }
    }

    pub fn clone_slice(
        ally: Allocator,
        slice: []const Self
    ) Allocator.Error!Self {
        const cloned = try ally.alloc(Self, slice.len);
        for (slice) |child, i| cloned[i] = try child.clone(ally);

        return cloned;
    }

    pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
        return switch (self) {
            .undef, .unit, .int, .stype => self,
            .ptr => |ptr| Self{ .ptr = try clone_opt_ptr(ally, ptr) },
            .list => |list| Self{ .list = try clone_opt_ptr(ally, list) },
            .func => |func| Self{
                .func = .{
                    .params = try clone_opt_slice(ally, func.params),
                    .returns = try clone_opt_ptr(ally, func.returns)
                }
            },
        };
    }
};

/// structured expression, the canonical representation of Fluent values
pub const Value = union(FlatType) {
    const Self = @This();

    undef,
    unit,

    int: i64,
    stype: Type,

    ptr: struct {
        owns: bool,
        to: *Self,
    },
    list: []Self,
    func: usize, // env block index

    pub fn deinit(self: Self, ally: Allocator) void {
        switch (self) {
            .ptr => |ptr| {
                if (ptr.owns) {
                    ptr.to.deinit(ally);
                    ally.destroy(ptr.to);
                }
            },
            .list => |children| {
                for (children) |child| child.deinit(ally);
                ally.free(children);
            },
            else => {}
        }
    }

    pub fn clone_slice(
        ally: Allocator,
        slice: []const Self
    ) Allocator.Error![]Self {
        const cloned = try ally.alloc(Self, slice.len);
        for (slice) |child, i| cloned[i] = try child.clone(ally);

        return cloned;
    }

    pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
        return switch(self) {
            .undef, .unit, .int, .func => self,
            .stype => |stype| Self{ .stype = try stype.clone(ally) },
            // TODO is this the right behavior?
            .ptr => |ptr| Self{ .ptr = .{ .owns = false, .to = ptr.to } },
            .list => |list| Self{ .list = try clone_slice(ally, list) },
        };
    }

    fn format_r(self: Self, writer: anytype) @TypeOf(writer).Error!void {
        switch (self) {
            .unit, .undef => try writer.print("{s}", .{@tagName(self)}),
            .int => |n| try writer.print("{d}", .{n}),
            .stype => |stype| try writer.print("{}", .{stype}),
            .ptr => |subtype| try writer.print("(ref {})", .{subtype}),
            .list => |list| try util.write_join("[", " ", "]", list, writer),
            .func => |index| try writer.print("(fn b{})", .{index}),
        }
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        _ = fmt;
        _ = options;

        try self.format_r(writer);
    }
};