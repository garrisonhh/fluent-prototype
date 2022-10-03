//! contains the essential representations of Fluent data
//! most meaty function here is `SExpr.from_expr` which translates the frontend
//! AST into an SExpr tree.

const std = @import("std");
const util = @import("../util/util.zig");
const frontend = @import("../frontend.zig");
const Env = @import("env.zig");

const Allocator = std.mem.Allocator;
const Expr = frontend.Expr;

/// flat type representation
pub const FlatType = enum {
    const Self = @This();

    // axiomatic
    undef,
    unit,

    // simple
    boolean,
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

    boolean,
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
                ally.destroy(func.returns);
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
            .undef, .unit, .boolean, .int, .stype => true,
            .ptr => |to| to.matches(Pattern.unwrap(pat.ptr)),
            .list => |of| of.matches(Pattern.unwrap(pat.list)),
            .func => |func| func: {
                // match params
                if (func.params.len != pat.func.params.len) {
                    break :func false;
                }

                for (func.params) |param, i| {
                    if (!param.matches(pat.func.params[i])) break :func false;
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
            .boolean => try writer.writeAll("Bool"),
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

    boolean,
    int,
    stype,
    ptr: ?*Self,

    list: ?*Self,
    func: struct {
        params: []Self,
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
        params: []const Self,
        returns: ?Self
    ) Allocator.Error!Self {
        return Self{
            .func = .{
                .params = try clone_slice(ally, params),
                .returns = try clone_opt(ally, returns),
            }
        };
    }

    pub fn deinit(self: Self, ally: Allocator) void {
        switch (self) {
            .undef, .unit, .boolean, .int, .stype => {},
            .ptr, .list => |of| if (of) |pat| pat.deinit(ally),
            .func => |func| {
                for (func.params) |param| param.deinit(ally);
                if (func.returns) |returns| returns.deinit(ally);
            }
        }
    }

    pub fn from_type(ally: Allocator, stype: Type) Allocator.Error!Self {
        return switch (stype) {
            .undef => Self{ .undef = {} },
            .unit => Self{ .unit = {} },
            .boolean => Self{ .boolean = {} },
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

    /// if pattern is incomplete, returns null
    pub fn to_type(ally: Allocator, opt_pat: ?Pattern) Allocator.Error!?Type {
        const pat = opt_pat orelse return null;

        return switch (pat) {
            .undef => Type{ .undef = {} },
            .unit => Type{ .unit = {} },
            .boolean => Type{ .boolean = {} },
            .int => Type{ .int = {} },
            .stype => Type{ .stype = {} },
            .ptr => |to| ptr: {
                const sub = (try Pattern.to_type(ally, Pattern.unwrap(to)))
                            orelse return null;
                break :ptr Type{ .ptr = try util.place_on(ally, sub) };
            },
            .list => |of| list: {
                const sub = (try Pattern.to_type(ally, Pattern.unwrap(of)))
                            orelse return null;
                break :list Type{ .list = try util.place_on(ally, sub) };
            },
            .func => |func| func: {
                const returns = (try Pattern.to_type(
                    ally,
                    Pattern.unwrap(func.returns)
                )) orelse return null;

                const params = try ally.alloc(Type, func.params.len);
                for (func.params) |param_pat, i| {
                    params[i] = (try Pattern.to_type(ally, param_pat)) orelse {
                        return null;
                    };
                }

                break :func Type{
                    .func = .{
                        .params = params,
                        .returns = try util.place_on(ally, returns)
                    }
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
    ) Allocator.Error!?*Self {
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
                    .params = try clone_slice(ally, func.params),
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

    boolean: bool,
    int: i64,
    stype: Type,

    // TODO make ptr, my_ptr, arr_ptr, my_arr_ptr distinct? or is this just
    // a type-level distinction?
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
            .undef, .unit, .boolean, .int, .func => self,
            .stype => |stype| Self{ .stype = try stype.clone(ally) },
            // TODO is this the right behavior?
            .ptr => |ptr| Self{ .ptr = .{ .owns = false, .to = ptr.to } },
            .list => |list| Self{ .list = try clone_slice(ally, list) },
        };
    }

    fn format_r(self: Self, writer: anytype) @TypeOf(writer).Error!void {
        switch (self) {
            .unit, .undef => try writer.print("{s}", .{@tagName(self)}),
            .boolean => |b| try writer.print("{}", .{b}),
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