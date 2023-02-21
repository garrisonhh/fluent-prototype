const std = @import("std");
const expect = std.testing.expect;
const Allocator = std.mem.Allocator;
const ZType = std.builtin.Type;
const builtin = @import("builtin");
const com = @import("common");
const kz = @import("kritzler");
const Env = @import("../env.zig");
const canon = @import("../canon.zig");
const Repr = canon.Repr;
const ReprId = canon.ReprId;
const TypeId = canon.TypeId;
const Object = canon.Object;
const Value = canon.Value;

const IField = union(enum) {
    const Self = @This();

    value: type,
    /// holds subtype
    slice: type,
    /// holds interface type
    coll: type,

    fn Into(comptime self: Self) type {
        return switch (self) {
            .value => |T| T,
            .slice => |T| SliceWrapper(T),
            .coll => |I| Wrapper(I),
        };
    }
};

const CollType = enum {
    @"struct",
    variant,
};

/// an interface for structured data
fn CollInterface(
    comptime in_itype: CollType,
    comptime in_Tag: type,
    comptime in_fields: []const IField,
) type {
    return struct {
        pub const itype = in_itype;
        pub const Tag = in_Tag;
        pub const fields = in_fields;

        /// index fields by tag
        pub fn field(comptime tag: Tag) IField {
            return fields[@enumToInt(tag)];
        }

        pub fn Value(comptime tag: Tag) type {
            return field(tag).value;
        }

        /// this interface expressed as a zig type
        pub const Into = switch (itype) {
            .@"struct" => st: {
                var st_fields: [fields.len]ZType.StructField = undefined;
                for (std.enums.values(Tag)) |tag, i| {
                    const field_type = field(tag).Into();

                    st_fields[i] = ZType.StructField{
                        .name = @tagName(tag),
                        .field_type = field_type,
                        .default_value = null,
                        .is_comptime = false,
                        .alignment = @alignOf(field_type),
                    };
                }

                break :st @Type(ZType{
                    .Struct = ZType.Struct{
                        .layout = .Auto,
                        .fields = &st_fields,
                        .decls = &.{},
                        .is_tuple = false,
                    },
                });
            },
            .variant => v: {
                var u_fields: [fields.len]ZType.UnionField = undefined;
                for (std.enums.values(Tag)) |tag, i| {
                    const field_type = field(tag).Into();

                    u_fields[i] = ZType.UnionField{
                        .name = @tagName(tag),
                        .field_type = field_type,
                        .alignment = @alignOf(field_type),
                    };
                }

                break :v @Type(ZType{
                    .Union = ZType.Union{
                        .layout = .Auto,
                        .tag_type = Tag,
                        .fields = &u_fields,
                        .decls = &.{},
                    },
                });
            },
        };

        fn dumpR(writer: anytype, level: usize) @TypeOf(writer).Error!void {
            const INDENT = 2;

            try writer.print("{s} interface {{\n", .{@tagName(itype)});
            inline for (fields) |ifield, i| {
                try writer.writeByteNTimes(' ', (level + 1) * INDENT);

                const name = @tagName(@intToEnum(Tag, i));
                try writer.print("{s}: ", .{name});

                switch (ifield) {
                    .value => |T| try writer.print("{}\n", .{T}),
                    .coll => |T| try T.dumpR(writer, level + 1),
                    .slice => @compileError("TODO dump slice wrapper"),
                }
            }
            try writer.writeByteNTimes(' ', level * INDENT);
            try writer.writeAll("}\n");
        }

        pub fn dump(writer: anytype) @TypeOf(writer).Error!void {
            try dumpR(writer, 0);
        }
    };
}

/// create an interface tag enum
fn genTag(comptime Template: type) type {
    const field_names = std.meta.fieldNames(Template);
    if (field_names.len == 0) {
        @compileError(std.fmt.comptimePrint(
            "type {} does not have any fields",
            .{Template},
        ));
    }

    var fields: [field_names.len]ZType.EnumField = undefined;
    for (field_names) |name, i| {
        fields[i] = ZType.EnumField{
            .name = name,
            .value = i,
        };
    }

    return @Type(ZType{
        .Enum = ZType.Enum{
            .layout = .Auto,
            .tag_type = usize,
            .fields = &fields,
            .decls = &.{},
            .is_exhaustive = true,
        },
    });
}

fn genField(comptime field_type: type) IField {
    return switch (field_type) {
        void,
        bool,
        u8,
        u16,
        u32,
        u64,
        i8,
        i16,
        i32,
        i64,
        f32,
        f64,
        TypeId,
        ReprId,
        => IField{ .value = field_type },

        else => switch (@typeInfo(field_type)) {
            .Pointer => |meta| switch (meta.size) {
                .One, .Many, .C => IField{ .value = field_type },
                .Slice => IField{ .slice = meta.child },
            },
            .Struct => IField{ .coll = field_type },
            .Union => |meta| u: {
                if (meta.tag_type == null) {
                    @compileError("variant templates must have a tag");
                }

                break :u IField{ .coll = field_type };
            },
            else => @compileError(std.fmt.comptimePrint(
                "type {} is not allowed in an object interface",
                .{field_type},
            )),
        },
    };
}

fn genInterface(comptime Template: type) type {
    comptime {
        const fields = std.meta.fields(Template);

        var ifields: [fields.len]IField = undefined;
        for (fields) |field, i| {
            ifields[i] = genField(field.field_type);
        }

        const itype: CollType = switch (@typeInfo(Template)) {
            .Struct => .@"struct",
            .Union => .variant,
            else => unreachable,
        };

        return CollInterface(itype, genTag(Template), &ifields);
    }
}

fn SliceWrapper(comptime T: type) type {
    return struct {
        const Self = @This();

        const Struct = Wrapper(struct {
            ptr: [*]T,
            len: u64,
        });

        const Elem = genField(T).Into();

        slice: Struct,

        fn of(base: *anyopaque, env: *Env) Self {
            return Self{ .slice = Struct.of(base, env) };
        }

        pub fn alloc(self: Self, length: usize) Allocator.Error!void {
            const data = try self.slice.env.ally.alloc(T, length);
            self.slice.set(.ptr, data.ptr);
            self.slice.set(.len, data.len);
        }

        pub fn free(self: Self) void {
            const slice = self.slice.get(.ptr)[0..self.len()];
            self.slice.env.ally.free(slice);
        }

        pub fn len(self: Self) usize {
            return self.slice.get(.len);
        }

        /// index into slice
        pub fn get(self: Self, index: usize) Elem {
            return self.slice.get(.ptr)[index];
        }

        /// get index of slice
        pub fn set(self: Self, index: usize, value: T) void {
            self.slice.get(.ptr)[index] = value;
        }
    };
}

pub fn Wrapper(comptime Template: type) type {
    return struct {
        const Self = @This();

        pub const I = genInterface(Template);

        base: *anyopaque,
        env: *Env,
        ty: TypeId,
        fields: []const Repr.Field,

        /// wrap raw data
        pub fn of(base: *anyopaque, env: *Env) Self {
            // CollInterface will verify these
            const ty = env.identifyZigType(Template) catch unreachable;
            const repr = env.reprOf(ty) catch unreachable;

            return Self{
                .base = base,
                .env = env,
                .ty = ty,
                .fields = env.rw.get(repr).coll,
            };
        }

        /// create an object and wrap it
        pub fn init(env: *Env) Object.InitError!Self {
            const ty = try env.identifyZigType(Template);
            return Self.wrap(env, try Object.init(env, ty));
        }

        /// free the wrapped object
        pub fn deinit(self: Self) void {
            self.unwrap().deinit(self.env);
        }

        /// wrap an instantiated object
        pub fn wrap(env: *Env, obj: Object) Self {
            if (builtin.mode == .Debug) {
                // ensure template type maps to the object type
                const ty = env.identifyZigType(Template) catch |e| {
                    std.debug.panic("{} while checking wrapped type", .{e});
                };

                std.debug.assert(ty.eql(obj.ty));
            }

            return Self{
                .base = obj.val.buf.ptr,
                .env = env,
                .ty = obj.ty,
                .fields = env.rw.get(obj.repr).coll,
            };
        }

        /// unwrap an instantiated object
        pub fn unwrap(self: Self) Object {
            // since finding the size and creating the object were already done
            // when instantiating the wrapper, they should never fail
            const ptr = @ptrCast([*]u8, self.base);
            const aligned_ptr = @alignCast(Value.Alignment, ptr);

            const sz = self.env.sizeOf(self.ty) catch unreachable;
            const val = Value.of(aligned_ptr[0..sz]);

            return Object.from(self.env, self.ty, val) catch unreachable;
        }

        pub fn clone(self: Self) Allocator.Error!Self {
            const obj = try self.unwrap().clone(self.env.ally);
            return Self.wrap(self.env, obj);
        }

        fn getFieldPtrIndexed(self: Self, tag_index: usize) *anyopaque {
            const offset = self.fields[tag_index].offset;
            return @intToPtr(*anyopaque, @ptrToInt(self.base) + offset);
        }

        fn getFieldPtr(self: Self, comptime tag: I.Tag) *anyopaque {
            const tag_index = @enumToInt(tag) + switch (I.itype) {
                .@"struct" => 0,
                // variant must skip 'tag' field
                .variant => 1,
            };

            return self.getFieldPtrIndexed(tag_index);
        }

        fn getTagPtr(self: Self) *I.Tag {
            const raw_ptr = self.getFieldPtrIndexed(0);
            const aligned_ptr = @alignCast(@alignOf(I.Tag), raw_ptr);
            return @ptrCast(*I.Tag, aligned_ptr);
        }

        /// get a pointer to a field value
        pub fn getPtr(self: Self, comptime tag: I.Tag) *I.Value(tag) {
            const V = I.Value(tag);

            const raw_ptr = self.getFieldPtr(tag);
            const aligned = @alignCast(@alignOf(V), raw_ptr);
            return @ptrCast(*V, aligned);
        }

        /// get a field value or interface
        pub fn get(self: Self, comptime tag: I.Tag) I.field(tag).Into() {
            if (I.itype == .variant) {
                @compileError("use into() to observe a variant");
            }

            return switch (I.field(tag)) {
                .value => self.getPtr(tag).*,
                .slice => |T| SliceWrapper(T).of(
                    self.getFieldPtr(tag),
                    self.env,
                ),
                .coll => |FieldI| wrapped: {
                    const tag_index = @enumToInt(tag);
                    const field_repr = self.fields[tag_index].of;
                    const parent_ty = self.env.tw.get(self.ty);

                    const field_ty = switch (I.itype) {
                        .@"struct" => parent_ty.@"struct"[tag_index].of,
                        .variant => parent_ty.variant[tag_index].of,
                    };

                    const field_ptr = self.getFieldPtr(tag);
                    const field_fields = self.env.rw.get(field_repr).coll;

                    break :wrapped Wrapper(FieldI){
                        .base = field_ptr,
                        .env = self.env,
                        .ty = field_ty,
                        .fields = field_fields,
                    };
                },
            };
        }

        /// get all of the fields of a
        pub fn into(self: Self) I.Into {
            switch (I.itype) {
                .@"struct" => {
                    var st: I.Into = undefined;
                    inline for (self.enums.values(I.Tag)) |tag| {
                        @field(st, @tagName(tag)) = self.get(tag);
                    }

                    return st;
                },
                .variant => {
                    const rt_tag = self.getTagPtr().*;
                    switch (rt_tag) {
                        inline else => |tag| {
                            return @unionInit(
                                I.Into,
                                @tagName(tag),
                                self.getPtr(tag).*,
                            );
                        },
                    }
                },
            }
        }

        pub fn set(
            self: Self,
            comptime tag: I.Tag,
            val: I.Value(tag),
        ) void {
            // tag must be set for variants
            if (I.itype == .variant) {
                self.getTagPtr().* = tag;
            }

            self.getPtr(tag).* = val;
        }

        pub fn render(
            self: Self,
            ctx: *kz.Context,
            _: void,
        ) Object.InitError!kz.Ref {
            return self.unwrap().render(ctx, self.env);
        }
    };
}

test "interface-struct" {
    const prelude = @import("prelude.zig");

    const writer = std.io.getStdErr().writer();
    try writer.writeByte('\n');

    var env = try Env.init(std.testing.allocator);
    defer env.deinit();

    try prelude.initPrelude(&env);

    const Vec3 = Wrapper(struct {
        x: u32,
        y: u32,
        z: u32,
    });

    const wrapped = try Vec3.init(&env);
    defer wrapped.deinit();

    wrapped.set(.x, 11);
    wrapped.set(.y, 22);
    wrapped.set(.z, 33);

    try expect(11 == wrapped.get(.x));
    try expect(22 == wrapped.get(.y));
    try expect(33 == wrapped.get(.z));

    try kz.display(env.ally, {}, wrapped, writer);
}

test "interface-nested-struct" {
    const prelude = @import("prelude.zig");

    const writer = std.io.getStdErr().writer();
    try writer.writeByte('\n');

    var env = try Env.init(std.testing.allocator);
    defer env.deinit();

    try prelude.initPrelude(&env);

    const Vec3 = struct { x: u32, y: u32, z: u32 };
    const ZRay = struct { pos: Vec3, dir: Vec3 };
    const Ray = Wrapper(ZRay);

    const wrapped = try Ray.init(&env);
    defer wrapped.deinit();

    wrapped.get(.pos).set(.x, 11);
    wrapped.get(.pos).set(.y, 22);
    wrapped.get(.pos).set(.z, 33);
    wrapped.get(.dir).set(.x, 44);
    wrapped.get(.dir).set(.y, 55);
    wrapped.get(.dir).set(.z, 66);

    try expect(11 == wrapped.get(.pos).get(.x));
    try expect(22 == wrapped.get(.pos).get(.y));
    try expect(33 == wrapped.get(.pos).get(.z));
    try expect(44 == wrapped.get(.dir).get(.x));
    try expect(55 == wrapped.get(.dir).get(.y));
    try expect(66 == wrapped.get(.dir).get(.z));

    try kz.display(env.ally, {}, wrapped, writer);
}

test "interface-variant" {
    const prelude = @import("prelude.zig");

    const writer = std.io.getStdErr().writer();
    try writer.writeByte('\n');

    var env = try Env.init(std.testing.allocator);
    defer env.deinit();

    try prelude.initPrelude(&env);

    const V = Wrapper(union(enum) {
        a: u32,
        b: i32,
        c: f32,
    });

    const wrapped = try V.init(&env);
    defer wrapped.deinit();

    // a
    wrapped.set(.a, 11);

    try kz.display(env.ally, {}, wrapped, writer);

    const val_a = wrapped.into();
    try expect(val_a == .a and val_a.a == 11);

    // b
    wrapped.set(.b, -22);

    const val_b = wrapped.into();
    try expect(val_b == .b and val_b.b == -22);

    try kz.display(env.ally, {}, wrapped, writer);

    // c
    wrapped.set(.c, 1.5);

    const val_c = wrapped.into();
    try expect(val_c == .c and val_c.c == 1.5);

    try kz.display(env.ally, {}, wrapped, writer);
}

test "interface-slice" {
    const prelude = @import("prelude.zig");

    const writer = std.io.getStdErr().writer();
    try writer.writeByte('\n');

    var env = try Env.init(std.testing.allocator);
    defer env.deinit();

    try prelude.initPrelude(&env);

    const S = Wrapper(struct {
        slice: []u32,
    });

    const wrapped = try S.init(&env);
    defer wrapped.deinit();

    const slice = wrapped.get(.slice);
    try slice.alloc(3);
    defer slice.free();

    try expect(slice.len() == 3);

    slice.set(0, 11);
    slice.set(1, 22);
    slice.set(2, 33);

    try expect(11 == slice.get(0));
    try expect(22 == slice.get(1));
    try expect(33 == slice.get(2));

    try kz.display(env.ally, {}, wrapped, writer);
}
