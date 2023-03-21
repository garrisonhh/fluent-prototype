const std = @import("std");
const expectEqual = std.testing.expectEqual;
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
const Image = canon.Image;
const Ptr = canon.Ptr;

const INDENT = 2;

const IField = union(enum) {
    const Self = @This();

    unit,
    value: type,
    /// holds subtype
    slice: type,
    /// holds interface type
    coll: type,

    fn Into(comptime self: Self) type {
        return switch (self) {
            .unit => void,
            .value => |T| T,
            .slice => |T| SliceWrapper(T),
            .coll => |I| Wrapper(I, null),
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
            try writer.print("{s} interface {{\n", .{@tagName(itype)});
            inline for (fields) |ifield, i| {
                try writer.writeByteNTimes(' ', (level + 1) * INDENT);

                const name = @tagName(@intToEnum(Tag, i));
                try writer.print("{s}: ", .{name});

                switch (ifield) {
                    .unit => try writer.writeAll("unit\n"),
                    .value => |T| try writer.print("{}\n", .{T}),
                    .coll => |T| try genInterface(T).dumpR(writer, level + 1),
                    .slice => |T| try SliceWrapper(T).dumpR(writer, level + 1),
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
        void => .unit,

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
                .One, .Many, .C => IField{ .value = Ptr },
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
            ptr: *T,
            len: u64,
        }, null);

        const ElemField = genField(T);
        const Elem = ElemField.Into();

        st: Struct,

        fn of(base: Ptr, env: *Env) Self {
            return Self{ .st = Struct.of(base, env) };
        }

        pub fn sizeOf(self: Self) usize {
            return self.st.sizeOf();
        }

        /// set the slice struct
        pub fn setInto(self: Self, sl_ptr: Ptr, sl_len: usize) void {
            self.st.set(.ptr, sl_ptr);
            self.st.set(.len, sl_len);
        }

        /// as a slice of values
        pub fn into(self: Self) []ElemField.value {
            const img = &self.st.env.img;

            const addr = self.st.get(.ptr);
            return img.intoSlice(addr, ElemField.value, self.len());
        }

        pub fn ptr(self: Self) Ptr {
            return self.st.get(.ptr);
        }

        pub fn len(self: Self) usize {
            return self.st.get(.len);
        }

        pub fn getElemPtr(self: Self, index: usize) Ptr {
            std.debug.assert(index < self.len());

            const p = self.st.get(.ptr);
            const elem_sz = switch (ElemField) {
                .unit => 0,
                .value => |V| @sizeOf(V),
                .slice => |E| SliceWrapper(E).of(p, self.st.env).sizeOf(),
                .coll => |I| Wrapper(I, null).of(p, self.st.env).sizeOf(),
            };

            return p.add(elem_sz * index);
        }

        /// index into slice
        pub fn get(self: Self, index: usize) Elem {
            const img = &self.st.env.img;
            const p = self.getElemPtr(index);

            return switch (ElemField) {
                .unit => {},
                .value => |V| img.read(p, V),
                .slice => |E| SliceWrapper(E).of(p, self.st.env),
                .coll => |I| Wrapper(I, null).of(p, self.st.env),
            };
        }

        /// set a value, use into() and get() to modify collections and slics
        pub fn set(self: Self, index: usize, value: ElemField.value) void {
            self.into()[index] = value;
        }

        fn dumpR(writer: anytype, level: usize) !void {
            try writer.writeAll("(slice) ");
            try Struct.I.dumpR(writer, level);
        }
    };
}

/// provide a zig template type and an optional mixin.
///
/// mixin can provide:
/// ```zig
/// /// called after Object.init
/// pub fn init(self: Self) Object.InitError!void
/// /// called prior to Object.deinit
/// pub fn deinit(self: Self) void
/// /// called after performing a shallow clone
/// pub fn clone(self: Self) Allocator.Error!void
/// ```
pub fn Wrapper(
    comptime Template: type,
    comptime Mixin: ?fn (type) type,
) type {
    return struct {
        const Self = @This();
        const Ext = if (Mixin) |M| M(Self) else struct {};

        pub const I = genInterface(Template);

        base: Ptr,
        ty: TypeId,

        env: *Env,
        fields: []const Repr.Field,

        /// wrap raw data
        pub fn of(base: Ptr, env: *Env) Self {
            // CollInterface will verify these
            const ty = env.identifyZigType(Template) catch unreachable;
            const repr = env.reprOf(ty) catch unreachable;

            return Self{
                .base = base,
                .ty = ty,
                .env = env,
                .fields = env.rw.get(repr).coll,
            };
        }

        /// create an object and wrap it
        pub fn init(env: *Env) Object.InitError!Self {
            const ty = try env.identifyZigType(Template);
            const self = Self.wrap(env, try Object.init(env, ty));

            if (@hasDecl(Ext, "init")) try Ext.init(self);

            return self;
        }

        /// free the wrapped object
        pub fn deinit(self: Self) void {
            if (@hasDecl(Ext, "deinit")) Ext.deinit(self);

            self.unwrap().deinit(self.env);
        }

        /// wrap an instantiated object
        pub fn wrap(env: *Env, obj: Object) Self {
            if (builtin.mode == .Debug) {
                // TODO this relies on hashing Types with 'self-awareness'

                // // ensure template type maps to the object type
                // const ty = env.identifyZigType(Template) catch |e| {
                // std.debug.panic("{} while checking wrapped type", .{e});
                // };

                // if (!ty.eql(obj.ty)) {
                // const stderr = std.io.getStdErr().writer();

                // stderr.writeAll("expected to wrap type:\n") catch {};
                // const expected = env.tw.get(ty);
                // kz.display(env.ally, env.tw, expected, stderr) catch {};
                // stderr.writeAll("instead received type:\n") catch {};
                // const received = env.tw.get(obj.ty);
                // kz.display(env.ally, env.tw, received, stderr) catch {};
                // }

                // std.debug.assert(ty.eql(obj.ty));
            }

            return Self{
                .base = obj.ptr,
                .env = env,
                .ty = obj.ty,
                .fields = env.rw.get(obj.repr).coll,
            };
        }

        /// unwrap an instantiated object
        pub fn unwrap(self: Self) Object {
            return Object.from(self.env, self.ty, self.base) catch unreachable;
        }

        pub fn clone(self: Self) Object.InitError!Self {
            const obj = try self.unwrap().clone(self.env);
            const cloned = Self.wrap(self.env, obj);

            if (@hasDecl(Ext, "clone")) try Ext.clone(cloned);

            return cloned;
        }

        pub fn sizeOf(self: Self) usize {
            // this would have errored out in init if it were to error
            return self.env.sizeOf(self.ty) catch unreachable;
        }

        fn getFieldPtrIndexed(self: Self, tag_index: usize) Ptr {
            const offset = self.fields[tag_index].offset;
            return self.base.add(offset);
        }

        fn getFieldPtr(self: Self, comptime tag: I.Tag) Ptr {
            const tag_index = @enumToInt(tag) + switch (I.itype) {
                .@"struct" => 0,
                // variant must skip 'tag' field
                .variant => 1,
            };

            return self.getFieldPtrIndexed(tag_index);
        }

        fn getTagPtr(self: Self) *I.Tag {
            std.debug.assert(I.itype == .variant);

            const ptr = self.getFieldPtrIndexed(0);
            return self.env.img.into(ptr, *I.Tag);
        }

        /// get a pointer to a field value
        pub fn getPtr(self: Self, comptime tag: I.Tag) *I.Value(tag) {
            const V = I.Value(tag);
            const aln = @alignOf(V);

            if (builtin.mode == .Debug and aln == 0) {
                @compileError(comptime std.fmt.comptimePrint(
                    "attempted to get a pointer to {}",
                    .{V},
                ));
            }

            const raw_ptr = self.getFieldPtr(tag);
            return self.env.img.into(raw_ptr, *V);
        }

        /// get a field value or interface
        pub fn get(self: Self, comptime tag: I.Tag) I.field(tag).Into() {
            if (I.itype == .variant) {
                std.debug.assert(tag == self.getTagPtr().*);
            }

            return switch (I.field(tag)) {
                .unit => {},
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

                    break :wrapped Wrapper(FieldI, null){
                        .base = field_ptr,
                        .env = self.env,
                        .ty = field_ty,
                        .fields = field_fields,
                    };
                },
            };
        }

        /// get all of the fields of a wrapped value
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
                    switch (self.getVarTag()) {
                        inline else => |tag| {
                            const name = @tagName(tag);
                            return @unionInit(I.Into, name, self.get(tag));
                        },
                    }
                },
            }
        }

        /// for variants, set the tag
        pub fn setVarTag(self: Self, tag: I.Tag) void {
            self.getTagPtr().* = tag;
        }

        pub fn getVarTag(self: Self) I.Tag {
            return self.getTagPtr().*;
        }

        /// sets the var tag and then returns the value. this is super useful
        /// for initializing variant types.
        pub fn setInto(self: Self, comptime tag: I.Tag) I.field(tag).Into() {
            self.setVarTag(tag);
            return self.get(tag);
        }

        fn SetType(comptime tag: I.Tag) type {
            return switch (I.field(tag)) {
                .unit => void,
                .value => |T| T,
                else => @compileError(std.fmt.comptimePrint(
                    "cannot set field `{s}`",
                    @tagName(tag),
                )),
            };
        }

        /// set a value based on the templated value. if this is a variant, also
        /// set the variant's tag.
        ///
        /// this copies nested data for nested fields.
        pub fn set(
            self: Self,
            comptime tag: I.Tag,
            val: SetType(tag),
        ) void {
            // tag must be set for variants
            if (I.itype == .variant) {
                self.setVarTag(tag);
            }

            switch (I.field(tag)) {
                .unit => {},
                .value => self.getPtr(tag).* = val,
                else => unreachable,
            }
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
    }, null);

    const wrapped = try Vec3.init(&env);
    defer wrapped.deinit();

    wrapped.set(.x, 11);
    wrapped.set(.y, 22);
    wrapped.set(.z, 33);

    try expectEqual(@as(u32, 11), wrapped.get(.x));
    try expectEqual(@as(u32, 22), wrapped.get(.y));
    try expectEqual(@as(u32, 33), wrapped.get(.z));

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
    const Ray = Wrapper(ZRay, null);

    const wrapped = try Ray.init(&env);
    defer wrapped.deinit();

    wrapped.get(.pos).set(.x, 11);
    wrapped.get(.pos).set(.y, 22);
    wrapped.get(.pos).set(.z, 33);
    wrapped.get(.dir).set(.x, 44);
    wrapped.get(.dir).set(.y, 55);
    wrapped.get(.dir).set(.z, 66);

    try expectEqual(@as(u32, 11), wrapped.get(.pos).get(.x));
    try expectEqual(@as(u32, 22), wrapped.get(.pos).get(.y));
    try expectEqual(@as(u32, 33), wrapped.get(.pos).get(.z));
    try expectEqual(@as(u32, 44), wrapped.get(.dir).get(.x));
    try expectEqual(@as(u32, 55), wrapped.get(.dir).get(.y));
    try expectEqual(@as(u32, 66), wrapped.get(.dir).get(.z));

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
    }, null);
    const VTag = V.I.Tag;

    const wrapped = try V.init(&env);
    defer wrapped.deinit();

    // a
    wrapped.set(.a, 11);

    try kz.display(env.ally, {}, wrapped, writer);

    const val_a = wrapped.into();
    try expectEqual(VTag.a, val_a);
    try expectEqual(@as(u32, 11), val_a.a);

    // b
    wrapped.set(.b, -22);

    const val_b = wrapped.into();
    try expectEqual(VTag.b, val_b);
    try expectEqual(@as(i32, -22), val_b.b);

    try kz.display(env.ally, {}, wrapped, writer);

    // c
    wrapped.set(.c, 1.5);

    const val_c = wrapped.into();
    try expectEqual(VTag.c, val_c);
    try expectEqual(@as(f32, 1.5), val_c.c);

    try kz.display(env.ally, {}, wrapped, writer);
}

test "interface-value-slice" {
    const prelude = @import("prelude.zig");

    const writer = std.io.getStdErr().writer();
    try writer.writeByte('\n');

    const ally = std.testing.allocator;
    var env = try Env.init(ally);
    defer env.deinit();

    try prelude.initPrelude(&env);

    const S = Wrapper(struct { slice: []u32 }, null);

    const wrapped = try S.init(&env);
    defer wrapped.deinit();

    const len = 3;
    const mem_sz = len * @sizeOf(u32);
    const mem = try env.alloc(.heap, mem_sz);
    defer env.free(mem, mem_sz);

    const slice = wrapped.get(.slice);
    slice.setInto(mem, len);

    try expectEqual(@as(usize, 3), slice.len());

    slice.set(0, 11);
    slice.set(1, 22);
    slice.set(2, 33);

    try expectEqual(@as(u32, 11), slice.get(0));
    try expectEqual(@as(u32, 22), slice.get(1));
    try expectEqual(@as(u32, 33), slice.get(2));

    try kz.display(env.ally, {}, wrapped, writer);
}

test "interface-coll-slice" {
    const prelude = @import("prelude.zig");

    const writer = std.io.getStdErr().writer();
    try writer.writeByte('\n');

    const ally = std.testing.allocator;
    var env = try Env.init(ally);
    defer env.deinit();

    try prelude.initPrelude(&env);

    const Test = struct {
        a: u32,
        b: bool,
    };
    const test_sz = try env.sizeOf(try env.identifyZigType(Test));

    const S = Wrapper(struct { slice: []Test }, null);

    const wrapped = try S.init(&env);
    defer wrapped.deinit();

    const slice = wrapped.get(.slice);
    const mem_sz = test_sz * 3;
    const mem = try env.alloc(.heap, mem_sz);
    defer env.free(mem, mem_sz);

    slice.setInto(mem, 3);

    try expectEqual(@as(usize, 3), slice.len());

    slice.get(0).set(.a, 11);
    slice.get(0).set(.b, true);
    slice.get(1).set(.a, 22);
    slice.get(1).set(.b, false);
    slice.get(2).set(.a, 33);
    slice.get(2).set(.b, true);

    const fst = slice.get(0);
    try expectEqual(@as(u32, 11), fst.get(.a));
    try expectEqual(true, fst.get(.b));

    const snd = slice.get(1);
    try expectEqual(@as(u32, 22), snd.get(.a));
    try expectEqual(false, snd.get(.b));

    const thd = slice.get(2);
    try expectEqual(@as(u32, 33), thd.get(.a));
    try expectEqual(true, thd.get(.b));

    try kz.display(env.ally, {}, wrapped, writer);
}
