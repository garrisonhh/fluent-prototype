const std = @import("std");
const Allocator = std.mem.Allocator;
const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();
const builtin = @import("builtin");
const kz = @import("kritzler");

const StdOutError = @TypeOf(stdout).Error;
const StdErrError = @TypeOf(stderr).Error;

const INDENT = 2;

/// types of values
pub const Type = union(enum) {
    const Tag = std.meta.Tag(@This());

    flag,
    nat,
    string,
};

/// the output of an option
pub const Value = union(Type.Tag) {
    const Self = @This();

    flag: void,
    nat: usize,
    string: []u8,

    fn deinit(self: Self, ally: Allocator) void {
        switch (self) {
            .string => |str| ally.free(str),
            else => {}
        }
    }

    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype
    ) @TypeOf(writer).Error!void {
        switch (self) {
            .flag => try writer.writeAll("flag"),
            .nat => |n| try writer.print("{d}", .{n}),
            .string => |str| try writer.print("{s}", .{str}),
        }
    }
};

/// used to encode cli arg options
/// interact with this through OptionId and Parser functions
const Option = struct {
    const Self = @This();

    name: []u8,
    desc: []u8,
    ty: Type,

    short: ?u8 = null,
    long: ?[]u8 = null,

    fn deinit(self: Self, ally: Allocator) void {
        ally.free(self.name);
        ally.free(self.desc);
        if (self.long) |long| ally.free(long);
    }
};

pub const Output = union(enum) {
    const Self = @This();

    pub const Options = std.StringHashMapUnmanaged(Value);

    /// maps option name -> value
    options: Options,
    path: struct {
        subcommand: []const u8,
        out: *Output,
    },

    pub fn deinit(self: *Self, ally: Allocator) void {
        switch (self.*) {
            .path => |path| {
                path.out.deinit(ally);
                ally.destroy(path.out);
            },
            .options => |*opts| {
                var iter = opts.valueIterator();
                while (iter.next()) |val| val.deinit(ally);
                opts.deinit(ally);
            }
        }
    }

    /// use to test for and access a specific path
    pub fn match(
        self: *const Self,
        path: []const []const u8,
    ) ?*const Options {
        if (path.len == 0) {
            return if (self.* == .options) &self.options else null;
        } else if (self.* != .path) {
            return null;
        }

        if (!std.mem.eql(u8, self.path.subcommand, path[0])) {
            return null;
        }

        return self.path.out.match(path[1..]);
    }

    /// same as match but in bool form
    pub fn matches(self: *const Self, path: []const []const u8) bool {
        return self.match(path) != null;
    }

    /// test if your output looks like a subcommand followed by `help`. if so,
    /// show usage and exit.
    pub fn filterHelp(
        self: *const Self,
        parser: *const Parser
    ) Parser.ExitError!void {
        // test if this output is help
        var state = self;
        var p: *const Parser = parser;
        while (state.* == .path) : (state = state.path.out) {
            if (std.mem.eql(u8, state.path.subcommand, "help")) {
                try p.usageExit();
            }

            p = p.subcommands.get(state.path.subcommand).?;
        }
    }

    pub fn render(
        self: Self,
        ctx: *kz.Context,
        _: void
    ) Allocator.Error!kz.Ref {
        return switch (self) {
            .path => |path| path: {
                const name = try ctx.print(.{}, "{s}", .{path.subcommand});
                const size = kz.toOffset(ctx.getSize(name));

                const data = try path.out.render(ctx, {});
                break :path try ctx.unify(name, data, .{INDENT, size[1]});
            },
            .options => |opts| opts: {
                var keys = std.ArrayList(kz.Ref).init(ctx.ally);
                var values = std.ArrayList(kz.Ref).init(ctx.ally);
                defer keys.deinit();
                defer values.deinit();

                const cyan = kz.Style{ .fg = .cyan };
                try keys.append(try ctx.print(cyan, "name", .{}));
                try values.append(try ctx.print(cyan, "value", .{}));

                var entries = opts.iterator();
                while (entries.next()) |entry| {
                    const key = try ctx.print(.{}, "{s}", .{entry.key_ptr.*});
                    const val = try ctx.print(.{}, "{}", .{entry.value_ptr.*});
                    try keys.append(key);
                    try values.append(val);
                }

                break :opts try ctx.slap(
                    try ctx.stack(keys.items, .bottom, .{}),
                    try ctx.stack(values.items, .bottom, .{}),
                    .right,
                    .{ .space = 1 }
                );
            },
        };
    }
};

pub const Parser = struct {
    const Self = @This();

    /// a handle for app options
    pub const OptionId = struct { index: usize };

    ally: Allocator,
    parent: ?*const Parser = null,
    name: []u8,
    desc: []u8,

    subcommands: std.StringHashMapUnmanaged(*Parser) = .{},

    // handles reference into this
    options: std.ArrayListUnmanaged(Option) = .{},
    short: std.AutoHashMapUnmanaged(u8, OptionId) = .{},
    long: std.StringHashMapUnmanaged(OptionId) = .{},
    positional: std.ArrayListUnmanaged(OptionId) = .{},

    pub fn init(
        ally: Allocator,
        name: []const u8,
        desc: []const u8
    ) Allocator.Error!Self {
         return Self{
            .ally = ally,
            .name = try ally.dupe(u8, name),
            .desc = try ally.dupe(u8, desc),
        };
    }

    pub fn deinit(self: *Self) void {
        self.ally.free(self.name);
        self.ally.free(self.desc);

        var subs = self.subcommands.valueIterator();
        while (subs.next()) |sub| {
            sub.*.deinit();
            self.ally.destroy(sub.*);
        }
        self.subcommands.deinit(self.ally);

        for (self.options.items) |opt| opt.deinit(self.ally);
        self.options.deinit(self.ally);
        self.short.deinit(self.ally);
        self.long.deinit(self.ally);
        self.positional.deinit(self.ally);
    }

    fn acquire(self: *Self, opt: Option) Allocator.Error!OptionId {
        const id = OptionId{ .index = self.options.items.len };
        try self.options.append(self.ally, opt);

        return id;
    }

    /// invalidated by next acquire() call
    fn get(self: Self, id: OptionId) Option {
        return self.options.items[id.index];
    }

    /// args must have a long name, and short names are tolerated
    pub fn addArg(
        self: *Self,
        short: ?u8,
        long: []const u8,
        name: []const u8,
        desc: []const u8,
        ty: Type,
    ) Allocator.Error!void {
        const long_owned = try self.ally.dupe(u8, long);

        const id = try self.acquire(Option{
            .name = try self.ally.dupe(u8, name),
            .desc = try self.ally.dupe(u8, desc),
            .ty = ty,
            .short = short,
            .long = long_owned,
        });
        if (short) |c| try self.short.put(self.ally, c, id);
        try self.long.put(self.ally, long_owned, id);
    }

    pub fn addPositional(
        self: *Self,
        name: []const u8,
        desc: []const u8,
        ty: Type,
    ) Allocator.Error!void {
        const id = try self.acquire(Option{
            .name = try self.ally.dupe(u8, name),
            .desc = try self.ally.dupe(u8, desc),
            .ty = ty,
        });
        try self.positional.append(self.ally, id);
    }

    /// forwards args to subcommand init
    pub fn addSubcommand(
        self: *Self,
        name: []const u8,
        desc: []const u8,
    ) Allocator.Error!*Parser {
        // make parser
        const parser = try self.ally.create(Self);
        parser.* = try Self.init(self.ally, name, desc);
        parser.parent = self;

        // store parser
        std.debug.assert(!self.subcommands.contains(name));
        try self.subcommands.putNoClobber(self.ally, parser.name, parser);

        return parser;
    }

    /// adds the `help` subcommand
    pub fn addHelp(self: *Self) Allocator.Error!void {
        _ = try self.addSubcommand("help", "show this message");
    }

    /// used for errors
    const OptType = union(enum) {
        short: u8,
        long: []const u8,
        positional: []const u8, // opt name

        pub fn format(
            self: @This(),
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) @TypeOf(writer).Error!void {
            switch (self) {
                .short => |c| try writer.print("-{c}", .{c}),
                .long => |str| try writer.print("--{s}", .{str}),
                .positional => |name| try writer.print("<{s}>", .{name}),
            }
        }
    };

    fn errorUnknownOpt(self: Self, ot: OptType) ExitError!noreturn {
        try self.errorExit("unknown opt {}", .{ot});
    }

    fn errorExpectsData(self: Self, ot: OptType) ExitError!noreturn {
        try self.errorExit("{c} expects data", .{ot});
    }

    // iterator type for iterating args
    const ParseIterator = struct {
        slices: []const []const u8,
        index: usize = 0,

        fn next(iter: *@This()) ?[]const u8 {
            if (iter.index >= iter.slices.len) return null;

            defer iter.index += 1;
            return iter.slices[iter.index];
        }

        fn unget(iter: *@This()) void {
            std.debug.assert(iter.index > 0);
            iter.index -= 1;
        }
    };

    fn parseValue(
        self: Self,
        ot: OptType,
        ty: Type,
        iter: *ParseIterator
    ) ExitError!Value {
        if (ty == .flag) {
            return Value{ .flag = {} };
        }

        const str = iter.next() orelse {
            try self.errorExpectsData(ot);
        };

        return switch (ty) {
            .flag => unreachable,
            .nat => Value{
                .nat = std.fmt.parseInt(usize, str, 0) catch {
                    try self.errorExit("{} expected nat", .{ot});
                }
            },
            .string => Value{
                .string = try self.ally.dupe(u8, str)
            },
        };
    }

    fn parseSlice(
        self: *Self,
        args: []const []const u8
    ) ExitError!Output {
        // check for subcommand
        if (args.len > 0) if (self.subcommands.get(args[0])) |sp| {
            var subout = try self.ally.create(Output);
            subout.* = try sp.parseSlice(args[1..]);

            return Output{
                .path = .{
                    .subcommand = sp.name,
                    .out = subout,
                }
            };
        };

        var map = Output.Options{};

        // non-positional options
        var iter = ParseIterator{ .slices = args };
        while (iter.next()) |arg| {
            std.debug.assert(arg.len != 0);

            if (arg.len == 1 or arg[1] != '-') {
                // invalid opt; could be positional though so break
                iter.unget();
                break;
            } else if (arg[1] == '-') {
                // long opt
                const long = arg[2..];
                const id = self.long.get(long) orelse {
                    try self.errorUnknownOpt(.{ .long = long });
                };
                const opt = self.get(id);
                const ot = OptType{ .long = long };

                const val = try self.parseValue(ot, opt.ty, &iter);
                try map.put(self.ally, opt.name, val);
            } else {
                // short opt(s)
                const short = arg[1..];

                for (short[0..short.len - 1]) |c| {
                    const id = self.short.get(c) orelse {
                        try self.errorUnknownOpt(.{ .short = c });
                    };
                    const opt = self.get(id);

                    if (opt.ty != .flag) {
                        try self.errorExpectsData(.{ .short = c });
                    }

                    try map.put(self.ally, opt.name, .flag);
                }

                const c = short[short.len - 1];
                const ot = OptType{ .short = c };
                const id = self.short.get(c) orelse {
                    try self.errorUnknownOpt(ot);
                };
                const opt = self.get(id);

                const val = try self.parseValue(ot, opt.ty, &iter);
                try map.put(self.ally, opt.name, val);
            }
        }

        // positional opts
        for (self.positional.items) |id| {
            const opt = self.get(id);
            const ot = OptType{ .positional = opt.name };

            const val = try self.parseValue(ot, opt.ty, &iter);
            try map.put(self.ally, opt.name, val);
        }

        // ensure parsing is done
        if (iter.next()) |str| {
            try self.errorExit("not sure what to do with `{s}`", .{str});
        }

        return Output{ .options = map };
    }

    /// parse from the process args
    pub fn parse(self: *Self) ExitError!Output {
        // collect args
        var args = std.ArrayList([]const u8).init(self.ally);
        defer args.deinit();

        var iter = try std.process.argsWithAllocator(self.ally);
        defer iter.deinit();

        while (iter.next()) |arg| {
            try args.append(arg);
        }

        // parse args, skip executable name
        return try self.parseSlice(args.items[1..]);
    }

    fn renderName(
        ctx: *kz.Context,
        sty: kz.Style,
        parser: Parser
    ) Allocator.Error!kz.Ref {
        var names = std.ArrayList(kz.Ref).init(ctx.ally);
        defer names.deinit();

        var p: *const Parser = &parser;
        while (true) {
            try names.append(try ctx.print(sty, "{s}", .{p.name}));
            p = p.parent orelse break;
        }

        return try ctx.stack(names.items, .left, .{ .space = 1 });
    }

    fn renderSection(
        ctx: *kz.Context,
        name: []const u8,
        section: kz.Ref
    ) Allocator.Error!kz.Ref {
        const header = try ctx.stack(
            &.{
                try ctx.print(.{}, "[", .{}),
                try ctx.print(.{ .fg = .cyan, }, "{s}", .{name}),
                try ctx.print(.{}, "]", .{}),
            },
            .right,
            .{}
        );
        const size = kz.toOffset(ctx.getSize(header));

        return try ctx.unify(header, section, .{INDENT, size[1]});
    }

    fn renderType(ctx: *kz.Context, ty: Type) Allocator.Error!?kz.Ref {
        const sty = kz.Style{ .fg = .green };
        return switch (ty) {
            .flag => null,
            .nat, .string => try ctx.print(sty, "{s}", .{@tagName(ty)}),
        };
    }

    /// renders opt flags + accepted type
    fn renderOptUsage(ctx: *kz.Context, opt: Option) Allocator.Error!kz.Ref {
        const ty = try renderType(ctx, opt.ty);

        var texs = std.ArrayList(kz.Ref).init(ctx.ally);
        defer texs.deinit();

        const yellow = kz.Style{ .fg = .yellow };

        if (opt.short) |short| {
            try texs.append(try ctx.print(yellow, "-{c}", .{short}));
            if (ty) |got| {
                try texs.append(try ctx.print(.{}, " ", .{}));
                try texs.append(got);
            }
        }

        if (opt.long) |long| {
            if (texs.items.len > 0) {
                try texs.append(try ctx.print(.{}, ", ", .{}));
            }

            try texs.append(try ctx.print(yellow, "--{s}", .{long}));
            if (ty) |got| {
                try texs.append(try ctx.print(.{}, " ", .{}));
                try texs.append(got);
            }
        }

        // positional
        if (opt.long == null and opt.short == null) {
            try texs.append(ty.?);
        }

        return try ctx.stack(texs.items, .right, .{});
    }

    /// usage + desc
    fn renderOpt(ctx: *kz.Context, opt: Option) Allocator.Error!kz.Ref {
        const usage = try renderOptUsage(ctx, opt);
        const size = kz.toOffset(ctx.getSize(usage));

        return try ctx.unify(
            usage,
            try ctx.print(.{}, "{s}", .{opt.desc}),
            .{INDENT, size[1]}
        );
    }

    pub fn render(
        self: Self,
        ctx: *kz.Context,
        _: void
    ) Allocator.Error!kz.Ref {
        const ally = ctx.ally;

        const yellow = kz.Style{ .fg = .yellow };

        // header
        const desc = try ctx.print(.{}, "{s}", .{self.desc});
        var example = try ctx.stub();

        const name = try renderName(ctx, .{}, self);
        defer ctx.drop(name);

        if (self.options.items.len > 0) {
            var ex = std.ArrayList(kz.Ref).init(ally);
            defer ex.deinit();

            try ex.append(try ctx.clone(name));

            if (self.short.count() + self.long.count() > 0) {
                try ex.append(try ctx.print(.{}, "[options]", .{}));
            }

            for (self.positional.items) |pos| {
                const pos_name = self.get(pos).name;
                try ex.append(try ctx.print(.{}, "<{s}>", .{pos_name}));
            }

            const opt_ex = try ctx.stack(ex.items, .right, .{ .space = 1 });
            example = try ctx.slap(example, opt_ex, .bottom, .{});
        }

        // add subcommand example if necessary
        if (self.subcommands.count() > 0) {
            example = try ctx.slap(
                example,
                try ctx.slap(
                    try ctx.clone(name),
                    try ctx.print(.{}, "<subcommand>", .{}),
                    .right,
                    .{ .space = 1 }
                ),
                .bottom,
                .{}
            );
        }

        // positional
        var poss = std.ArrayList(kz.Ref).init(ally);
        defer poss.deinit();

        for (self.positional.items) |opt| {
            try poss.append(try renderOpt(ctx, self.get(opt)));
        }

        const pos = try ctx.stack(poss.items, .bottom, .{});

        // body
        var opts = std.ArrayList(kz.Ref).init(ally);
        defer opts.deinit();

        // TODO sort alphabetically

        for (self.options.items) |opt| {
            if (opt.short != null or opt.long != null) {
                try opts.append(try renderOpt(ctx, opt));
            }
        }

        const opt = try ctx.stack(opts.items, .bottom, .{});

        // subcommands
        var subs = std.ArrayList(kz.Ref).init(ally);
        defer subs.deinit();

        // TODO sort alphabetically

        var iter = self.subcommands.iterator();
        while (iter.next()) |entry| {
            const sub = entry.value_ptr.*;

            const sub_ex = try renderName(ctx, yellow, sub.*);
            const size = kz.toOffset(ctx.getSize(sub_ex));

            try subs.append(try ctx.unify(
                sub_ex,
                try ctx.print(.{}, "{s}", .{sub.desc}),
                .{INDENT, size[1]}
            ));
        }

        const subcommands = try ctx.stack(subs.items, .bottom, .{ .space = 1 });

        // put it together
        var pieces = std.BoundedArray(kz.Ref, 10){};

        pieces.appendAssumeCapacity(desc);
        pieces.appendAssumeCapacity(try renderSection(ctx, "usage", example));

        if (self.positional.items.len > 0) {
            const tex = try renderSection(ctx, "positional", pos);
            pieces.appendAssumeCapacity(tex);
        }

        if (opts.items.len > 0) {
            const tex = try renderSection(ctx, "options", opt);
            pieces.appendAssumeCapacity(tex);
        }

        if (self.subcommands.count() > 0) {
            const tex = try renderSection(ctx, "subcommands", subcommands);
            pieces.appendAssumeCapacity(tex);
        }

        return try ctx.stack(pieces.slice(), .bottom, .{ .space = 1 });
    }

    pub const ExitError = Allocator.Error || StdErrError;

    /// print usage message to stderr and exit(1)
    pub fn usageExit(self: Self) ExitError!noreturn {
        try kz.display(self.ally, {}, self, stderr);
        std.process.exit(1);
    }

    pub fn errorExit(
        self: Self,
        comptime fmt: []const u8,
        args: anytype
    ) ExitError!noreturn {
        var ctx = kz.Context.init(self.ally);
        defer ctx.deinit();

        const rendered = try ctx.stack(
            &.{
                try ctx.print(.{}, "[", .{}),
                try ctx.print(.{ .fg = .red }, "cli error", .{}),
                try ctx.print(.{}, "] ", .{}),
                try ctx.print(.{}, fmt, args),
            },
            .right,
            .{}
        );

        try ctx.write(rendered, stderr);
        std.process.exit(1);
    }
};

