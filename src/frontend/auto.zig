//! datatypes for autogenerated parsing

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");

pub const Form = enum {
    file,
    call,
    list,

    parens,
    dot,
    dict,

    // no children
    unit,
    number,
    string,
    symbol,

    // math
    add,
    sub,
    mul,
    div,
    mod,

    // conditions
    eq,
    gt,
    lt,
    ge,
    le,

    // ptrs
    addr,

    // flow
    stmt,
    @"if",
    @"fn",
    def,

    /// maps tagName to enum value. used for parsing form exprs.
    const map = map: {
        const arr = std.enums.values(Form);

        const KV = struct {
            @"0": []const u8,
            @"1": Form,
        };

        var kvs: [arr.len]KV = undefined;
        for (arr) |tag, i| {
            kvs[i] = KV{
                .@"0" = @tagName(tag),
                .@"1" = tag,
            };
        }

        break :map std.ComptimeStringMap(Form, kvs);
    };

    /// used for generating error messages etc.
    pub fn name(self: Form) []const u8 {
        return switch (self) {
            .file, .call, .list, .unit, .symbol, .string, .number
                => @tagName(self),
            .parens => "parentheses",
            .dict => "dictionary literal",
            .seq => "sequence",
            .anno => "annotation",
            .dot => "field access",
            .add => "add operator",
            .sub => "subtract operator",
            .mul => "multiply operator",
            .div => "divide operator",
            .mod => "modulus operator",
            .eq => "equality operator",
            .gt => "greater than operator",
            .lt => "less than operator",
            .ge => "greater than or equals operator",
            .le => "less than or equals operator",
            .addr => "address of operator",
            .stmt => "statement expression",
            .@"if" => "if expression",
            .@"fn" => "fn expression",
            .def => "declaration",
        };
    }
};

/// used by parseForm to encode syntax
pub const FormExpr = union(enum) {
    pub const ClimbBehavior = enum { same, increase, reset };
    pub const Flag = enum { one, opt, any, multi };

    pub const Expr = struct {
        flag: Flag = .one,
        behavior: ClimbBehavior = .same,

        pub fn innerPrec(self: @This(), prec: usize) usize {
            return switch (self.behavior) {
                .same => prec,
                .increase => prec + 1,
                .reset => 0,
            };
        }
    };

    const Self = @This();
    const Tag = std.meta.Tag(Self);

    expr: Expr,
    word: []const u8, // TODO use common.Symbol

    pub fn hash(self: Self) u64 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHashStrat(&hasher, self, .Deep);

        return hasher.final();
    }

    pub fn eql(self: Self, other: Self) bool {
        return @as(Tag, self) == @as(Tag, other) and switch (self) {
            .word => std.mem.eql(u8, self.word, other.word),
            .expr => std.meta.eql(self.expr, other.expr),
        };
    }

    /// whether this fexpr represents this word
    pub fn isWord(self: Self, word: []const u8) bool {
        return self == .word and std.mem.eql(u8, word, self.word);
    }

    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        switch (self) {
            .expr => |expr| {
                try writer.writeByte('$');

                try writer.writeAll(switch (expr.behavior) {
                    .same => "-",
                    .increase => "^",
                    .reset => "_",
                });

                try writer.writeAll(switch (expr.flag) {
                    .one => "",
                    .opt => "?",
                    .any => "*",
                    .multi => "+",
                });
            },
            .word => |word| {
                try writer.writeByte('`');
                try writer.writeAll(word);
            }
        }
    }
};

pub const Syntax = struct {
    const Self = @This();

    const Direction = enum { l, r };

    form: Form,
    fexprs: []const FormExpr,

    fn fexprsCount(comptime str: []const u8) usize {
        var count: usize = 0;
        var tokens = std.mem.tokenize(u8, str, " ");
        while (tokens.next()) |_| {
            count += 1;
        }

        return count;
    }

    fn badFormExpr(comptime tok: []const u8) noreturn {
        @compileError("`" ++ tok ++ "` is not a valid FormExpr");
    }

    fn parseFormExpr(comptime tok: []const u8) FormExpr {
        comptime {
            return switch (tok[0]) {
                '`' => word: {
                    const word = tok[1..];
                    if (!SYMBOLS.has(word) and !KEYWORDS.has(word)) {
                        @compileError(
                            "`" ++ word ++ "` is not a symbol or keyword"
                        );
                    }

                    break :word FormExpr{ .word = word };
                },
                '$' => switch (tok.len) {
                    1 => FormExpr{ .expr = .{} },
                    2 => FormExpr{
                        .expr = .{
                            .flag = switch (tok[1]) {
                                '?' => .opt,
                                '*' => .any,
                                '+' => .multi,
                                else => badFormExpr(tok)
                            },
                        }
                    },
                    else => badFormExpr(tok)
                },
                else => {
                    @compileError("`" ++ tok ++ "` is not a valid FormExpr");
                }
            };
        }
    }

    /// compiles a series of expressions separated by spaces. expression can
    /// be:
    /// - '`' followed by a keyword or symbol
    /// - '$' representing a child expr, followed by one of '*', '?', or '+'
    ///   which represent their regex counterparts
    fn parseForm(
        comptime dir: Direction,
        comptime str: []const u8
    ) [fexprsCount(str)]FormExpr {
        comptime {
            @setEvalBranchQuota(100_000);

            var fexprs: [fexprsCount(str)]FormExpr = undefined;
            var index: usize = 0;

            std.debug.assert(fexprs.len > 0);

            var tokens = std.mem.tokenize(u8, str, " ");
            while (tokens.next()) |tok| : (index += 1) {
                fexprs[index] = parseFormExpr(tok);
            }

            // directions are handled by changing the precedence climbing rule
            // for LR or RR exprs to ignore all same-precedence rules. this is
            // so that the parser doesn't have to understand directions
            const inc_index = switch (dir) {
                .l => fexprs.len - 1,
                .r => 0,
            };

            if (fexprs[inc_index] == .expr) {
                fexprs[inc_index].expr.behavior = .increase;
            }

            // if an expression is inside of two keywords, it is a 'container'
            // and precedence should be reset when parsing it
            var i: usize = 1;
            while (i < fexprs.len - 1) : (i += 1) {
                const is_container = fexprs[i] == .expr
                                 and fexprs[i - 1] == .word
                                 and fexprs[i + 1] == .word;

                if (is_container) {
                    fexprs[i].expr.behavior = .reset;
                }
            }

            return fexprs;
        }
    }

    pub fn init(
        form: Form,
        comptime dir: Direction,
        comptime syntax: []const u8
    ) Self {
        const fexprs = &parseForm(dir, syntax);
        std.debug.assert(fexprs.len > 0);

        return Self{
            .form = form,
            .fexprs = fexprs,
        };
    }

    /// whether this rule is left-recursive
    pub fn isLR(self: Self) bool {
        const left = self.fexprs[0];
        return left == .expr and left.expr.behavior == .same;
    }

    /// whether this rule starts with a word
    pub fn isPrefixed(self: Self) bool {
        return self.fexprs[0] == .word;
    }

    /// whether this rule starts with a specific word
    pub fn hasPrefix(self: Self, prefix: []const u8) bool {
        const left = self.fexprs[0];
        return left == .word and std.mem.eql(u8, prefix, left.word);
    }

    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.print("[{s}]", .{@tagName(self.form)});

        for (self.fexprs) |fexpr| {
            try writer.print(" {}", .{fexpr});
        }
    }
};

/// data structure used to make syntax searches fast enough
pub const SyntaxTable = struct {
    const Self = @This();

    const Term = struct {
        prec: usize,
        rule: *const Syntax,
    };

    /// for rules that start with a word
    prefixed: std.StringHashMapUnmanaged(Term) = .{},
    /// for rules that start with an expr with flag `.one`, this maps to the
    /// second position
    infixed: std.StringHashMapUnmanaged(Term) = .{},
    /// rules that are neither prefixed nor infixed
    unfixed: std.ArrayListUnmanaged(Term) = .{},

    pub fn deinit(self: *Self, ally: Allocator) void {
        self.prefixed.deinit(ally);
        self.infixed.deinit(ally);
        self.unfixed.deinit(ally);
    }

    pub const PutError =
        Allocator.Error
     || error { OverwroteRule };

    fn putUnique(
        ally: Allocator,
        map: *std.StringHashMapUnmanaged(Term),
        key: []const u8,
        term: Term,
    ) PutError!void {
        const res = try map.getOrPut(ally, key);
        if (res.found_existing) {
            return PutError.OverwroteRule;
        }

        res.value_ptr.* = term;
    }

    pub fn put(
        self: *Self,
        ally: Allocator,
        rule: *const Syntax,
        prec: usize,
    ) PutError!void {
        std.debug.assert(rule.fexprs.len > 0);

        const term = Term{
            .prec = prec,
            .rule = rule,
        };

        if (rule.fexprs[0] == .word) {
            try putUnique(ally, &self.prefixed, rule.fexprs[0].word, term);
        } else if (rule.fexprs.len > 1 and rule.fexprs[1] == .word) {
            try putUnique(ally, &self.infixed, rule.fexprs[1].word, term);
        } else {
            try self.unfixed.append(ally, term);
        }
    }

    /// debug print this table
    pub fn dump(self: Self) void {
        std.debug.print("[syntax table]\n", .{});

        var prefixed = self.prefixed.iterator();
        while (prefixed.next()) |entry| {
            std.debug.print(
                "`{s}` -> ({}) {}\n",
                .{entry.key_ptr.*, entry.value_ptr.prec, entry.value_ptr.rule},
            );
        }
        std.debug.print("\n", .{});

        var infixed = self.infixed.iterator();
        while (infixed.next()) |entry| {
            std.debug.print(
                "$ `{s}` -> ({}) {}\n",
                .{entry.key_ptr.*, entry.value_ptr.prec, entry.value_ptr.rule},
            );
        }
        std.debug.print("\n", .{});

        for (self.unfixed.items) |term| {
            std.debug.print("-> ({}) {}\n", .{term.prec, term.rule});
        }
        std.debug.print("\n", .{});
    }
};

fn comptimeStringSet(comptime list: []const []const u8) type {
    comptime {
        // manipulate so it's consumable by ComptimeStringMap
        const KV = struct {
            @"0": []const u8,
            @"1": void = {},
        };
        var kvs: [list.len]KV = undefined;
        for (list) |keyword, i| {
            kvs[i] = KV{ .@"0" = keyword };
        }

        return std.ComptimeStringMap(void, kvs);
    }
}

/// syntax organized by precedence in ascending order
/// this is to be used for generating a SyntaxTable, not directly!
pub const SYNTAX = t: {
    const x = Syntax.init;
    break :t &[_][]const Syntax {
        &.{
            x(.parens, .l, "$ `\n\n"),
        },
        &.{
            x(.def,    .r, "$ `:: $"),
        },
        &.{
            x(.parens, .l, "`( $? `)"),
            x(.@"if",  .r, "`if $ `then $ `else $"),
        },
        &.{
            x(.add,    .l, "$ `+ $"),
            x(.sub,    .l, "$ `- $"),
        },
        &.{
            x(.mul,    .l, "$ `* $"),
            x(.div,    .l, "$ `/ $"),
            x(.mod,    .l, "$ `% $"),
        },
        &.{
            x(.call,   .l, "$ $+"),
        },
        &.{
            x(.addr,   .l, "`& $"),
        },
        &.{
            x(.dot,    .l, "$ `. $"),
        },
    };
};
pub const MAX_PRECEDENCE = SYNTAX.len;

pub const KEYWORDS = comptimeStringSet(&.{
    "fn",
    "if", "then", "else",
});

const SYMBOL_LIST = &[_][]const u8{
    "\n\n",
    "&", ".", "=", "::", "->", ",", ";", ":",
    // math
    "=", "+", "-", "*", "/", "%",
    // cond
    "==", ">", "<", ">=", "<=",
    // matched
    "(", ")", "[", "]", "{", "}", "|",
};

pub const SYMBOLS = comptimeStringSet(SYMBOL_LIST);

pub const MAX_SYMBOL_LEN = max: {
    var max: usize = 0;
    for (SYMBOL_LIST) |sym| {
        max = @max(max, sym.len);
    }

    break :max max;
};