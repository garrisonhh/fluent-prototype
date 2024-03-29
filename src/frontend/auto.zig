//! datatypes for fluent's parser + lexer generation

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const com = @import("common");
const Symbol = com.Symbol;

pub const Form = enum {
    file,
    call,

    // data structures
    coll,
    tuple,

    // pure sugar, eliminated on desugar step
    parens,
    comma,
    kv,
    stmt,

    // no children
    unit,
    number,
    string,
    symbol,

    // pointers/access
    addr,
    mut,
    dot,

    // types
    ptr,
    many_ptr,
    slice,
    arrow,

    // bool
    @"and",
    @"or",
    not,

    // math
    add,
    sub,
    mul,
    div,
    mod,
    shl,
    shr,

    // conditions
    eq,
    gt,
    lt,
    ge,
    le,

    // flow
    def,
    block,
    @"if",
    @"fn",
    lambda,

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
            // zig fmt: off
            .file, .call, .tuple, .unit, .symbol, .string, .number,
            .lambda, .slice,
            // zig fmt: on
            => @tagName(self),
            .coll => "collection literal",
            .comma => "sequence",
            .stmt => "statement expression",
            .arrow => "arrow operator",
            .parens => "parentheses",
            .kv => "key/value pair",
            .dot => "field access",
            .@"and" => "logical and",
            .@"or" => "logical or",
            .not => "logical not",
            .add => "add operator",
            .sub => "subtract operator",
            .mul => "multiply operator",
            .div => "divide operator",
            .mod => "modulus operator",
            .shl => "bitshift left operator",
            .shr => "bitshift right operator",
            .eq => "equality operator",
            .gt => "greater than operator",
            .lt => "less than operator",
            .ge => "greater than or equals operator",
            .le => "less than or equals operator",
            .addr => "addressing operator",
            .ptr => "pointer operator",
            .many_ptr => "many-pointer operator",
            .mut => "mutablity qualifier",
            .def => "declaration",
            .block => "block",
            .@"if" => "if expression",
            .@"fn" => "fn expression",
        };
    }

    /// the name of the form as a builtin (if it exists)
    ///
    /// these are used for translating RawExpr forms
    pub fn builtin(self: Form) ?Symbol {
        const builtin_map = comptime map: {
            const by_name = [_]Form{
                .block,
                .def,
                .@"if",
                .@"fn",

                .tuple,

                .@"and",
                .@"or",
                .not,
            };

            const pairs = @as([]const struct {
                @"0": Form,
                @"1": []const u8,
            }, &.{
                .{ .file, "ns" },

                .{ .coll, "array" },
                .{ .addr, "&" },

                .{ .add, "+" },
                .{ .sub, "-" },
                .{ .mul, "*" },
                .{ .div, "/" },
                .{ .mod, "%" },
                .{ .shl, "<<" },
                .{ .shr, ">>" },

                .{ .eq, "==" },
                .{ .gt, ">" },
                .{ .lt, "<" },
                .{ .ge, ">=" },
                .{ .le, "<=" },

                .{ .arrow, "Fn" },
                .{ .slice, "Slice" },
            });

            // strings -> symbols
            const LPair = struct {
                @"0": []const u8,
                @"1": Symbol,
            };

            var data: [pairs.len + by_name.len]LPair = undefined;
            for (pairs) |pair, i| {
                data[i].@"0" = @tagName(@as(Form, pair.@"0"));
                data[i].@"1" = Symbol.init(pair.@"1");
            }

            for (by_name) |form, i| {
                data[pairs.len + i].@"0" = @tagName(form);
                data[pairs.len + i].@"1" = Symbol.init(@tagName(form));
            }

            break :map std.ComptimeStringMap(Symbol, data);
        };

        return builtin_map.get(@tagName(self));
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
    word: []const u8,

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
                var buf: [512]u8 = undefined;
                var fba = std.heap.FixedBufferAllocator.init(&buf);
                const ally = fba.allocator();

                const escaped = com.stringEscape(ally, word) catch "<long str>";
                try writer.print("`{s}`", .{escaped});
            },
        }
    }
};

pub const Syntax = struct {
    const Self = @This();

    const Direction = enum { l, r };

    form: Form,
    fexprs: []const FormExpr,

    fn fexprsCount(comptime str: []const u8) usize {
        comptime {
            @setEvalBranchQuota(100_000);

            var count: usize = 0;
            var tokens = std.mem.tokenize(u8, str, " ");
            while (tokens.next()) |_| {
                count += 1;
            }

            return count;
        }
    }

    fn badFormExpr(comptime tok: []const u8) noreturn {
        @compileError("`" ++ tok ++ "` is not a valid FormExpr");
    }

    fn parseFormExpr(comptime tok: []const u8) FormExpr {
        comptime {
            @setEvalBranchQuota(100_000);

            return switch (tok[0]) {
                '`' => word: {
                    const word = tok[1..];
                    if (!SYMBOLS.has(word) and !KEYWORDS.has(word)) {
                        @compileError(
                            "`" ++ word ++ "` is not a symbol or keyword",
                        );
                    }

                    break :word FormExpr{ .word = word };
                },
                '$' => switch (tok.len) {
                    1 => FormExpr{ .expr = .{} },
                    2 => FormExpr{ .expr = .{
                        .flag = switch (tok[1]) {
                            '?' => .opt,
                            '*' => .any,
                            '+' => .multi,
                            else => badFormExpr(tok),
                        },
                    } },
                    else => badFormExpr(tok),
                },
                else => {
                    @compileError("`" ++ tok ++ "` is not a valid FormExpr");
                },
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
        comptime str: []const u8,
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
                // zig fmt: off
                const is_container = fexprs[i] == .expr
                                 and fexprs[i - 1] == .word
                                 and fexprs[i + 1] == .word;
                // zig fmt: on

                if (is_container) {
                    fexprs[i].expr.behavior = .reset;
                }
            }

            // check for LR flags
            if (fexprs[0] == .expr and fexprs[0].expr.flag != .one) {
                @compileError("parsing flags on a LR expr is out of scope");
            }

            return fexprs;
        }
    }

    pub fn init(
        form: Form,
        comptime dir: Direction,
        comptime syntax: []const u8,
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

    pub const Term = struct {
        prec: usize,
        rule: *const Syntax,
    };

    const Rules = std.BoundedArray(Term, 4);
    const RuleMap = std.StringHashMapUnmanaged(Rules);

    /// for rules that start with a word
    prefixed: RuleMap = .{},
    /// for rules that start with an expr with flag `.one`, this maps to the
    /// second position
    infixed: RuleMap = .{},
    /// rules that are neither prefixed nor infixed
    unfixed: Rules = .{},

    pub fn deinit(self: *Self, ally: Allocator) void {
        self.prefixed.deinit(ally);
        self.infixed.deinit(ally);
    }

    pub const PutError = Allocator.Error || error{Overflow};

    fn putFixed(
        ally: Allocator,
        map: *RuleMap,
        key: []const u8,
        term: Term,
    ) PutError!void {
        const res = try map.getOrPut(ally, key);
        if (!res.found_existing) {
            res.value_ptr.* = .{};
        }

        try res.value_ptr.append(term);
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
            try putFixed(ally, &self.prefixed, rule.fexprs[0].word, term);
        } else if (rule.fexprs.len > 1 and rule.fexprs[1] == .word) {
            try putFixed(ally, &self.infixed, rule.fexprs[1].word, term);
        } else {
            try self.unfixed.append(term);
        }
    }

    fn getFixed(map: RuleMap, key: []const u8) []const Term {
        return if (map.get(key)) |arr| arr.slice() else &.{};
    }

    pub fn getInfix(self: Self, key: []const u8) []const Term {
        return getFixed(self.infixed, key);
    }

    pub fn getPrefix(self: Self, key: []const u8) []const Term {
        return getFixed(self.prefixed, key);
    }

    /// debug print this table
    pub fn dump(self: Self) void {
        std.debug.print("[syntax table]\n", .{});

        var prefixed = self.prefixed.iterator();
        while (prefixed.next()) |entry| {
            std.debug.print(
                "`{s}` -> ({}) {}\n",
                .{
                    entry.key_ptr.*,
                    entry.value_ptr.prec,
                    entry.value_ptr.rule,
                },
            );
        }
        std.debug.print("\n", .{});

        var infixed = self.infixed.iterator();
        while (infixed.next()) |entry| {
            std.debug.print(
                "$ `{s}` -> ({}) {}\n",
                .{
                    entry.key_ptr.*,
                    entry.value_ptr.prec,
                    entry.value_ptr.rule,
                },
            );
        }
        std.debug.print("\n", .{});

        for (self.unfixed.items) |term| {
            std.debug.print("-> ({}) {}\n", .{ term.prec, term.rule });
        }
        std.debug.print("\n", .{});
    }
};

fn comptimeStringSet(comptime array: []const []const u8) type {
    comptime {
        @setEvalBranchQuota(100_000);

        // manipulate so it's consumable by ComptimeStringMap
        const KV = struct {
            @"0": []const u8,
            @"1": void = {},
        };
        var kvs: [array.len]KV = undefined;
        for (array) |keyword, i| {
            kvs[i] = KV{ .@"0" = keyword };
        }

        return std.ComptimeStringMap(void, kvs);
    }
}

/// syntax organized by precedence in ascending order
/// this is to be used for generating a SyntaxTable, not directly!
pub const SYNTAX: []const []const Syntax = t: {
    @setEvalBranchQuota(100_000);

    const x = Syntax.init;
    break :t &[_][]const Syntax{
        &.{
            x(Form.def, .r, "$ `:: $"),
            x(Form.stmt, .r, "$ `; $"),
            x(Form.parens, .l, "`( $? `)"),
            x(Form.coll, .l, "`{ $? `}"),
            x(Form.@"if", .r, "`if $ `then $ `else $"),
            x(Form.@"fn", .l, "`fn $ `= $"),
            x(Form.lambda, .r, "`| $? `| $"),
        },
        &.{
            x(Form.comma, .r, "$ `, $"),
        },
        &.{
            x(Form.kv, .r, "$ `: $"),
        },
        &.{
            x(Form.arrow, .r, "$ `-> $"),
        },
        &.{
            x(Form.@"and", .l, "$ `and $"),
            x(Form.@"or", .l, "$ `or $"),
            x(Form.@"or", .r, "`! $"),
        },
        &.{
            x(Form.eq, .l, "$ `== $"),
            x(Form.gt, .l, "$ `> $"),
            x(Form.lt, .l, "$ `< $"),
            x(Form.ge, .l, "$ `>= $"),
            x(Form.le, .l, "$ `<= $"),
        },
        &.{
            x(Form.shl, .l, "$ `<< $"),
            x(Form.shr, .l, "$ `>> $"),
        },
        &.{
            x(Form.add, .l, "$ `+ $"),
            x(Form.sub, .l, "$ `- $"),
        },
        &.{
            x(Form.mul, .l, "$ `* $"),
            x(Form.div, .l, "$ `/ $"),
            x(Form.mod, .l, "$ `% $"),
        },
        &.{
            x(Form.call, .l, "$ $+"),
        },
        &.{
            x(Form.addr, .r, "`& $"),
            x(Form.slice, .r, "`[ `] $"),
            x(Form.many_ptr, .r, "`[ `* `] $"),
            x(Form.ptr, .r, "`* $"),
            x(Form.mut, .r, "`mut $"),
        },
        &.{
            x(Form.dot, .l, "$ `. $"),
        },
    };
};
pub const MAX_PRECEDENCE = SYNTAX.len;

pub const KEYWORDS = comptimeStringSet(&.{
    "mut",
    "and",
    "or",
    "fn",
    "if",
    "then",
    "else",
});

// zig fmt: off
const SYMBOL_LIST = &[_][]const u8{
    "&", ".", "=", "::", "->", ",",  ";", ":", "!",
    // math
    "+", "-", "*", "/", "%", "<<", ">>",
    // cond
    "==", ">", "<", ">=", "<=",
    // matched
    "(", ")", "[",  "]", "{", "}", "|",
};
// zig fmt: on

pub const SYMBOLS = comptimeStringSet(SYMBOL_LIST);

pub const MAX_SYMBOL_LEN = max: {
    var max: usize = 0;
    for (SYMBOL_LIST) |sym| {
        max = @max(max, sym.len);
    }

    break :max max;
};
