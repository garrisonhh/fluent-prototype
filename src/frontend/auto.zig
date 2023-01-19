//! datatypes for autogenerated parsing

const std = @import("std");

pub const KEYWORDS = kw: {
    const list = [_][]const u8{
        // parens
        "(", ")",
        // lists
        "[", "]",
        // if
        "if", "then", "else"
    };

    // manipulate so it's consumable by ComptimeStringMap
    const KV = struct {
        @"0": []const u8,
        @"1": void = {},
    };
    var kvs: [list.len]KV = undefined;
    for (list) |keyword, i| {
        kvs[i] = KV{ .@"0" = keyword };
    }

    break :kw std.ComptimeStringMap(void, kvs);
};

pub const Form = enum {
    // special
    file,
    list,
    parens,

    // general ops
    dot,

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

    // flow
    stmt,
    @"if",
};

pub const Precedence = struct {
    power: usize,
    /// for binary ops
    right: bool = false,
};

/// used by parseForm to encode syntax
pub const FormExpr = union(enum) {
    expr,
    keyword: []const u8,
};

pub const Syntax = struct {
    const Self = @This();

    pub const Kind = enum { prefixed, binary };

    form: Form,
    prec: Precedence,
    fexprs: []const FormExpr,
    kind: Kind,

    fn init(form: Form, prec: Precedence, comptime str: []const u8) Self {
        const fexprs = &parseFormExprs(str);
        std.debug.assert(fexprs.len > 0);

        const is_binary = fexprs.len == 3
                      and fexprs[0] == .keyword
                      and fexprs[1] == .expr
                      and fexprs[2] == .keyword;

        if (!is_binary) {
            // must be prefixed
            std.debug.assert(fexprs[0] == .keyword);
        }

        return Self{
            .form = form,
            .prec = prec,
            .fexprs = fexprs,
            .kind = if (is_binary) .binary else .prefixed,
        };
    }
};

fn countFormExprs(comptime str: []const u8) usize {
    comptime {
        var count: usize = 0;
        var iter = std.mem.tokenize(u8, str, " ");
        while (iter.next() != null) {
            count += 1;
        }

        return count;
    }
}

/// parses a formexpr at comptime.
/// this is a series of keywords and `<exprs>`
fn parseFormExprs(comptime str: []const u8) [countFormExprs(str)]FormExpr {
    comptime {
        var fexprs = std.BoundedArray(FormExpr, countFormExprs(str)){};

        var iter = std.mem.tokenize(u8, str, " ");
        while (iter.next()) |tok| {
            if (tok[0] == '<' and tok[tok.len - 1] == '>') {
                fexprs.appendAssumeCapacity(.expr);
            } else {
                fexprs.appendAssumeCapacity(.{ .keyword = tok });
            }
        }

        return fexprs.buffer;
    }
}

fn tableWhereCount(comptime kind: Syntax.Kind) usize {
    comptime {
        // count entries
        var count: usize = 0;
        for (SYNTAX_TABLE) |syntax| {
            count += @boolToInt(syntax.kind == kind);
        }

        return kind;
    }
}

fn tableWhere(comptime kind: Syntax.Kind) [tableWhereCount(kind)]Syntax {
    comptime {
        const count = tableWhereCount(kind);

        var arr = std.BoundedArray(Syntax, count){};
        for (SYNTAX_TABLE) |syntax| {
            if (syntax.kind == kind) {
                arr.appendAssumeCapacity(syntax);
            }
        }

        return arr.buffer;
    }
}

/// the definition of fluent syntax
pub const SYNTAX_TABLE = table: {
    // precedences
    const P = Precedence;

    const iota = struct {
        var n: usize = 0;
        fn f() usize {
            defer n += 1;
            return n;
        }
    }.f;

    const NONE           = P{ .power = iota() };
    const STATEMENT      = P{ .power = iota(), .right = true };
    const COMPARISON     = P{ .power = iota() };
    const ADDITIVE       = P{ .power = iota() };
    const MULTIPLICATIVE = P{ .power = iota() };
    const FIELD_ACCESS   = P{ .power = iota(), .right = true };

    // syntax definitions
    const x = Syntax.init;
    break :table [_]Syntax{
        x(.@"if", NONE,           "if <> then <> else <>"),

        x(.stmt,  STATEMENT,      "<> ; <>"),

        x(.eq,    COMPARISON,     "<> == <>"),
        x(.gt,    COMPARISON,     "<> > <>"),
        x(.lt,    COMPARISON,     "<> < <>"),
        x(.ge,    COMPARISON,     "<> >= <>"),
        x(.le,    COMPARISON,     "<> <= <>"),

        x(.add,   ADDITIVE,       "<> + <>"),
        x(.sub,   ADDITIVE,       "<> - <>"),

        x(.mul,   MULTIPLICATIVE, "<> * <>"),
        x(.div,   MULTIPLICATIVE, "<> / <>"),
        x(.mod,   MULTIPLICATIVE, "<> % <>"),

        x(.dot,   FIELD_ACCESS,   "<> . <>"),
    };
};

pub const PREFIXED_SYNTAX = tableWhere(.prefixed);
pub const BINARY_SYNTAX = tableWhere(.binary);
