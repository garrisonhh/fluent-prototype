const std = @import("std");
const util = @import("../util/util.zig");
const FlFile = @import("../file.zig");

const Context = FlFile.Context;
const Allocator = std.mem.Allocator;

pub const Error = util.CompileFailure
               || FlFile.Error
               || Allocator.Error;

const CharClass = enum {
    const Self = @This();

    space,
    lparen,
    rparen,
    lbracket,
    rbracket,
    digit,
    colon, // atoms
    squote, // quote (eventually)
    dquote, // strings
    lexical, // valid identifier chars that aren't digits

    fn is_alphanumeric(self: Self) bool {
        return self == .lexical or self == .digit;
    }
};

fn classify_char(ch: u8) CharClass {
    return switch (ch) {
        0, ' ', '\n', '\r', '\t' => .space,
        '(' => .lparen,
        ')' => .rparen,
        '[' => .lbracket,
        ']' => .rbracket,
        '0'...'9' => .digit,
        ':' => .colon,
        '\'' => .squote,
        '"' => .dquote,
        else => .lexical
    };
}

pub const Token = struct {
    pub const Type = enum {
        lparen,
        rparen,
        lbracket,
        rbracket,
        ident,
        atom,
        number,
        string,
        character
    };

    ttype: Type,
    view: []const u8
};

pub const TokenBuffer = struct {
    const Self = @This();

    tokens: std.MultiArrayList(Token),

    pub fn init() Self {
        return Self{
            .tokens = std.MultiArrayList(Token){}
        };
    }

    pub fn deinit(self: *Self, ctx: *Context) void {
        self.tokens.deinit(ctx.ally);
    }

    fn emit(
        self: *Self,
        ctx: *Context,
        ttype: Token.Type,
        view: []const u8
    ) !void {
        try self.tokens.append(ctx.ally, Token{
            .ttype = ttype,
            .view = view
        });
    }

    fn validate_parens(self: *const Self, ctx: *Context) Error!void {
        var level: i32 = 0;
        var warned_unmatched_at_zero = false;
        var dipped_below_zero = false;

        for (self.tokens.items(.ttype)) |ttype, i| {
            switch (ttype) {
                .lparen => {
                    // try to diagnose mismatched parens early on
                    if (!warned_unmatched_at_zero and level > 0) {
                        const view = self.tokens.items(.view)[i];
                        const loc = try FlFile.Location.of_slice(
                            ctx.lfile,
                            view
                        );

                        if (loc.char == 0) {
                            try ctx.add_message(
                                .warning,
                                "got here with an unmatched paren leftover. "
                                ++ "was that intentional?",
                                view
                            );
                            warned_unmatched_at_zero = true;
                        }
                    }

                    level += 1;
                },
                .rparen => {
                    level -= 1;

                    if (level < 0) {
                        try ctx.add_message(
                            .err,
                            "this paren is unmatched.",
                            self.tokens.items(.view)[i]
                        );
                        dipped_below_zero = true;
                    }
                },
                else => {}
            }
        }

        if (level > 0) {
            try ctx.add_message(
                .err,
                "reached end of program without matching a paren.",
                self.tokens.items(.view)[self.tokens.len - 1]
            );
        }

        if (level > 0 or dipped_below_zero) return util.CompilationFailed;
    }

    pub fn debug(self: *const Self) void {
        std.debug.print("tokens:\n", .{});

        const ttypes = self.tokens.items(.ttype);
        const views = self.tokens.items(.view);

        var i: usize = 0;
        while (i < self.tokens.len) : (i += 1) {
            std.debug.print(
                "{s:10} | `{s}`\n",
                .{@tagName(ttypes[i]), views[i]}
            );
        }

        std.debug.print("\n", .{});
    }
};

fn char_at(str: []const u8, index: usize) u8 {
    return if (index < str.len) str[index] else 0;
}

fn classify_char_at(str: []const u8, index: usize) CharClass {
    return classify_char(char_at(str, index));
}

fn tokenize(ctx: *Context) Allocator.Error!TokenBuffer {
    const str = ctx.lfile.text;
    var tbuf = TokenBuffer.init();

    var i: usize = 0;
    while (i < str.len) {
        const start = i;
        var start_class = classify_char_at(str, i);
        i += 1;

        // allow negative numbers (is this too hacky? idk)
        if (char_at(str, i - 1) == '-' and classify_char_at(str, i) == .digit) {
            start_class = .digit;
        }

        try switch (start_class) {
            .space => {},
            .lparen => tbuf.emit(ctx, .lparen, str[start..i]),
            .rparen => tbuf.emit(ctx, .rparen, str[start..i]),
            .lbracket => tbuf.emit(ctx, .lbracket, str[start..i]),
            .rbracket => tbuf.emit(ctx, .rbracket, str[start..i]),
            .digit => {
                while (classify_char_at(str, i).is_alphanumeric()) i += 1;
                try tbuf.emit(ctx, .number, str[start..i]);
            },
            .lexical, .colon => {
                while (classify_char_at(str, i).is_alphanumeric()) {
                    i += 1;
                }

                const token_type: Token.Type =
                    if (start_class == .colon) .atom else .ident;

                try tbuf.emit(ctx, token_type, str[start..i]);
            },
            .squote => {
                while (true) {
                    const ch = char_at(str, i);
                    const unescaped_squote =
                        ch == '\'' and char_at(str, i - 1) != '\\';

                    i += 1;

                    if (unescaped_squote) {
                        try tbuf.emit(ctx, .character, str[start..i]);
                        break;
                    } else if (ch == 0) {
                        try ctx.add_message(
                            .err,
                            "looks like this char literal never terminated.",
                            str[start..start + 1]
                        );
                        break;
                    }
                }
            },
            .dquote => {
                while (true) {
                    const ch = char_at(str, i);
                    const unescaped_dquote =
                        ch == '"' and char_at(str, i - 1) != '\\';

                    i += 1;

                    if (unescaped_dquote) {
                        try tbuf.emit(ctx, .string, str[start..i]);
                        break;
                    } else if (ch == 0) {
                        try ctx.add_message(
                            .err,
                            "looks like this string literal never terminated.",
                            str[start..start + 1]
                        );
                        break;
                    }
                }
            },
        };
    }

    return tbuf;
}

/// tokenizes ctx.lfile
pub fn lex(ctx: *Context) Error!TokenBuffer {
    const tbuf = try tokenize(ctx);
    try tbuf.validate_parens(ctx);

    return tbuf;
}