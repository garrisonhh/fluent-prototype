const std = @import("std");
const context = @import("../context.zig");
const lex = @import("lex.zig");

const Allocator = std.mem.Allocator;

const ParseError = lex.TokenizeError;

pub fn parse(ally: Allocator, handle: context.FileHandle) ParseError!void {
    const tokens = try lex.tokenize(ally, handle);

    for (tokens) |token| {
        if (std.mem.eql(u8, token.slice, "i32")) {
            _ = try context.postMessage(.debug, token.loc, "this is an i32!");
        }
    }
}
