const std = @import("std");

const Token = @This();

type: Type,
literal: []const u8,

// TODO: Tagged union?
const Type = enum {
    Illegal,
    Eof,

    // Literals
    Identifier,
    Integer,

    // Symbols
    Assign,
    Plus,
    Comma,
    SemiColon,
    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Function,
    Let,
};

const map = std.ComptimeStringMap(Type, .{
    .{ "fn", .Function },
    .{ "let", .Let },
});

pub fn lookupIdentifier(literal: []const u8) ?Type {
    return map.get(literal);
}
