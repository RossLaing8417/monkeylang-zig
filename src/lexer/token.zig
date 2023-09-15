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

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    LessThan,
    GreaterThan,

    // Symbols
    Comma,
    SemiColon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

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
