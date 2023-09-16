const std = @import("std");

const Token = @This();

type: Type,
literal: []const u8,

// TODO: Tagged union?
// I tried it but probs need to move this into lexer.zig for it to work
pub const Type = enum {
    Illegal,
    Eof,

    // Literals
    Identifier,
    Integer,

    // Operators
    Assign,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Bang,
    LessThan,
    GreaterThan,
    Equal,
    NotEqual,

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
    True,
    False,
    If,
    Else,
    Return,
};

const map = std.ComptimeStringMap(Type, .{
    .{ "fn", .Function },
    .{ "let", .Let },
    .{ "true", .True },
    .{ "false", .False },
    .{ "if", .If },
    .{ "else", .Else },
    .{ "return", .Return },
});

pub fn lookupIdentifier(literal: []const u8) ?Type {
    return map.get(literal);
}
