const std = @import("std");

const Token = @import("token.zig");

const Lexer = @This();

input: []const u8,
position: usize = 0,
read_position: usize = 0,
ch: u8 = 0,

pub fn init(input: []const u8) Lexer {
    var lexer = Lexer{
        .input = input,
    };

    lexer.readChar();

    return lexer;
}

pub fn nextToken(self: *Lexer) Token {
    self.skipWhiteSpace();

    // TODO: This doesn't feel right...
    var token: Token = switch (self.ch) {
        '=' => .{ .type = .Assign, .literal = "=" },
        '+' => .{ .type = .Plus, .literal = "+" },
        ',' => .{ .type = .Comma, .literal = "," },
        ';' => .{ .type = .SemiColon, .literal = ";" },
        '(' => .{ .type = .LParen, .literal = "(" },
        ')' => .{ .type = .RParen, .literal = ")" },
        '{' => .{ .type = .LBrace, .literal = "{" },
        '}' => .{ .type = .RBrace, .literal = "}" },
        0 => .{ .type = .Eof, .literal = "" },
        'a'...'z', 'A'...'Z', '_' => {
            const literal = self.readIdentifier();
            return .{
                .type = if (Token.lookupIdentifier(literal)) |token| token else .Identifier,
                .literal = literal,
            };
        },
        '0'...'9' => {
            return .{ .type = .Integer, .literal = self.readNumber() };
        },
        else => .{ .type = .Illegal, .literal = "" },
    };

    self.readChar();

    return token;
}

fn readChar(self: *Lexer) void {
    if (self.read_position >= self.input.len) {
        self.ch = 0;
    } else {
        self.ch = self.input[self.read_position];
    }
    self.position = self.read_position;
    self.read_position += 1;
}

fn readIdentifier(self: *Lexer) []const u8 {
    var position = self.position;

    while (std.ascii.isAlphabetic(self.ch)) {
        self.readChar();
    }

    return self.input[position..self.position];
}

fn readNumber(self: *Lexer) []const u8 {
    var position = self.position;

    while (std.ascii.isDigit(self.ch)) {
        self.readChar();
    }

    return self.input[position..self.position];
}

fn skipWhiteSpace(self: *Lexer) void {
    while (std.ascii.isWhitespace(self.ch)) {
        self.readChar();
    }
}

test "1.3 The Lexer - Test 1" {
    const input = "=+(){},;";

    const expected = [_]Token{
        .{ .type = .Assign, .literal = "=" },
        .{ .type = .Plus, .literal = "+" },
        .{ .type = .LParen, .literal = "(" },
        .{ .type = .RParen, .literal = ")" },
        .{ .type = .LBrace, .literal = "{" },
        .{ .type = .RBrace, .literal = "}" },
        .{ .type = .Comma, .literal = "," },
        .{ .type = .SemiColon, .literal = ";" },
    };

    var lexer = Lexer.init(input);

    for (expected) |token| {
        try std.testing.expectEqualDeep(token, lexer.nextToken());
    }
}

test "1.3 The Lexer - Test 2" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\let add = fn(x, y) {
        \\  x + y;
        \\};
        \\let result = add(five, ten);
    ;

    const expected = [_]Token{
        .{ .type = .Let, .literal = "let" },
        .{ .type = .Identifier, .literal = "five" },
        .{ .type = .Assign, .literal = "=" },
        .{ .type = .Integer, .literal = "5" },
        .{ .type = .SemiColon, .literal = ";" },
        .{ .type = .Let, .literal = "let" },
        .{ .type = .Identifier, .literal = "ten" },
        .{ .type = .Assign, .literal = "=" },
        .{ .type = .Integer, .literal = "10" },
        .{ .type = .SemiColon, .literal = ";" },
        .{ .type = .Let, .literal = "let" },
        .{ .type = .Identifier, .literal = "add" },
        .{ .type = .Assign, .literal = "=" },
        .{ .type = .Function, .literal = "fn" },
        .{ .type = .LParen, .literal = "(" },
        .{ .type = .Identifier, .literal = "x" },
        .{ .type = .Comma, .literal = "," },
        .{ .type = .Identifier, .literal = "y" },
        .{ .type = .RParen, .literal = ")" },
        .{ .type = .LBrace, .literal = "{" },
        .{ .type = .Identifier, .literal = "x" },
        .{ .type = .Plus, .literal = "+" },
        .{ .type = .Identifier, .literal = "y" },
        .{ .type = .SemiColon, .literal = ";" },
        .{ .type = .RBrace, .literal = "}" },
        .{ .type = .SemiColon, .literal = ";" },
    };

    var lexer = Lexer.init(input);

    for (expected) |token| {
        try std.testing.expectEqualDeep(token, lexer.nextToken());
    }
}
