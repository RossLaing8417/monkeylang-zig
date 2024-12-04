const std = @import("std");

pub const Token = @import("token.zig");

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

    const token: Token = switch (self.ch) {
        '=' => blk: {
            if (self.peekChar() == '=') {
                self.readChar();
                break :blk .{ .type = .Equal, .literal = "==" };
            }
            break :blk .{ .type = .Assign, .literal = "=" };
        },
        '+' => .{ .type = .Plus, .literal = "+" },
        '-' => .{ .type = .Minus, .literal = "-" },
        '*' => .{ .type = .Asterisk, .literal = "*" },
        '/' => .{ .type = .Slash, .literal = "/" },
        '!' => blk: {
            if (self.peekChar() == '=') {
                self.readChar();
                break :blk .{ .type = .NotEqual, .literal = "!=" };
            }
            break :blk .{ .type = .Bang, .literal = "!" };
        },
        '<' => .{ .type = .LessThan, .literal = "<" },
        '>' => .{ .type = .GreaterThan, .literal = ">" },

        ',' => .{ .type = .Comma, .literal = "," },
        ';' => .{ .type = .SemiColon, .literal = ";" },
        '(' => .{ .type = .LeftParen, .literal = "(" },
        ')' => .{ .type = .RightParen, .literal = ")" },
        '{' => .{ .type = .LeftBrace, .literal = "{" },
        '}' => .{ .type = .RightBrace, .literal = "}" },
        '[' => .{ .type = .LeftBracket, .literal = "[" },
        ']' => .{ .type = .RightBracket, .literal = "]" },
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

        '"' => {
            const literal = self.readString();
            self.readChar();
            return .{ .type = .String, .literal = literal };
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
    const position = self.position;

    while (std.ascii.isAlphabetic(self.ch)) {
        self.readChar();
    }

    return self.input[position..self.position];
}

fn readNumber(self: *Lexer) []const u8 {
    const position = self.position;

    while (std.ascii.isDigit(self.ch)) {
        self.readChar();
    }

    return self.input[position..self.position];
}

fn readString(self: *Lexer) []const u8 {
    // Read the starting "
    self.readChar();

    const start = self.position;

    while (self.ch != '"') {
        if (self.ch == '\\' and self.peekChar() == '"') {
            self.readChar();
        }
        self.readChar();
    }

    return self.input[start..self.position];
}

fn skipWhiteSpace(self: *Lexer) void {
    while (std.ascii.isWhitespace(self.ch)) {
        self.readChar();
    }
}

fn peekChar(self: *Lexer) u8 {
    if (self.read_position >= self.input.len) {
        return 0;
    }
    return self.input[self.read_position];
}

test "Lexer" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\let add = fn(x, y) {
        \\  x + y;
        \\};
        \\let result = add(five, ten);
        \\!-/*5;
        \\5 < 10 > 5;
        \\if (5 < 10) {
        \\  return true;
        \\} else {
        \\  return false;
        \\}
        \\10 == 10;
        \\10 != 9;
        \\"foobar";
        \\"foo bar";
        \\"foo\"bar\"baz";
        \\[1, 2];
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
        .{ .type = .LeftParen, .literal = "(" },
        .{ .type = .Identifier, .literal = "x" },
        .{ .type = .Comma, .literal = "," },
        .{ .type = .Identifier, .literal = "y" },
        .{ .type = .RightParen, .literal = ")" },
        .{ .type = .LeftBrace, .literal = "{" },
        .{ .type = .Identifier, .literal = "x" },
        .{ .type = .Plus, .literal = "+" },
        .{ .type = .Identifier, .literal = "y" },
        .{ .type = .SemiColon, .literal = ";" },
        .{ .type = .RightBrace, .literal = "}" },
        .{ .type = .SemiColon, .literal = ";" },
        .{ .type = .Let, .literal = "let" },
        .{ .type = .Identifier, .literal = "result" },
        .{ .type = .Assign, .literal = "=" },
        .{ .type = .Identifier, .literal = "add" },
        .{ .type = .LeftParen, .literal = "(" },
        .{ .type = .Identifier, .literal = "five" },
        .{ .type = .Comma, .literal = "," },
        .{ .type = .Identifier, .literal = "ten" },
        .{ .type = .RightParen, .literal = ")" },
        .{ .type = .SemiColon, .literal = ";" },
        .{ .type = .Bang, .literal = "!" },
        .{ .type = .Minus, .literal = "-" },
        .{ .type = .Slash, .literal = "/" },
        .{ .type = .Asterisk, .literal = "*" },
        .{ .type = .Integer, .literal = "5" },
        .{ .type = .SemiColon, .literal = ";" },
        .{ .type = .Integer, .literal = "5" },
        .{ .type = .LessThan, .literal = "<" },
        .{ .type = .Integer, .literal = "10" },
        .{ .type = .GreaterThan, .literal = ">" },
        .{ .type = .Integer, .literal = "5" },
        .{ .type = .SemiColon, .literal = ";" },
        .{ .type = .If, .literal = "if" },
        .{ .type = .LeftParen, .literal = "(" },
        .{ .type = .Integer, .literal = "5" },
        .{ .type = .LessThan, .literal = "<" },
        .{ .type = .Integer, .literal = "10" },
        .{ .type = .RightParen, .literal = ")" },
        .{ .type = .LeftBrace, .literal = "{" },
        .{ .type = .Return, .literal = "return" },
        .{ .type = .True, .literal = "true" },
        .{ .type = .SemiColon, .literal = ";" },
        .{ .type = .RightBrace, .literal = "}" },
        .{ .type = .Else, .literal = "else" },
        .{ .type = .LeftBrace, .literal = "{" },
        .{ .type = .Return, .literal = "return" },
        .{ .type = .False, .literal = "false" },
        .{ .type = .SemiColon, .literal = ";" },
        .{ .type = .RightBrace, .literal = "}" },
        .{ .type = .Integer, .literal = "10" },
        .{ .type = .Equal, .literal = "==" },
        .{ .type = .Integer, .literal = "10" },
        .{ .type = .SemiColon, .literal = ";" },
        .{ .type = .Integer, .literal = "10" },
        .{ .type = .NotEqual, .literal = "!=" },
        .{ .type = .Integer, .literal = "9" },
        .{ .type = .SemiColon, .literal = ";" },
        .{ .type = .String, .literal = "foobar" },
        .{ .type = .SemiColon, .literal = ";" },
        .{ .type = .String, .literal = "foo bar" },
        .{ .type = .SemiColon, .literal = ";" },
        .{ .type = .String, .literal = "foo\\\"bar\\\"baz" },
        .{ .type = .SemiColon, .literal = ";" },
        .{ .type = .LeftBracket, .literal = "[" },
        .{ .type = .Integer, .literal = "1" },
        .{ .type = .Comma, .literal = "," },
        .{ .type = .Integer, .literal = "2" },
        .{ .type = .RightBracket, .literal = "]" },
        .{ .type = .SemiColon, .literal = ";" },
        .{ .type = .Eof, .literal = "" },
    };

    var lexer = Lexer.init(input);

    for (expected) |token| {
        const result = lexer.nextToken();
        try std.testing.expectEqual(token.type, result.type);
        try std.testing.expectEqualStrings(token.literal, result.literal);
    }
}
