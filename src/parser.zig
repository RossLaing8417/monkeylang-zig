const std = @import("std");

const Lexer = @import("lexer.zig");
const Ast = @import("ast.zig");

const Parser = @This();
const Token = Lexer.Token;
const Program = Ast.Program;
const Statement = Ast.Statement;
const Expression = Ast.Expression;

lexer: *Lexer,
current_token: Token = undefined,
peek_token: Token = undefined,

pub fn init(lexer: *Lexer) Parser {
    var parser = Parser{
        .lexer = lexer,
    };

    parser.nextToken();
    parser.nextToken();

    return parser;
}

pub fn parseProgram(self: *Parser, allocator: std.mem.Allocator) !*Program {
    var program = try Program.init(allocator);
    errdefer program.deinit(allocator);

    while (self.current_token.type != .Eof) {
        if (self.parseStatement()) |statement| {
            try program.statements.append(statement);
        }
        self.nextToken();
    }

    return program;
}

fn parseStatement(self: *Parser) ?Statement {
    switch (self.current_token.type) {
        .Let => return self.parseLetStatement(),
        else => return null,
    }
}

fn parseLetStatement(self: *Parser) ?Statement {
    var statement = Ast.LetStatement{ .token = self.current_token };

    if (!self.expectPeek(.Identifier)) return null;
    statement.name = Ast.Identifier{
        .token = self.current_token,
        .value = self.current_token.literal,
    };

    if (!self.expectPeek(.Assign)) return null;

    while (!self.currentTokenIs(.SemiColon)) {
        self.nextToken();
    }

    return Statement{ .LetStatement = statement };
}

fn nextToken(self: *Parser) void {
    self.current_token = self.peek_token;
    self.peek_token = self.lexer.nextToken();
}

fn currentTokenIs(self: *Parser, token_type: Token.Type) bool {
    return self.current_token.type == token_type;
}

fn peekTokenIs(self: *Parser, token_type: Token.Type) bool {
    return self.peek_token.type == token_type;
}

fn expectPeek(self: *Parser, token_type: Token.Type) bool {
    if (self.peekTokenIs(token_type)) {
        self.nextToken();
        return true;
    }
    return false;
}

test "Let Statements" {
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;

    var lexer = Lexer.init(input);
    var parser = Parser.init(&lexer);
    var program = try parser.parseProgram(std.testing.allocator);
    defer program.deinit(std.testing.allocator);

    try std.testing.expectEqual(program.statements.items.len, 3);

    const Expected = struct { identifier: []const u8 };
    const expected = [_]Expected{
        .{ .identifier = "x" },
        .{ .identifier = "y" },
        .{ .identifier = "foobar" },
    };

    for (expected, program.statements.items) |value, statement| {
        try std.testing.expectEqualStrings("let", statement.tokenLiteral());

        switch (statement) {
            .LetStatement => |let_statement| {
                try std.testing.expectEqualStrings(value.identifier, let_statement.name.value);
                try std.testing.expectEqualStrings(value.identifier, let_statement.name.tokenLiteral());
            },
            else => unreachable,
        }
    }
}
