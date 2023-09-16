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
allocator: std.mem.Allocator,
errors: std.ArrayList([]const u8),

pub fn init(lexer: *Lexer, allocator: std.mem.Allocator) !*Parser {
    var parser = try allocator.create(Parser);
    errdefer parser.deinit();

    parser.* = Parser{
        .lexer = lexer,
        .allocator = allocator,
        .errors = std.ArrayList([]const u8).init(allocator),
    };

    parser.nextToken();
    parser.nextToken();

    return parser;
}

pub fn deinit(self: *Parser) void {
    self.errors.deinit();
    self.allocator.destroy(self);
}

pub fn parseProgram(self: *Parser, allocator: std.mem.Allocator) !*Program {
    var program = try Program.init(allocator);
    errdefer program.deinit();

    while (self.current_token.type != .Eof) {
        if (try self.parseStatement()) |statement| {
            try program.statements.append(statement);
        }
        self.nextToken();
    }

    return program;
}

fn parseStatement(self: *Parser) !?Statement {
    switch (self.current_token.type) {
        .Let => return try self.parseLetStatement(),
        else => return null,
    }
}

fn parseLetStatement(self: *Parser) !?Statement {
    var statement = Ast.LetStatement{ .token = self.current_token };

    if (!try self.expectPeek(.Identifier)) return null;
    statement.name = Ast.Identifier{
        .token = self.current_token,
        .value = self.current_token.literal,
    };

    if (!try self.expectPeek(.Assign)) return null;

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

fn expectPeek(self: *Parser, token_type: Token.Type) !bool {
    if (self.peekTokenIs(token_type)) {
        self.nextToken();
        return true;
    }
    try self.peekError(token_type);
    return false;
}

fn peekError(self: *Parser, token_type: Token.Type) !void {
    try self.errors.append(try std.fmt.allocPrint(self.allocator, "Expected token {s} but instead got token {s}", .{
        @tagName(token_type),
        @tagName(self.peek_token.type),
    }));
}

test "Let Statements" {
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
        // \\let x  5;
        // \\let = 10;
        // \\let 838383;
    ;

    var lexer = Lexer.init(input);

    var parser = try Parser.init(&lexer, std.testing.allocator);
    defer parser.deinit();

    var program = try parser.parseProgram(std.testing.allocator);
    defer program.deinit();

    if (parser.errors.items.len > 0) {
        std.debug.print("Parser failed with {d} errors:\n", .{parser.errors.items.len});
        for (parser.errors.items) |message| {
            std.debug.print("- {s}\n", .{message});
        }
        try std.testing.expect(false);
    }

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
