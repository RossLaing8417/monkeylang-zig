const std = @import("std");

const Lexer = @import("lexer.zig");
const Ast = @import("ast.zig");

const Parser = @This();
const Token = Lexer.Token;
const Program = Ast.Program;
const Statement = Ast.Statement;
const Expression = Ast.Expression;

const ParseError = std.mem.Allocator.Error || std.fmt.AllocPrintError || std.fmt.ParseIntError;

lexer: *Lexer,
current_token: Token = undefined,
peek_token: Token = undefined,
allocator: std.mem.Allocator,
errors: std.ArrayList([]const u8),

const Precedence = enum {
    Lowest,
    Equals,
    LessOrGreater,
    Sum,
    Product,
    Prefix,
    Call,
};

pub fn init(lexer: *Lexer, allocator: std.mem.Allocator) ParseError!*Parser {
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

pub fn parseProgram(self: *Parser, allocator: std.mem.Allocator) ParseError!*Program {
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

fn parseStatement(self: *Parser) ParseError!?*Statement {
    switch (self.current_token.type) {
        .Let => return try self.parseLetStatement(),
        .Return => return try self.parseReturnStatement(),
        else => return try self.parseExpressionStatement(),
    }
}

fn parseExpression(self: *Parser, precedence: Precedence) ParseError!*Expression {
    _ = precedence;
    switch (self.current_token.type) {
        .Identifier => return try self.parseIdentifier(),
        .Integer => return try self.parseInteger(),
        .Bang => return try self.parsePrefixExpression(),
        .Minus => return try self.parsePrefixExpression(),
        else => return undefined,
    }
}

fn parseExpressionStatement(self: *Parser) ParseError!*Statement {
    var expr_statement = try self.allocator.create(Ast.ExpressionStatement);
    expr_statement.* = Ast.ExpressionStatement{
        .token = self.current_token,
        .expression = try self.parseExpression(.Lowest),
    };

    if (self.peekTokenIs(.SemiColon)) {
        self.nextToken();
    }

    var statement = try self.allocator.create(Statement);
    statement.* = Statement{ .ExpressionStatement = expr_statement };

    return statement;
}

fn parseLetStatement(self: *Parser) ParseError!?*Statement {
    var let_statement = try self.allocator.create(Ast.LetStatement);
    let_statement.* = Ast.LetStatement{ .token = self.current_token };

    if (!try self.expectPeek(.Identifier)) return null;
    let_statement.name = try self.allocator.create(Ast.Identifier);
    let_statement.name.* = Ast.Identifier{
        .token = self.current_token,
        .value = self.current_token.literal,
    };

    if (!try self.expectPeek(.Assign)) return null;

    while (!self.currentTokenIs(.SemiColon)) {
        self.nextToken();
    }

    var statement = try self.allocator.create(Statement);
    statement.* = Statement{ .LetStatement = let_statement };

    return statement;
}

fn parseReturnStatement(self: *Parser) ParseError!*Statement {
    var return_statement = try self.allocator.create(Ast.ReturnStatement);
    return_statement.* = Ast.ReturnStatement{ .token = self.current_token };

    self.nextToken();

    while (!self.currentTokenIs(.SemiColon)) {
        self.nextToken();
    }

    var statement = try self.allocator.create(Statement);
    statement.* = Statement{ .ReturnStatement = return_statement };

    return statement;
}

fn parseIdentifier(self: *Parser) ParseError!*Expression {
    var identifier = try self.allocator.create(Ast.Identifier);
    identifier.* = Ast.Identifier{
        .token = self.current_token,
        .value = self.current_token.literal,
    };

    var expression = try self.allocator.create(Expression);
    expression.* = Expression{ .Identifier = identifier };

    return expression;
}

fn parseInteger(self: *Parser) ParseError!*Expression {
    _ = try std.fmt.parseInt(i64, self.current_token.literal, 10);

    var integer = try self.allocator.create(Ast.Integer);
    integer.* = Ast.Integer{
        .token = self.current_token,
        .value = self.current_token.literal,
    };

    var expression = try self.allocator.create(Expression);
    expression.* = Expression{ .Integer = integer };

    return expression;
}

fn parsePrefixExpression(self: *Parser) ParseError!*Expression {
    var prefix_expression = try self.allocator.create(Ast.PrefixExpression);
    prefix_expression.* = Ast.PrefixExpression{
        .token = self.current_token,
        .operator = self.current_token.literal,
    };

    self.nextToken();

    prefix_expression.operand = try self.parseExpression(.Prefix);

    var expression = try self.allocator.create(Expression);
    expression.* = Expression{ .PrefixExpression = prefix_expression };

    return expression;
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

fn expectPeek(self: *Parser, token_type: Token.Type) ParseError!bool {
    if (self.peekTokenIs(token_type)) {
        self.nextToken();
        return true;
    }
    try self.peekError(token_type);
    return false;
}

fn peekError(self: *Parser, token_type: Token.Type) ParseError!void {
    try self.errors.append(try std.fmt.allocPrint(self.allocator, "Expected token {s} but instead got token {s}", .{
        @tagName(token_type),
        @tagName(self.peek_token.type),
    }));
}

test "Let Statement" {
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;

    var lexer = Lexer.init(input);

    // TODO: Memory management...
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var allocator = arena.allocator();

    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    // var buffer: [input.len * 2]u8 = undefined;
    // var stream = std.io.fixedBufferStream(&buffer);
    // program.write(stream.writer());
    // std.debug.print("Let Statements:\n{s}\n", .{buffer});

    if (parser.errors.items.len > 0) {
        std.debug.print("Parser failed with {d} errors:\n", .{parser.errors.items.len});
        for (parser.errors.items) |message| {
            std.debug.print("- {s}\n", .{message});
        }
        try std.testing.expect(false);
    }

    try std.testing.expectEqual(program.statements.items.len, 3);

    const Expected = struct { value: []const u8 };
    const expected_values = [_]Expected{
        .{ .value = "x" },
        .{ .value = "y" },
        .{ .value = "foobar" },
    };

    for (expected_values, program.statements.items) |expected, statement| {
        try std.testing.expectEqualStrings("let", statement.tokenLiteral());

        switch (statement.*) {
            .LetStatement => |let_statement| {
                try std.testing.expectEqualStrings(expected.value, let_statement.name.value);
                try std.testing.expectEqualStrings(expected.value, let_statement.name.tokenLiteral());
            },
            else => unreachable,
        }
    }
}

test "Return Statement" {
    const input =
        \\return 5;
        \\return 10;
        \\return 993322;
    ;

    var lexer = Lexer.init(input);

    // TODO: Memory management...
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var allocator = arena.allocator();

    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    // var buffer: [input.len * 2]u8 = undefined;
    // var stream = std.io.fixedBufferStream(&buffer);
    // program.write(stream.writer());
    // std.debug.print("Return Statements:\n{s}\n", .{buffer});

    if (parser.errors.items.len > 0) {
        std.debug.print("Parser failed with {d} errors:\n", .{parser.errors.items.len});
        for (parser.errors.items) |message| {
            std.debug.print("- {s}\n", .{message});
        }
        try std.testing.expect(false);
    }

    try std.testing.expectEqual(program.statements.items.len, 3);

    const Expected = struct { value: []const u8 };
    const expected_values = [_]Expected{
        .{ .value = "return" },
        .{ .value = "return" },
        .{ .value = "return" },
    };

    for (expected_values, program.statements.items) |expected, statement| {
        try std.testing.expectEqualStrings(expected.value, statement.tokenLiteral());

        // switch (statement) {
        //     .ReturnStatement => |return_statement| {
        //         try std.testing.expectEqualStrings(expected.value, return_statement.name.value);
        //         try std.testing.expectEqualStrings(expected.value, return_statement.name.tokenLiteral());
        //     },
        //     else => unreachable,
        // }
    }
}

test "Identifier/Literal Expression" {
    const input =
        \\foobar;
        \\5;
    ;

    var lexer = Lexer.init(input);

    // TODO: Memory management...
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var allocator = arena.allocator();

    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    // var buffer: [input.len * 2]u8 = undefined;
    // var stream = std.io.fixedBufferStream(&buffer);
    // program.write(stream.writer());
    // std.debug.print("Identifier/Literal Expressions:\n{s}\n", .{buffer});

    if (parser.errors.items.len > 0) {
        std.debug.print("Parser failed with {d} errors:\n", .{parser.errors.items.len});
        for (parser.errors.items) |message| {
            std.debug.print("- {s}\n", .{message});
        }
        try std.testing.expect(false);
    }

    try std.testing.expectEqual(program.statements.items.len, 2);

    const Expected = struct { value: []const u8 };
    const expected_values = [_]Expected{
        .{ .value = "foobar" },
        .{ .value = "5" },
    };

    for (expected_values, program.statements.items) |expected, statement| {
        try std.testing.expectEqualStrings(expected.value, statement.tokenLiteral());

        switch (statement.*) {
            .ExpressionStatement => |expr_statement| {
                switch (expr_statement.expression.*) {
                    .Identifier => |identifier| {
                        try std.testing.expectEqualStrings(expected.value, identifier.value);
                        try std.testing.expectEqualStrings(expected.value, identifier.tokenLiteral());
                    },
                    .Integer => |integer| {
                        try std.testing.expectEqualStrings(expected.value, integer.value);
                        try std.testing.expectEqualStrings(expected.value, integer.tokenLiteral());
                    },
                    else => unreachable,
                }
            },
            else => unreachable,
        }
    }
}

test "Prefix Operators" {
    const input =
        \\!5;
        \\-15;
    ;

    var lexer = Lexer.init(input);

    // TODO: Memory management...
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var allocator = arena.allocator();

    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    var program = try parser.parseProgram(allocator);
    defer program.deinit();

    // var buffer: [input.len * 2]u8 = undefined;
    // var stream = std.io.fixedBufferStream(&buffer);
    // program.write(stream.writer());
    // std.debug.print("Prefix Operators:\n{s}\n", .{buffer});

    if (parser.errors.items.len > 0) {
        std.debug.print("Parser failed with {d} errors:\n", .{parser.errors.items.len});
        for (parser.errors.items) |message| {
            std.debug.print("- {s}\n", .{message});
        }
        try std.testing.expect(false);
    }

    try std.testing.expectEqual(program.statements.items.len, 2);

    const Expected = struct { prefix: []const u8, value: []const u8 };
    const expected_values = [_]Expected{
        .{ .prefix = "!", .value = "5" },
        .{ .prefix = "-", .value = "15" },
    };

    for (expected_values, program.statements.items) |expected, statement| {
        try std.testing.expectEqualStrings(expected.prefix, statement.tokenLiteral());

        switch (statement.*) {
            .ExpressionStatement => |expr_statement| {
                switch (expr_statement.expression.*) {
                    .PrefixExpression => |prefix_expression| {
                        try std.testing.expectEqualStrings(expected.prefix, prefix_expression.operator);
                        try std.testing.expectEqualStrings(expected.prefix, prefix_expression.tokenLiteral());
                        switch (prefix_expression.operand.*) {
                            .Identifier => |identifier| {
                                try std.testing.expectEqualStrings(expected.value, identifier.value);
                                try std.testing.expectEqualStrings(expected.value, identifier.tokenLiteral());
                            },
                            .Integer => |integer| {
                                try std.testing.expectEqualStrings(expected.value, integer.value);
                                try std.testing.expectEqualStrings(expected.value, integer.tokenLiteral());
                            },
                            else => unreachable,
                        }
                    },
                    else => unreachable,
                }
            },
            else => unreachable,
        }
    }
}

test "Writing" {
    var statement = Ast.Statement{
        .LetStatement = @constCast(&Ast.LetStatement{
            .token = Token{ .type = .Let, .literal = "let" },
            .name = @constCast(&Ast.Identifier{
                .token = Token{ .type = .Identifier, .literal = "myVar" },
                .value = "myVar",
            }),
            .value = @constCast(&Expression{
                .Identifier = @constCast(&Ast.Identifier{
                    .token = Token{ .type = .Identifier, .literal = "anotherVar" },
                    .value = "anotherVar",
                }),
            }),
        }),
    };

    // TODO: Memory management...
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var allocator = arena.allocator();

    var program = try Program.init(allocator);
    defer program.deinit();

    try program.statements.append(&statement);

    const input = "let myVar = anotherVar;\n";
    var buffer: [input.len]u8 = undefined;
    var buf_writer = std.io.fixedBufferStream(&buffer);

    program.write(buf_writer.writer());

    try std.testing.expectEqualStrings(input, &buffer);
}