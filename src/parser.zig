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

const Precedence = enum(u8) {
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
    var left_operand = switch (self.current_token.type) {
        .Identifier => try self.parseIdentifier(),
        .Integer => try self.parseInteger(),
        .Bang => try self.parsePrefixExpression(),
        .Minus => try self.parsePrefixExpression(),
        .True => try self.parseBoolean(),
        .False => try self.parseBoolean(),
        .LeftParen => try self.parseGroupedExpression(),
        .If => try self.parseIfExpression(),
        .Function => try self.parseFunctionLiteral(),
        else => unreachable,
    };

    while (true) {
        if (self.peekTokenIs(.SemiColon) or @intFromEnum(precedence) >= @intFromEnum(self.peekPrecedence())) {
            break;
        }

        // I don't know what the right pattern is here if there is any...?
        // #skillissue
        _ = switch (self.peek_token.type) {
            .Plus => true,
            .Minus => true,
            .Asterisk => true,
            .Slash => true,
            .Equal => true,
            .NotEqual => true,
            .LessThan => true,
            .GreaterThan => true,
            .LeftParen => true,
            else => return left_operand,
        };

        self.nextToken();

        left_operand = switch (self.current_token.type) {
            .LeftParen => try self.parseCallExpression(left_operand),
            else => try self.parseInfixExpression(left_operand),
        };
    }

    return left_operand;
}

fn parseGroupedExpression(self: *Parser) ParseError!*Expression {
    self.nextToken();

    var expression = self.parseExpression(.Lowest);

    _ = try self.expectPeek(.RightParen);

    return expression;
}

fn parseIfExpression(self: *Parser) ParseError!*Expression {
    var if_expression = try self.allocator.create(Ast.IfExpression);
    if_expression.* = Ast.IfExpression{
        .token = self.current_token,
        .condition = undefined,
        .consequence = undefined,
        .alternative = null,
    };

    _ = try self.expectPeek(.LeftParen);

    self.nextToken();
    if_expression.condition = try self.parseExpression(.Lowest);

    _ = try self.expectPeek(.RightParen);
    _ = try self.expectPeek(.LeftBrace);

    if_expression.consequence = try self.parseBlockStatement();

    if (self.peekTokenIs(.Else)) {
        self.nextToken();

        _ = try self.expectPeek(.LeftBrace);

        if_expression.alternative = try self.parseBlockStatement();
    }

    var expression = try self.allocator.create(Expression);
    expression.* = Expression{ .IfExpression = if_expression };

    return expression;
}

fn parseFunctionLiteral(self: *Parser) ParseError!*Expression {
    var function_literal = try Ast.FunctionLiteral.init(self.current_token, self.allocator);

    _ = try self.expectPeek(.LeftParen);

    while (!self.peekTokenIs(.RightParen)) {
        self.nextToken();

        var identifier = try self.allocator.create(Ast.Identifier);
        identifier.* = Ast.Identifier{
            .token = self.current_token,
            .value = self.current_token.literal,
        };

        try function_literal.parameters.append(identifier);

        if (self.peekTokenIs(.Comma)) {
            self.nextToken();
        }
    }

    _ = try self.expectPeek(.RightParen);
    _ = try self.expectPeek(.LeftBrace);

    function_literal.body = try self.parseBlockStatement();

    var expression = try self.allocator.create(Expression);
    expression.* = Expression{ .FunctionLiteral = function_literal };

    return expression;
}

fn parseCallExpression(self: *Parser, function: *Expression) ParseError!*Expression {
    var call_expression = try Ast.CallExpression.init(self.current_token, self.allocator);
    call_expression.function = function;

    while (!self.peekTokenIs(.RightParen)) {
        self.nextToken();

        var expression = try self.parseExpression(.Lowest);

        try call_expression.arguments.append(expression);

        if (self.peekTokenIs(.Comma)) {
            self.nextToken();
        }
    }

    _ = try self.expectPeek(.RightParen);

    var expression = try self.allocator.create(Expression);
    expression.* = Expression{ .CallExpression = call_expression };

    return expression;
}

fn parseBlockStatement(self: *Parser) ParseError!*Ast.BlockStatement {
    var block_statement = try Ast.BlockStatement.init(self.current_token, self.allocator);

    self.nextToken();

    while (!self.currentTokenIs(.RightBrace) and !self.currentTokenIs(.Eof)) {
        if (try self.parseStatement()) |statement| {
            try block_statement.statements.append(statement);
        }
        self.nextToken();
    }

    return block_statement;
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

    self.nextToken();

    let_statement.value = try self.parseExpression(.Lowest);

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

    return_statement.return_value = try self.parseExpression(.Lowest);

    if (self.peekTokenIs(.SemiColon)) {
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
    var integer = try self.allocator.create(Ast.Integer);
    integer.* = Ast.Integer{
        .token = self.current_token,
        .value = try std.fmt.parseInt(i64, self.current_token.literal, 10),
    };

    var expression = try self.allocator.create(Expression);
    expression.* = Expression{ .Integer = integer };

    return expression;
}

fn parseBoolean(self: *Parser) ParseError!*Expression {
    var boolean = try self.allocator.create(Ast.Boolean);
    boolean.* = Ast.Boolean{
        .token = self.current_token,
        .value = std.mem.eql(u8, self.current_token.literal, "true"),
    };

    var expression = try self.allocator.create(Expression);
    expression.* = Expression{ .Boolean = boolean };

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

fn parseInfixExpression(self: *Parser, left_operand: *Expression) ParseError!*Expression {
    var infix_expression = try self.allocator.create(Ast.InfixExpression);
    infix_expression.* = Ast.InfixExpression{
        .token = self.current_token,
        .operator = self.current_token.literal,
        .left_operand = left_operand,
    };

    const current_precedence = self.currentPrecedence();

    self.nextToken();

    // Decrement current_precedence param for right-associativity
    infix_expression.right_operand = try self.parseExpression(current_precedence);

    var expression = try self.allocator.create(Expression);
    expression.* = Expression{ .InfixExpression = infix_expression };

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
    try self.errors.append(try std.fmt.allocPrint(self.allocator, "Expected token '{s}' but got token '{s}' instead", .{
        @tagName(token_type),
        @tagName(self.peek_token.type),
    }));
}

fn precedenceMap(token_type: Token.Type) Precedence {
    return switch (token_type) {
        .Equal => .Equals,
        .NotEqual => .Equals,
        .LessThan => .LessOrGreater,
        .GreaterThan => .LessOrGreater,
        .Plus => .Sum,
        .Minus => .Sum,
        .Asterisk => .Product,
        .Slash => .Product,
        .LeftParen => .Call,
        else => .Lowest,
    };
}

fn peekPrecedence(self: *Parser) Precedence {
    return precedenceMap(self.peek_token.type);
}

fn currentPrecedence(self: *Parser) Precedence {
    return precedenceMap(self.current_token.type);
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

    const Expected = struct { identifier: []const u8, literal: []const u8, value: i64 };
    const expected_values = [_]Expected{
        .{ .identifier = "x", .literal = "5", .value = 5 },
        .{ .identifier = "y", .literal = "10", .value = 10 },
        .{ .identifier = "foobar", .literal = "838383", .value = 838383 },
    };

    for (expected_values, program.statements.items) |expected, statement| {
        try std.testing.expectEqualStrings("let", statement.tokenLiteral());

        switch (statement.*) {
            .LetStatement => |let_statement| {
                try std.testing.expectEqualStrings(expected.identifier, let_statement.name.value);
                try std.testing.expectEqualStrings(expected.identifier, let_statement.name.tokenLiteral());

                switch (let_statement.value.*) {
                    .Identifier => |identifier| {
                        try std.testing.expectEqualStrings(expected.identifier, identifier.value);
                        try std.testing.expectEqualStrings(expected.identifier, identifier.tokenLiteral());
                    },
                    .Integer => |integer| {
                        try std.testing.expectEqual(expected.value, integer.value);
                        try std.testing.expectEqualStrings(expected.literal, integer.tokenLiteral());
                    },
                    else => unreachable,
                }
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

    const Expected = struct { name: []const u8, literal: []const u8, value: i64 };
    const expected_values = [_]Expected{
        .{ .name = "return", .literal = "5", .value = 5 },
        .{ .name = "return", .literal = "10", .value = 10 },
        .{ .name = "return", .literal = "993322", .value = 993322 },
    };

    for (expected_values, program.statements.items) |expected, statement| {
        try std.testing.expectEqualStrings(expected.name, statement.tokenLiteral());

        switch (statement.*) {
            .ReturnStatement => |return_statement| {
                try std.testing.expectEqualStrings(expected.name, return_statement.tokenLiteral());
                switch (return_statement.return_value.*) {
                    .Integer => |integer| {
                        try std.testing.expectEqual(expected.value, integer.value);
                        try std.testing.expectEqualStrings(expected.literal, integer.tokenLiteral());
                    },
                    else => unreachable,
                }
            },
            else => unreachable,
        }
    }
}

test "Identifier/Literal Expression" {
    const input =
        \\foobar;
        \\5;
        \\true;
        \\false;
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

    try std.testing.expectEqual(program.statements.items.len, 4);

    const Expected = struct { literal: []const u8, value: union { string: []const u8, int: i64, boolean: bool } };
    const expected_values = [_]Expected{
        .{ .literal = "foobar", .value = .{ .string = "foobar" } },
        .{ .literal = "5", .value = .{ .int = 5 } },
        .{ .literal = "true", .value = .{ .boolean = true } },
        .{ .literal = "false", .value = .{ .boolean = false } },
    };

    for (expected_values, program.statements.items) |expected, statement| {
        try std.testing.expectEqualStrings(expected.literal, statement.tokenLiteral());

        switch (statement.*) {
            .ExpressionStatement => |expr_statement| {
                switch (expr_statement.expression.*) {
                    .Identifier => |identifier| {
                        try std.testing.expectEqualStrings(expected.value.string, identifier.value);
                        try std.testing.expectEqualStrings(expected.literal, identifier.tokenLiteral());
                    },
                    .Integer => |integer| {
                        try std.testing.expectEqual(expected.value.int, integer.value);
                        try std.testing.expectEqualStrings(expected.literal, integer.tokenLiteral());
                    },
                    .Boolean => |boolean| {
                        try std.testing.expectEqual(expected.value.boolean, boolean.value);
                        try std.testing.expectEqualStrings(expected.literal, boolean.tokenLiteral());
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

    const Expected = struct { prefix: []const u8, literal: []const u8, value: i64 };
    const expected_values = [_]Expected{
        .{ .prefix = "!", .literal = "5", .value = 5 },
        .{ .prefix = "-", .literal = "15", .value = 15 },
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
                            .Integer => |integer| {
                                try std.testing.expectEqual(expected.value, integer.value);
                                try std.testing.expectEqualStrings(expected.literal, integer.tokenLiteral());
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

test "Infix Operators" {
    const input =
        \\5 + 5;
        \\5 - 5;
        \\5 * 5;
        \\5 / 5;
        \\5 > 5;
        \\5 < 5;
        \\5 == 5;
        \\5 != 5;
        \\true == true;
        \\true != false;
        \\false == false;
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
    // std.debug.print("Infix Operators:\n{s}\n", .{buffer});

    if (parser.errors.items.len > 0) {
        std.debug.print("Parser failed with {d} errors:\n", .{parser.errors.items.len});
        for (parser.errors.items) |message| {
            std.debug.print("- {s}\n", .{message});
        }
        try std.testing.expect(false);
    }

    try std.testing.expectEqual(program.statements.items.len, 11);

    const Value = struct { literal: []const u8, value: union { int: i64, boolean: bool } };
    const Expected = struct { lhs: Value, operator: []const u8, rhs: Value };
    const expected_values = [_]Expected{
        .{ .lhs = .{ .literal = "5", .value = .{ .int = 5 } }, .operator = "+", .rhs = .{ .literal = "5", .value = .{ .int = 5 } } },
        .{ .lhs = .{ .literal = "5", .value = .{ .int = 5 } }, .operator = "-", .rhs = .{ .literal = "5", .value = .{ .int = 5 } } },
        .{ .lhs = .{ .literal = "5", .value = .{ .int = 5 } }, .operator = "*", .rhs = .{ .literal = "5", .value = .{ .int = 5 } } },
        .{ .lhs = .{ .literal = "5", .value = .{ .int = 5 } }, .operator = "/", .rhs = .{ .literal = "5", .value = .{ .int = 5 } } },
        .{ .lhs = .{ .literal = "5", .value = .{ .int = 5 } }, .operator = ">", .rhs = .{ .literal = "5", .value = .{ .int = 5 } } },
        .{ .lhs = .{ .literal = "5", .value = .{ .int = 5 } }, .operator = "<", .rhs = .{ .literal = "5", .value = .{ .int = 5 } } },
        .{ .lhs = .{ .literal = "5", .value = .{ .int = 5 } }, .operator = "==", .rhs = .{ .literal = "5", .value = .{ .int = 5 } } },
        .{ .lhs = .{ .literal = "5", .value = .{ .int = 5 } }, .operator = "!=", .rhs = .{ .literal = "5", .value = .{ .int = 5 } } },
        .{ .lhs = .{ .literal = "true", .value = .{ .boolean = true } }, .operator = "==", .rhs = .{ .literal = "true", .value = .{ .boolean = true } } },
        .{ .lhs = .{ .literal = "true", .value = .{ .boolean = true } }, .operator = "!=", .rhs = .{ .literal = "false", .value = .{ .boolean = false } } },
        .{ .lhs = .{ .literal = "false", .value = .{ .boolean = false } }, .operator = "==", .rhs = .{ .literal = "false", .value = .{ .boolean = false } } },
    };

    for (expected_values, program.statements.items) |expected, statement| {
        try std.testing.expectEqualStrings(expected.lhs.literal, statement.tokenLiteral());

        switch (statement.*) {
            .ExpressionStatement => |expr_statement| {
                switch (expr_statement.expression.*) {
                    .InfixExpression => |infix_expression| {
                        try std.testing.expectEqualStrings(expected.operator, infix_expression.operator);
                        try std.testing.expectEqualStrings(expected.operator, infix_expression.tokenLiteral());
                        switch (infix_expression.left_operand.*) {
                            .Integer => |integer| {
                                try std.testing.expectEqual(expected.lhs.value.int, integer.value);
                                try std.testing.expectEqualStrings(expected.lhs.literal, integer.tokenLiteral());
                            },
                            .Boolean => |boolean| {
                                try std.testing.expectEqual(expected.lhs.value.boolean, boolean.value);
                                try std.testing.expectEqualStrings(expected.lhs.literal, boolean.tokenLiteral());
                            },
                            else => unreachable,
                        }
                        switch (infix_expression.right_operand.*) {
                            .Integer => |integer| {
                                try std.testing.expectEqual(expected.rhs.value.int, integer.value);
                                try std.testing.expectEqualStrings(expected.rhs.literal, integer.tokenLiteral());
                            },
                            .Boolean => |boolean| {
                                try std.testing.expectEqual(expected.rhs.value.boolean, boolean.value);
                                try std.testing.expectEqualStrings(expected.rhs.literal, boolean.tokenLiteral());
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

test "Operator Precedence" {
    const Test = struct { input: []const u8, expected: []const u8 };
    var tests = [_]Test{
        .{ .input = "-a * b;\n", .expected = "((-a) * b);\n" },
        .{ .input = "!-a;\n", .expected = "(!(-a));\n" },
        .{ .input = "a + b + c;\n", .expected = "((a + b) + c);\n" },
        .{ .input = "a + b - c;\n", .expected = "((a + b) - c);\n" },
        .{ .input = "a * b * c;\n", .expected = "((a * b) * c);\n" },
        .{ .input = "a * b / c;\n", .expected = "((a * b) / c);\n" },
        .{ .input = "a + b / c;\n", .expected = "(a + (b / c));\n" },
        .{ .input = "a + b * c + d / e - f;\n", .expected = "(((a + (b * c)) + (d / e)) - f);\n" },
        .{ .input = "3 + 4;\n-5 * 5;\n", .expected = "(3 + 4);\n((-5) * 5);\n" },
        .{ .input = "5 > 4 == 3 < 4;\n", .expected = "((5 > 4) == (3 < 4));\n" },
        .{ .input = "5 < 4 != 3 > 4;\n", .expected = "((5 < 4) != (3 > 4));\n" },
        .{ .input = "3 + 4 * 5 == 3 * 1 + 4 * 5;\n", .expected = "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));\n" },
        .{ .input = "true;\n", .expected = "true;\n" },
        .{ .input = "false;\n", .expected = "false;\n" },
        .{ .input = "3 > 5 == false;\n", .expected = "((3 > 5) == false);\n" },
        .{ .input = "3 < 5 == true;\n", .expected = "((3 < 5) == true);\n" },
        .{ .input = "1 + (2 + 3) + 4;\n", .expected = "((1 + (2 + 3)) + 4);\n" },
        .{ .input = "(5 + 5) * 2;\n", .expected = "((5 + 5) * 2);\n" },
        .{ .input = "-(5 + 5);\n", .expected = "(-(5 + 5));\n" },
        .{ .input = "!(true == true);\n", .expected = "(!(true == true));\n" },
        .{ .input = "a + add(b * c) + d", .expected = "((a + add((b * c))) + d);\n" },
        .{ .input = "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", .expected = "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));\n" },
        .{ .input = "add(a + b + c * d / f + g)", .expected = "add((((a + b) + ((c * d) / f)) + g));\n" },
    };

    // TODO: Memory management...
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var allocator = arena.allocator();

    for (tests) |test_entry| {
        var lexer = Lexer.init(test_entry.input);

        var parser = try Parser.init(&lexer, allocator);
        defer parser.deinit();

        var program = try parser.parseProgram(allocator);
        defer program.deinit();

        var buffer = try allocator.alloc(u8, test_entry.expected.len);
        defer allocator.free(buffer);

        var buf_writer = std.io.fixedBufferStream(buffer);

        program.write(buf_writer.writer());

        try std.testing.expectEqualStrings(test_entry.expected, buffer);
    }
}

test "If Expression" {
    const input =
        \\if (x < y) { x }
        \\if (x < y) { x } else { y }
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
    // std.debug.print("If Expression:\n{s}\n", .{buffer});

    if (parser.errors.items.len > 0) {
        std.debug.print("Parser failed with {d} errors:\n", .{parser.errors.items.len});
        for (parser.errors.items) |message| {
            std.debug.print("- {s}\n", .{message});
        }
        try std.testing.expect(false);
    }

    try std.testing.expectEqual(@as(usize, 2), program.statements.items.len);

    const Expected = struct {
        token: []const u8,
        condition: struct { lhs: []const u8, operator: []const u8, rhs: []const u8 },
        consequence: []const u8,
        alternative: ?[]const u8,
    };

    const expected_values = [_]Expected{
        .{
            .token = "if",
            .condition = .{ .lhs = "x", .operator = "<", .rhs = "y" },
            .consequence = "x",
            .alternative = null,
        },
        .{
            .token = "if",
            .condition = .{ .lhs = "x", .operator = "<", .rhs = "y" },
            .consequence = "x",
            .alternative = "y",
        },
    };

    for (expected_values, program.statements.items) |expected, statement| {
        try std.testing.expectEqualStrings(expected.token, statement.tokenLiteral());

        switch (statement.*) {
            .ExpressionStatement => |expr_statement| {
                switch (expr_statement.expression.*) {
                    .IfExpression => |if_expression| {
                        try std.testing.expectEqualStrings(expected.token, if_expression.tokenLiteral());
                        switch (if_expression.condition.*) {
                            .InfixExpression => |infix_expression| {
                                try std.testing.expectEqualStrings(expected.condition.operator, infix_expression.operator);
                                try std.testing.expectEqualStrings(expected.condition.operator, infix_expression.tokenLiteral());
                                switch (infix_expression.left_operand.*) {
                                    .Identifier => |identifier| {
                                        try std.testing.expectEqualStrings(expected.condition.lhs, identifier.value);
                                        try std.testing.expectEqualStrings(expected.condition.lhs, identifier.tokenLiteral());
                                    },
                                    else => unreachable,
                                }
                                switch (infix_expression.right_operand.*) {
                                    .Identifier => |identifier| {
                                        try std.testing.expectEqualStrings(expected.condition.rhs, identifier.value);
                                        try std.testing.expectEqualStrings(expected.condition.rhs, identifier.tokenLiteral());
                                    },
                                    else => unreachable,
                                }
                            },
                            else => unreachable,
                        }
                        try std.testing.expectEqual(@as(usize, 1), if_expression.consequence.statements.items.len);
                        switch (if_expression.consequence.statements.items[0].*) {
                            .ExpressionStatement => |consq_statement| {
                                switch (consq_statement.expression.*) {
                                    .Identifier => |identifier| {
                                        try std.testing.expectEqualStrings(expected.consequence, identifier.value);
                                        try std.testing.expectEqualStrings(expected.consequence, identifier.tokenLiteral());
                                    },
                                    else => unreachable,
                                }
                            },
                            else => unreachable,
                        }
                        if (expected.alternative) |alternative| {
                            try std.testing.expectEqual(@as(usize, 1), if_expression.alternative.?.statements.items.len);
                            switch (if_expression.alternative.?.statements.items[0].*) {
                                .ExpressionStatement => |alt_statement| {
                                    switch (alt_statement.expression.*) {
                                        .Identifier => |identifier| {
                                            try std.testing.expectEqualStrings(alternative, identifier.value);
                                            try std.testing.expectEqualStrings(alternative, identifier.tokenLiteral());
                                        },
                                        else => unreachable,
                                    }
                                },
                                else => unreachable,
                            }
                        } else {
                            try std.testing.expectEqual(expected.alternative, null);
                        }
                    },
                    else => unreachable,
                }
            },
            else => unreachable,
        }
    }
}

test "Function Literal" {
    const input =
        \\fn(x, y) { x + y; }
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
    // std.debug.print("Function Literal:\n{s}\n", .{buffer});

    if (parser.errors.items.len > 0) {
        std.debug.print("Parser failed with {d} errors:\n", .{parser.errors.items.len});
        for (parser.errors.items) |message| {
            std.debug.print("- {s}\n", .{message});
        }
        try std.testing.expectEqual(@as(usize, 0), parser.errors.items.len);
    }

    try std.testing.expectEqual(@as(usize, 1), program.statements.items.len);

    const Expected = struct {
        token: []const u8,
        parameters: [2][]const u8,
        body: struct { lhs: []const u8, operator: []const u8, rhs: []const u8 },
    };

    const expected_values = [_]Expected{
        .{
            .token = "fn",
            .parameters = .{ "x", "y" },
            .body = .{ .lhs = "x", .operator = "+", .rhs = "y" },
        },
    };

    for (expected_values, program.statements.items) |expected, statement| {
        try std.testing.expectEqualStrings(expected.token, statement.tokenLiteral());

        switch (statement.*) {
            .ExpressionStatement => |expr_statement| {
                switch (expr_statement.expression.*) {
                    .FunctionLiteral => |function_literal| {
                        try std.testing.expectEqualStrings(expected.token, function_literal.tokenLiteral());
                        try std.testing.expectEqual(expected.parameters.len, function_literal.parameters.items.len);
                        for (expected.parameters, function_literal.parameters.items) |expected_param, function_param| {
                            try std.testing.expectEqualStrings(expected_param, function_param.value);
                            try std.testing.expectEqualStrings(expected_param, function_param.tokenLiteral());
                        }
                        try std.testing.expectEqual(@as(usize, 1), function_literal.body.statements.items.len);
                        switch (function_literal.body.statements.items[0].*) {
                            .ExpressionStatement => |body_statement| {
                                switch (body_statement.expression.*) {
                                    .InfixExpression => |infix_expression| {
                                        try std.testing.expectEqualStrings(expected.body.operator, infix_expression.operator);
                                        try std.testing.expectEqualStrings(expected.body.operator, infix_expression.tokenLiteral());
                                        switch (infix_expression.left_operand.*) {
                                            .Identifier => |identifier| {
                                                try std.testing.expectEqualStrings(expected.body.lhs, identifier.value);
                                                try std.testing.expectEqualStrings(expected.body.lhs, identifier.tokenLiteral());
                                            },
                                            else => unreachable,
                                        }
                                        switch (infix_expression.right_operand.*) {
                                            .Identifier => |identifier| {
                                                try std.testing.expectEqualStrings(expected.body.rhs, identifier.value);
                                                try std.testing.expectEqualStrings(expected.body.rhs, identifier.tokenLiteral());
                                            },
                                            else => unreachable,
                                        }
                                    },
                                    else => unreachable,
                                }
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

test "Call Expression" {
    const input =
        \\add(1, 2 * 3, 4 + 5)
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
    // std.debug.print("Call Expression:\n{s}\n", .{buffer});

    if (parser.errors.items.len > 0) {
        std.debug.print("Parser failed with {d} errors:\n", .{parser.errors.items.len});
        for (parser.errors.items) |message| {
            std.debug.print("- {s}\n", .{message});
        }
        try std.testing.expectEqual(@as(usize, 0), parser.errors.items.len);
    }

    try std.testing.expectEqual(@as(usize, 1), program.statements.items.len);

    const Value = struct { literal: []const u8, value: i64 };
    const ExpectedArgs = struct { lhs: Value, operator: []const u8, rhs: Value };
    var args = [_]ExpectedArgs{
        .{ .lhs = .{ .literal = "1", .value = 1 }, .operator = "", .rhs = .{ .literal = "", .value = undefined } },
        .{ .lhs = .{ .literal = "2", .value = 2 }, .operator = "*", .rhs = .{ .literal = "3", .value = 3 } },
        .{ .lhs = .{ .literal = "4", .value = 4 }, .operator = "+", .rhs = .{ .literal = "5", .value = 5 } },
    };

    const Expected = struct { token: []const u8, function: []const u8, arguments: []ExpectedArgs };
    const expected_values = [_]Expected{
        .{
            .token = "add",
            .function = "add",
            .arguments = args[0..],
        },
    };

    for (expected_values, program.statements.items) |expected, statement| {
        try std.testing.expectEqualStrings(expected.token, statement.tokenLiteral());

        switch (statement.*) {
            .ExpressionStatement => |expr_statement| {
                switch (expr_statement.expression.*) {
                    .CallExpression => |call_expression| {
                        try std.testing.expectEqualStrings("(", call_expression.tokenLiteral());
                        switch (call_expression.function.*) {
                            .Identifier => |identifier| {
                                try std.testing.expectEqualStrings(expected.function, identifier.value);
                                try std.testing.expectEqualStrings(expected.function, identifier.tokenLiteral());
                            },
                            else => unreachable,
                        }
                        try std.testing.expectEqual(expected.arguments.len, call_expression.arguments.items.len);
                        for (expected.arguments, call_expression.arguments.items) |expected_arg, call_arg| {
                            switch (call_arg.*) {
                                .Integer => |integer| {
                                    try std.testing.expectEqual(expected_arg.lhs.value, integer.value);
                                    try std.testing.expectEqualStrings(expected_arg.lhs.literal, integer.tokenLiteral());
                                },
                                .InfixExpression => |infix_expression| {
                                    try std.testing.expectEqualStrings(expected_arg.operator, infix_expression.operator);
                                    try std.testing.expectEqualStrings(expected_arg.operator, infix_expression.tokenLiteral());
                                    switch (infix_expression.left_operand.*) {
                                        .Integer => |integer| {
                                            try std.testing.expectEqual(expected_arg.lhs.value, integer.value);
                                            try std.testing.expectEqualStrings(expected_arg.lhs.literal, integer.tokenLiteral());
                                        },
                                        else => unreachable,
                                    }
                                    switch (infix_expression.right_operand.*) {
                                        .Integer => |integer| {
                                            try std.testing.expectEqual(expected_arg.rhs.value, integer.value);
                                            try std.testing.expectEqualStrings(expected_arg.rhs.literal, integer.tokenLiteral());
                                        },
                                        else => unreachable,
                                    }
                                },
                                else => unreachable,
                            }
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
