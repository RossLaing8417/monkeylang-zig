const std = @import("std");

const Lexer = @import("lexer.zig");
const Ast = @import("ast.zig");

const Parser = @This();
const Node = Ast.Node;
const Token = Lexer.Token;

const Error = std.mem.Allocator.Error || std.fmt.AllocPrintError || std.fmt.ParseIntError;

allocator: std.mem.Allocator,
tokens: []const Token,
tok_i: usize,
nodes: std.ArrayList([]const Node),
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

pub fn parseProgram(self: *Parser) !void {
    while (self.tokes[self.tok_i].type != .Eof) {
        if (try self.parseStatement()) |node| {
            try self.append(node);
        }
    }
}

fn parseStatement(self: *Parser) !Node {
    const node = switch (self.tokens[self.tok_i].type) {
        .Let => try self.parseLetStatement(),
        .Return => try self.parseReturnStatement(),
        else => try self.parseExpressionStatement(),
    };
    _ = self.consumeToken(.SemiColon);
    return node;
}

fn parseExpression(self: *Parser, precedence: Precedence) !Node {
    var left_operand = switch (self.tokens[self.tok_i].type) {
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
        const token = self.tokens[self.tok_i];
        if (self.currentTokenIs(.SemiColon) or @intFromEnum(precedence) >= @intFromEnum(self.tokenPrecedence(token))) {
            break;
        }

        // I don't know what the right pattern is here if there is any...?
        // #skillissue
        _ = switch (self.tokens[self.tok_i + 1].type) {
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

        left_operand = switch (self.tokens[self.tok_i].type) {
            .LeftParen => try self.parseCallExpression(left_operand),
            else => try self.parseInfixExpression(left_operand),
        };
    }

    return left_operand;
}

fn parseGroupedExpression(self: *Parser) !Node {
    var grouped_expression = try self.allocator.create(Ast.GroupedExpression);

    grouped_expression.* = .{
        .token = try self.expectToken(.LeftParen),
        .expression = self.parseExpression(.Lowest),
    };

    _ = try self.expectToken(.RightParen);

    return .{ .GroupedExpression = grouped_expression };
}

fn parseIfExpression(self: *Parser) !Node {
    var if_expression = try self.allocator.create(Ast.IfExpression);
    if_expression.* = Ast.IfExpression{
        .token = try self.expectToken(.IfExpression),
        .condition = undefined,
        .consequence = undefined,
        .alternative = null,
    };

    _ = try self.expectToken(.LeftParen);

    if_expression.condition = try self.parseExpression(.Lowest);

    _ = try self.expectToken(.RightParen);

    if_expression.consequence = try self.parseBlockStatement();

    if (self.consumeToken(.Else)) |_| {
        if_expression.alternative = try self.parseBlockStatement();
    }

    return .{ .IfExpression = if_expression };
}

fn parseFunctionLiteral(self: *Parser) !Node {
    var parameters = std.ArrayList(*Ast.Identifier).init(self.allocator);
    defer parameters.deinit();

    const token = try self.expectToken(.FunctionLiteral);
    _ = try self.expectToken(.LeftParen);

    while (!self.currentTokenIs(.RightParen)) {
        const tok = self.expectToken(.Identifier);

        var identifier = try self.allocator.create(Ast.Identifier);
        identifier.* = Ast.Identifier{
            .token = tok,
            .value = tok.literal,
        };

        try parameters.append(identifier);

        _ = self.consumeToken(.Comma);
    }

    _ = try self.expectToken(.RightParen);

    var function_literal = try self.allocator.create(Ast.FunctionLiteral);
    function_literal.* = .{
        .token = token,
        .body = try self.parseBlockStatement(),
        .parameters = parameters.toOwnedSlice(),
    };

    return .{ .FunctionLiteral = function_literal };
}

fn parseCallExpression(self: *Parser, function: Node) !Node {
    var arguments = std.ArrayList(Node).init(self.allocator);
    defer arguments.deinit();

    const token = self.expectToken(.LeftParen);

    while (!self.currentTokenIs(.RightParen)) {
        try arguments.append(try self.parseExpression(.Lowest));
        _ = self.currentTokenIs(.Comma);
    }

    _ = try self.expectToken(.RightParen);

    var call_expression = try self.allocator.create(Ast.CallExpression);
    call_expression.* = .{
        .token = token,
        .function = function,
        .arguments = try arguments.toOwnedSlice(),
    };

    return .{ .CallExpression = call_expression };
}

fn parseBlockStatement(self: *Parser) !*Ast.BlockStatement {
    var statements = std.ArrayList(Node).init(self.allocator);
    defer statements.deinit();

    const token = self.expectToken(.LeftBrace);

    while (!self.currentTokenIs(.RightBrace) and !self.currentTokenIs(.Eof)) {
        if (try self.parseStatement()) |statement| {
            try statements.append(statement);
        }
        _ = self.nextToken();
    }

    var block_statement = try self.allocator.create(Ast.BlockStatement);
    block_statement.* = .{
        .token = token,
        .statements = try statements.toOwnedSlice(),
    };

    return block_statement;
}

fn parseExpressionStatement(self: *Parser) !Node {
    var expr_statement = try self.allocator.create(Ast.ExpressionStatement);
    expr_statement.* = Ast.ExpressionStatement{
        .token = self.tokens[self.tok_i],
        .expression = try self.parseExpression(.Lowest),
    };

    return .{ .ExpressionStatement = expr_statement };
}

fn parseLetStatement(self: *Parser) !Node {
    var let_statement = try self.allocator.create(Ast.LetStatement);
    let_statement.* = Ast.LetStatement{
        .token = self.nextToken(),
    };

    const token = self.nextToken();

    let_statement.name = try self.allocator.create(Ast.Identifier);
    let_statement.name.* = Ast.Identifier{
        .token = token,
        .value = token.literal,
    };

    _ = self.expectToken(.Equals);

    let_statement.value = try self.parseExpression(.Lowest);

    return .{ .LetStatement = let_statement };
}

fn parseReturnStatement(self: *Parser) !Node {
    var return_statement = try self.allocator.create(Ast.ReturnStatement);
    return_statement.* = Ast.ReturnStatement{ .token = self.current_token };

    self.nextToken();

    return_statement.return_value = try self.parseExpression(.Lowest);

    return .{ .ReturnStatement = return_statement };
}

fn parseIdentifier(self: *Parser) !Node {
    var identifier = try self.allocator.create(Ast.Identifier);
    identifier.* = Ast.Identifier{
        .token = self.current_token,
        .value = self.current_token.literal,
    };

    return .{ .Identifier = identifier };
}

fn parseInteger(self: *Parser) !Node {
    var integer = try self.allocator.create(Ast.Integer);
    integer.* = Ast.Integer{
        .token = self.current_token,
        .value = try std.fmt.parseInt(i64, self.current_token.literal, 10),
    };

    return .{ .Integer = integer };
}

fn parseBoolean(self: *Parser) !Node {
    var boolean = try self.allocator.create(Ast.Boolean);
    boolean.* = Ast.Boolean{
        .token = self.current_token,
        .value = std.mem.eql(u8, self.current_token.literal, "true"),
    };

    return .{ .Boolean = boolean };
}

fn parsePrefixExpression(self: *Parser) !Node {
    var prefix_expression = try self.allocator.create(Ast.PrefixExpression);
    prefix_expression.* = Ast.PrefixExpression{
        .token = self.current_token,
        .operator = self.current_token.literal,
    };

    self.nextToken();

    prefix_expression.operand = try self.parseExpression(.Prefix);

    return .{ .PrefixExpression = prefix_expression };
}

fn parseInfixExpression(self: *Parser, left_operand: Node) !Node {
    const token = self.nextToken();

    var infix_expression = try self.allocator.create(Ast.InfixExpression);
    infix_expression.* = Ast.InfixExpression{
        .token = token,
        .operator = token.literal,
        .left_operand = left_operand,
    };

    // Decrement precedence param for right-associativity
    infix_expression.right_operand = try self.parseExpression(self.tokenPrecedence(token));

    return .{ .InfixExpression = infix_expression };
}

fn nextToken(self: *Parser) Token {
    std.debug.assert(self.tok_i < self.tokens.len);
    const token = self.tokens[self.tok_i];
    self.tok_i += 1;
    return token;
}

fn expectToken(self: *Parser, token_type: Token.Type) !Token {
    if (!self.currentTokenIs(token_type)) {
        try self.errors.append(try std.fmt.allocPrint(self.allocator, "Expected token '{s}' but got token '{s}' instead", .{
            @tagName(token_type),
            @tagName(self.tokens[self.tok_i].type),
        }));
    }
    return self.nextToken();
}

fn consumeToken(self: *Parser, token_type: Token.Type) ?Token {
    if (self.currentTokenIs(token_type)) {
        return self.nextToken();
    }
    return null;
}

fn currentTokenIs(self: *Parser, token_type: Token.Type) bool {
    std.debug.assert(self.tok_i < self.tokens.len);
    return self.tokens[self.tok_i].type == token_type;
}

// fn peekTokenIs(self: *Parser, token_type: Token.Type) bool {
//     std.debug.assert(self.tok_i + 1 < self.tokens.len);
//     return self.tokens[self.tok_i + 1].type == token_type;
// }

fn tokenPrecedence(token_type: Token.Type) Precedence {
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

test "Let Statement" {
    const input =
        \\let foo = 10;
        \\let bar = true;
        \\let baz = foo;
    ;

    const expected = [_]Ast.Node{
        .{
            .LetStatement = &.{
                .token = .LetStatement,
                .name = "foo",
                .value = .{ .Integer = &.{ .token = .Integer, .value = 10 } },
            },
        },
        .{
            .LetStatement = &.{
                .token = .LetStatement,
                .name = "bar",
                .value = .{ .Boolean = &.{ .token = .Boolean, .value = true } },
            },
        },
        .{
            .LetStatement = &.{
                .token = .LetStatement,
                .name = "baz",
                .value = .{ .Integer = &.{ .token = .Integer, .value = 5 } },
            },
        },
    };

    var allocator = std.testing.allocator;
    var ast = try Ast.parse(allocator, input);
    defer ast.deinit(allocator);

    // var buffer: [input.len * 2]u8 = undefined;
    // var stream = std.io.fixedBufferStream(&buffer);
    // program.write(stream.writer());
    // std.debug.print("Let Statements:\n{s}\n", .{buffer});

    try expectEqualAst(&expected, &ast);
}

fn expectEqualAst(expected_nodes: []const Node, ast: *Ast) !void {
    if (ast.errors.items.len > 0) {
        std.debug.print("Parse failed with {d} errors:\n", .{ast.errors.items.len});
        for (ast.errors.items) |message| {
            std.debug.print("- {s}\n", .{message});
        }
        try std.testing.expect(false);
    }

    try expectEqualNodes(expected_nodes, ast.node);
}

fn expectEqualNodes(expected_nodes: []const Node, actual_nodes: []const Node) !void {
    try std.testing.expectEqual(expected_nodes.len, actual_nodes.len);

    for (expected_nodes, actual_nodes) |expected, actual| {
        try expectEqualNode(expected, actual);
    }
}

fn expectEqualNode(expected: Node, actual: Node) !void {
    try std.testing.expectEqual(expected, actual);
    switch (expected) {
        // Statements
        .LetStatement => |node| try expectEqualLetStatement(node, actual.LetStatement),
        .ReturnStatement => |node| try expectEqualReturnStatement(node, actual.ReturnStatement),
        .ExpressionStatement => |node| try expectEqualExpressionStatement(node, actual.ExpressionStatement),

        .BlockStatement => |node| try expectEqualBlockStatement(node, actual.BlockStatement),

        // Expressions
        .Identifier => |node| try expectEqualIdentifier(node, actual.Identifier),
        .Integer => |node| try expectEqualInteger(node, actual.Integer),
        .Boolean => |node| try expectEqualBoolean(node, actual.Boolean),

        .PrefixExpression => |node| try expectEqualPrefixExpression(node, actual.PrefixExpression),
        .InfixExpression => |node| try expectEqualInfixExpression(node, actual.InfixExpression),
        .GroupedExpression => |node| try expectEqualGroupedExpression(node, actual.GroupedExpression),
        .IfExpression => |node| try expectEqualIfExpression(node, actual.IfExpression),
        .FunctionLiteral => |node| try expectEqualFunctionLiteral(node, actual.FunctionLiteral),
        .CallExpression => |node| try expectEqualCallExpression(node, actual.CallExpression),
    }
}

fn expectEqualLetStatement(expected: *Ast.LetStatement, actual: *Ast.LetStatement) !void {
    try std.testing.expectEqual(expected.token, actual.token);
}

fn expectEqualReturnStatement(expected: *Ast.ReturnStatement, actual: *Ast.ReturnStatement) !void {
    try std.testing.expectEqual(expected.token, actual.token);
}

fn expectEqualExpressionStatement(expected: *Ast.ExpressionStatement, actual: *Ast.ExpressionStatement) !void {
    try std.testing.expectEqual(expected.token, actual.token);
}

fn expectEqualBlockStatement(expected: *Ast.BlockStatement, actual: *Ast.BlockStatement) !void {
    try std.testing.expectEqual(expected.token, actual.token);
}

fn expectEqualIdentifier(expected: *Ast.Identifier, actual: *Ast.Identifier) !void {
    try std.testing.expectEqual(expected.token, actual.token);
}

fn expectEqualInteger(expected: *Ast.Integer, actual: *Ast.Integer) !void {
    try std.testing.expectEqual(expected.token, actual.token);
}

fn expectEqualBoolean(expected: *Ast.Boolean, actual: *Ast.Boolean) !void {
    try std.testing.expectEqual(expected.token, actual.token);
}

fn expectEqualPrefixExpression(expected: *Ast.PrefixExpression, actual: *Ast.PrefixExpression) !void {
    try std.testing.expectEqual(expected.token, actual.token);
}

fn expectEqualInfixExpression(expected: *Ast.InfixExpression, actual: *Ast.InfixExpression) !void {
    try std.testing.expectEqual(expected.token, actual.token);
}

fn expectEqualGroupedExpression(expected: *Ast.GroupedExpression, actual: *Ast.GroupedExpression) !void {
    try std.testing.expectEqual(expected.token, actual.token);
}

fn expectEqualIfExpression(expected: *Ast.IfExpression, actual: *Ast.IfExpression) !void {
    try std.testing.expectEqual(expected.token, actual.token);
}

fn expectEqualFunctionLiteral(expected: *Ast.FunctionLiteral, actual: *Ast.FunctionLiteral) !void {
    try std.testing.expectEqual(expected.token, actual.token);
}

fn expectEqualCallExpression(expected: *Ast.CallExpression, actual: *Ast.CallExpression) !void {
    try std.testing.expectEqual(expected.token, actual.token);
}

// test "Return Statement" {
//     const input =
//         \\return 5;
//         \\return 10;
//         \\return 993322;
//     ;
//
//     var lexer = Lexer.init(input);
//
//     // TODO: Memory management...
//     var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//     defer arena.deinit();
//
//     var allocator = arena.allocator();
//
//     var parser = try Parser.init(&lexer, allocator);
//     defer parser.deinit();
//
//     var program = try parser.parseProgram(allocator);
//     defer program.deinit();
//
//     // var buffer: [input.len * 2]u8 = undefined;
//     // var stream = std.io.fixedBufferStream(&buffer);
//     // program.write(stream.writer());
//     // std.debug.print("Return Statements:\n{s}\n", .{buffer});
//
//     if (parser.errors.items.len > 0) {
//         std.debug.print("Parser failed with {d} errors:\n", .{parser.errors.items.len});
//         for (parser.errors.items) |message| {
//             std.debug.print("- {s}\n", .{message});
//         }
//         try std.testing.expect(false);
//     }
//
//     try std.testing.expectEqual(program.statements.items.len, 3);
//
//     const Expected = struct { name: []const u8, literal: []const u8, value: i64 };
//     const expected_values = [_]Expected{
//         .{ .name = "return", .literal = "5", .value = 5 },
//         .{ .name = "return", .literal = "10", .value = 10 },
//         .{ .name = "return", .literal = "993322", .value = 993322 },
//     };
//
//     for (expected_values, program.statements.items) |expected, statement| {
//         try std.testing.expectEqualStrings(expected.name, statement.tokenLiteral());
//
//         switch (statement.*) {
//             .ReturnStatement => |return_statement| {
//                 try std.testing.expectEqualStrings(expected.name, return_statement.tokenLiteral());
//                 switch (return_statement.return_value.*) {
//                     .Integer => |integer| {
//                         try std.testing.expectEqual(expected.value, integer.value);
//                         try std.testing.expectEqualStrings(expected.literal, integer.tokenLiteral());
//                     },
//                     else => unreachable,
//                 }
//             },
//             else => unreachable,
//         }
//     }
// }
//
// test "Identifier/Literal Expression" {
//     const input =
//         \\foobar;
//         \\5;
//         \\true;
//         \\false;
//     ;
//
//     var lexer = Lexer.init(input);
//
//     // TODO: Memory management...
//     var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//     defer arena.deinit();
//
//     var allocator = arena.allocator();
//
//     var parser = try Parser.init(&lexer, allocator);
//     defer parser.deinit();
//
//     var program = try parser.parseProgram(allocator);
//     defer program.deinit();
//
//     // var buffer: [input.len * 2]u8 = undefined;
//     // var stream = std.io.fixedBufferStream(&buffer);
//     // program.write(stream.writer());
//     // std.debug.print("Identifier/Literal Expressions:\n{s}\n", .{buffer});
//
//     if (parser.errors.items.len > 0) {
//         std.debug.print("Parser failed with {d} errors:\n", .{parser.errors.items.len});
//         for (parser.errors.items) |message| {
//             std.debug.print("- {s}\n", .{message});
//         }
//         try std.testing.expect(false);
//     }
//
//     try std.testing.expectEqual(program.statements.items.len, 4);
//
//     const Expected = struct { literal: []const u8, value: union { string: []const u8, int: i64, boolean: bool } };
//     const expected_values = [_]Expected{
//         .{ .literal = "foobar", .value = .{ .string = "foobar" } },
//         .{ .literal = "5", .value = .{ .int = 5 } },
//         .{ .literal = "true", .value = .{ .boolean = true } },
//         .{ .literal = "false", .value = .{ .boolean = false } },
//     };
//
//     for (expected_values, program.statements.items) |expected, statement| {
//         try std.testing.expectEqualStrings(expected.literal, statement.tokenLiteral());
//
//         switch (statement.*) {
//             .ExpressionStatement => |expr_statement| {
//                 switch (expr_statement.expression.*) {
//                     .Identifier => |identifier| {
//                         try std.testing.expectEqualStrings(expected.value.string, identifier.value);
//                         try std.testing.expectEqualStrings(expected.literal, identifier.tokenLiteral());
//                     },
//                     .Integer => |integer| {
//                         try std.testing.expectEqual(expected.value.int, integer.value);
//                         try std.testing.expectEqualStrings(expected.literal, integer.tokenLiteral());
//                     },
//                     .Boolean => |boolean| {
//                         try std.testing.expectEqual(expected.value.boolean, boolean.value);
//                         try std.testing.expectEqualStrings(expected.literal, boolean.tokenLiteral());
//                     },
//                     else => unreachable,
//                 }
//             },
//             else => unreachable,
//         }
//     }
// }
//
// test "Prefix Operators" {
//     const input =
//         \\!5;
//         \\-15;
//     ;
//
//     var lexer = Lexer.init(input);
//
//     // TODO: Memory management...
//     var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//     defer arena.deinit();
//
//     var allocator = arena.allocator();
//
//     var parser = try Parser.init(&lexer, allocator);
//     defer parser.deinit();
//
//     var program = try parser.parseProgram(allocator);
//     defer program.deinit();
//
//     // var buffer: [input.len * 2]u8 = undefined;
//     // var stream = std.io.fixedBufferStream(&buffer);
//     // program.write(stream.writer());
//     // std.debug.print("Prefix Operators:\n{s}\n", .{buffer});
//
//     if (parser.errors.items.len > 0) {
//         std.debug.print("Parser failed with {d} errors:\n", .{parser.errors.items.len});
//         for (parser.errors.items) |message| {
//             std.debug.print("- {s}\n", .{message});
//         }
//         try std.testing.expect(false);
//     }
//
//     try std.testing.expectEqual(program.statements.items.len, 2);
//
//     const Expected = struct { prefix: []const u8, literal: []const u8, value: i64 };
//     const expected_values = [_]Expected{
//         .{ .prefix = "!", .literal = "5", .value = 5 },
//         .{ .prefix = "-", .literal = "15", .value = 15 },
//     };
//
//     for (expected_values, program.statements.items) |expected, statement| {
//         try std.testing.expectEqualStrings(expected.prefix, statement.tokenLiteral());
//
//         switch (statement.*) {
//             .ExpressionStatement => |expr_statement| {
//                 switch (expr_statement.expression.*) {
//                     .PrefixExpression => |prefix_expression| {
//                         try std.testing.expectEqualStrings(expected.prefix, prefix_expression.operator);
//                         try std.testing.expectEqualStrings(expected.prefix, prefix_expression.tokenLiteral());
//                         switch (prefix_expression.operand.*) {
//                             .Integer => |integer| {
//                                 try std.testing.expectEqual(expected.value, integer.value);
//                                 try std.testing.expectEqualStrings(expected.literal, integer.tokenLiteral());
//                             },
//                             else => unreachable,
//                         }
//                     },
//                     else => unreachable,
//                 }
//             },
//             else => unreachable,
//         }
//     }
// }
//
// test "Infix Operators" {
//     const input =
//         \\5 + 5;
//         \\5 - 5;
//         \\5 * 5;
//         \\5 / 5;
//         \\5 > 5;
//         \\5 < 5;
//         \\5 == 5;
//         \\5 != 5;
//         \\true == true;
//         \\true != false;
//         \\false == false;
//     ;
//
//     var lexer = Lexer.init(input);
//
//     // TODO: Memory management...
//     var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//     defer arena.deinit();
//
//     var allocator = arena.allocator();
//
//     var parser = try Parser.init(&lexer, allocator);
//     defer parser.deinit();
//
//     var program = try parser.parseProgram(allocator);
//     defer program.deinit();
//
//     // var buffer: [input.len * 2]u8 = undefined;
//     // var stream = std.io.fixedBufferStream(&buffer);
//     // program.write(stream.writer());
//     // std.debug.print("Infix Operators:\n{s}\n", .{buffer});
//
//     if (parser.errors.items.len > 0) {
//         std.debug.print("Parser failed with {d} errors:\n", .{parser.errors.items.len});
//         for (parser.errors.items) |message| {
//             std.debug.print("- {s}\n", .{message});
//         }
//         try std.testing.expect(false);
//     }
//
//     try std.testing.expectEqual(program.statements.items.len, 11);
//
//     const Value = struct { literal: []const u8, value: union { int: i64, boolean: bool } };
//     const Expected = struct { lhs: Value, operator: []const u8, rhs: Value };
//     const expected_values = [_]Expected{
//         .{ .lhs = .{ .literal = "5", .value = .{ .int = 5 } }, .operator = "+", .rhs = .{ .literal = "5", .value = .{ .int = 5 } } },
//         .{ .lhs = .{ .literal = "5", .value = .{ .int = 5 } }, .operator = "-", .rhs = .{ .literal = "5", .value = .{ .int = 5 } } },
//         .{ .lhs = .{ .literal = "5", .value = .{ .int = 5 } }, .operator = "*", .rhs = .{ .literal = "5", .value = .{ .int = 5 } } },
//         .{ .lhs = .{ .literal = "5", .value = .{ .int = 5 } }, .operator = "/", .rhs = .{ .literal = "5", .value = .{ .int = 5 } } },
//         .{ .lhs = .{ .literal = "5", .value = .{ .int = 5 } }, .operator = ">", .rhs = .{ .literal = "5", .value = .{ .int = 5 } } },
//         .{ .lhs = .{ .literal = "5", .value = .{ .int = 5 } }, .operator = "<", .rhs = .{ .literal = "5", .value = .{ .int = 5 } } },
//         .{ .lhs = .{ .literal = "5", .value = .{ .int = 5 } }, .operator = "==", .rhs = .{ .literal = "5", .value = .{ .int = 5 } } },
//         .{ .lhs = .{ .literal = "5", .value = .{ .int = 5 } }, .operator = "!=", .rhs = .{ .literal = "5", .value = .{ .int = 5 } } },
//         .{ .lhs = .{ .literal = "true", .value = .{ .boolean = true } }, .operator = "==", .rhs = .{ .literal = "true", .value = .{ .boolean = true } } },
//         .{ .lhs = .{ .literal = "true", .value = .{ .boolean = true } }, .operator = "!=", .rhs = .{ .literal = "false", .value = .{ .boolean = false } } },
//         .{ .lhs = .{ .literal = "false", .value = .{ .boolean = false } }, .operator = "==", .rhs = .{ .literal = "false", .value = .{ .boolean = false } } },
//     };
//
//     for (expected_values, program.statements.items) |expected, statement| {
//         try std.testing.expectEqualStrings(expected.lhs.literal, statement.tokenLiteral());
//
//         switch (statement.*) {
//             .ExpressionStatement => |expr_statement| {
//                 switch (expr_statement.expression.*) {
//                     .InfixExpression => |infix_expression| {
//                         try std.testing.expectEqualStrings(expected.operator, infix_expression.operator);
//                         try std.testing.expectEqualStrings(expected.operator, infix_expression.tokenLiteral());
//                         switch (infix_expression.left_operand.*) {
//                             .Integer => |integer| {
//                                 try std.testing.expectEqual(expected.lhs.value.int, integer.value);
//                                 try std.testing.expectEqualStrings(expected.lhs.literal, integer.tokenLiteral());
//                             },
//                             .Boolean => |boolean| {
//                                 try std.testing.expectEqual(expected.lhs.value.boolean, boolean.value);
//                                 try std.testing.expectEqualStrings(expected.lhs.literal, boolean.tokenLiteral());
//                             },
//                             else => unreachable,
//                         }
//                         switch (infix_expression.right_operand.*) {
//                             .Integer => |integer| {
//                                 try std.testing.expectEqual(expected.rhs.value.int, integer.value);
//                                 try std.testing.expectEqualStrings(expected.rhs.literal, integer.tokenLiteral());
//                             },
//                             .Boolean => |boolean| {
//                                 try std.testing.expectEqual(expected.rhs.value.boolean, boolean.value);
//                                 try std.testing.expectEqualStrings(expected.rhs.literal, boolean.tokenLiteral());
//                             },
//                             else => unreachable,
//                         }
//                     },
//                     else => unreachable,
//                 }
//             },
//             else => unreachable,
//         }
//     }
// }
//
// test "Operator Precedence" {
//     const Test = struct { input: []const u8, expected: []const u8 };
//     var tests = [_]Test{
//         .{ .input = "-a * b;\n", .expected = "((-a) * b);\n" },
//         .{ .input = "!-a;\n", .expected = "(!(-a));\n" },
//         .{ .input = "a + b + c;\n", .expected = "((a + b) + c);\n" },
//         .{ .input = "a + b - c;\n", .expected = "((a + b) - c);\n" },
//         .{ .input = "a * b * c;\n", .expected = "((a * b) * c);\n" },
//         .{ .input = "a * b / c;\n", .expected = "((a * b) / c);\n" },
//         .{ .input = "a + b / c;\n", .expected = "(a + (b / c));\n" },
//         .{ .input = "a + b * c + d / e - f;\n", .expected = "(((a + (b * c)) + (d / e)) - f);\n" },
//         .{ .input = "3 + 4;\n-5 * 5;\n", .expected = "(3 + 4);\n((-5) * 5);\n" },
//         .{ .input = "5 > 4 == 3 < 4;\n", .expected = "((5 > 4) == (3 < 4));\n" },
//         .{ .input = "5 < 4 != 3 > 4;\n", .expected = "((5 < 4) != (3 > 4));\n" },
//         .{ .input = "3 + 4 * 5 == 3 * 1 + 4 * 5;\n", .expected = "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));\n" },
//         .{ .input = "true;\n", .expected = "true;\n" },
//         .{ .input = "false;\n", .expected = "false;\n" },
//         .{ .input = "3 > 5 == false;\n", .expected = "((3 > 5) == false);\n" },
//         .{ .input = "3 < 5 == true;\n", .expected = "((3 < 5) == true);\n" },
//         .{ .input = "1 + (2 + 3) + 4;\n", .expected = "((1 + (2 + 3)) + 4);\n" },
//         .{ .input = "(5 + 5) * 2;\n", .expected = "((5 + 5) * 2);\n" },
//         .{ .input = "-(5 + 5);\n", .expected = "(-(5 + 5));\n" },
//         .{ .input = "!(true == true);\n", .expected = "(!(true == true));\n" },
//         .{ .input = "a + add(b * c) + d", .expected = "((a + add((b * c))) + d);\n" },
//         .{ .input = "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", .expected = "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));\n" },
//         .{ .input = "add(a + b + c * d / f + g)", .expected = "add((((a + b) + ((c * d) / f)) + g));\n" },
//     };
//
//     // TODO: Memory management...
//     var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//     defer arena.deinit();
//
//     var allocator = arena.allocator();
//
//     for (tests) |test_entry| {
//         var lexer = Lexer.init(test_entry.input);
//
//         var parser = try Parser.init(&lexer, allocator);
//         defer parser.deinit();
//
//         var program = try parser.parseProgram(allocator);
//         defer program.deinit();
//
//         var buffer = try allocator.alloc(u8, test_entry.expected.len);
//         defer allocator.free(buffer);
//
//         var buf_writer = std.io.fixedBufferStream(buffer);
//
//         program.write(buf_writer.writer());
//
//         try std.testing.expectEqualStrings(test_entry.expected, buffer);
//     }
// }
//
// test "If Expression" {
//     const input =
//         \\if (x < y) { x }
//         \\if (x < y) { x } else { y }
//     ;
//
//     var lexer = Lexer.init(input);
//
//     // TODO: Memory management...
//     var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//     defer arena.deinit();
//
//     var allocator = arena.allocator();
//
//     var parser = try Parser.init(&lexer, allocator);
//     defer parser.deinit();
//
//     var program = try parser.parseProgram(allocator);
//     defer program.deinit();
//
//     // var buffer: [input.len * 2]u8 = undefined;
//     // var stream = std.io.fixedBufferStream(&buffer);
//     // program.write(stream.writer());
//     // std.debug.print("If Expression:\n{s}\n", .{buffer});
//
//     if (parser.errors.items.len > 0) {
//         std.debug.print("Parser failed with {d} errors:\n", .{parser.errors.items.len});
//         for (parser.errors.items) |message| {
//             std.debug.print("- {s}\n", .{message});
//         }
//         try std.testing.expect(false);
//     }
//
//     try std.testing.expectEqual(@as(usize, 2), program.statements.items.len);
//
//     const Expected = struct {
//         token: []const u8,
//         condition: struct { lhs: []const u8, operator: []const u8, rhs: []const u8 },
//         consequence: []const u8,
//         alternative: ?[]const u8,
//     };
//
//     const expected_values = [_]Expected{
//         .{
//             .token = "if",
//             .condition = .{ .lhs = "x", .operator = "<", .rhs = "y" },
//             .consequence = "x",
//             .alternative = null,
//         },
//         .{
//             .token = "if",
//             .condition = .{ .lhs = "x", .operator = "<", .rhs = "y" },
//             .consequence = "x",
//             .alternative = "y",
//         },
//     };
//
//     for (expected_values, program.statements.items) |expected, statement| {
//         try std.testing.expectEqualStrings(expected.token, statement.tokenLiteral());
//
//         switch (statement.*) {
//             .ExpressionStatement => |expr_statement| {
//                 switch (expr_statement.expression.*) {
//                     .IfExpression => |if_expression| {
//                         try std.testing.expectEqualStrings(expected.token, if_expression.tokenLiteral());
//                         switch (if_expression.condition.*) {
//                             .InfixExpression => |infix_expression| {
//                                 try std.testing.expectEqualStrings(expected.condition.operator, infix_expression.operator);
//                                 try std.testing.expectEqualStrings(expected.condition.operator, infix_expression.tokenLiteral());
//                                 switch (infix_expression.left_operand.*) {
//                                     .Identifier => |identifier| {
//                                         try std.testing.expectEqualStrings(expected.condition.lhs, identifier.value);
//                                         try std.testing.expectEqualStrings(expected.condition.lhs, identifier.tokenLiteral());
//                                     },
//                                     else => unreachable,
//                                 }
//                                 switch (infix_expression.right_operand.*) {
//                                     .Identifier => |identifier| {
//                                         try std.testing.expectEqualStrings(expected.condition.rhs, identifier.value);
//                                         try std.testing.expectEqualStrings(expected.condition.rhs, identifier.tokenLiteral());
//                                     },
//                                     else => unreachable,
//                                 }
//                             },
//                             else => unreachable,
//                         }
//                         try std.testing.expectEqual(@as(usize, 1), if_expression.consequence.statements.items.len);
//                         switch (if_expression.consequence.statements.items[0].*) {
//                             .ExpressionStatement => |consq_statement| {
//                                 switch (consq_statement.expression.*) {
//                                     .Identifier => |identifier| {
//                                         try std.testing.expectEqualStrings(expected.consequence, identifier.value);
//                                         try std.testing.expectEqualStrings(expected.consequence, identifier.tokenLiteral());
//                                     },
//                                     else => unreachable,
//                                 }
//                             },
//                             else => unreachable,
//                         }
//                         if (expected.alternative) |alternative| {
//                             try std.testing.expectEqual(@as(usize, 1), if_expression.alternative.?.statements.items.len);
//                             switch (if_expression.alternative.?.statements.items[0].*) {
//                                 .ExpressionStatement => |alt_statement| {
//                                     switch (alt_statement.expression.*) {
//                                         .Identifier => |identifier| {
//                                             try std.testing.expectEqualStrings(alternative, identifier.value);
//                                             try std.testing.expectEqualStrings(alternative, identifier.tokenLiteral());
//                                         },
//                                         else => unreachable,
//                                     }
//                                 },
//                                 else => unreachable,
//                             }
//                         } else {
//                             try std.testing.expectEqual(expected.alternative, null);
//                         }
//                     },
//                     else => unreachable,
//                 }
//             },
//             else => unreachable,
//         }
//     }
// }
//
// test "Function Literal" {
//     const input =
//         \\fn(x, y) { x + y; }
//     ;
//
//     var lexer = Lexer.init(input);
//
//     // TODO: Memory management...
//     var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//     defer arena.deinit();
//
//     var allocator = arena.allocator();
//
//     var parser = try Parser.init(&lexer, allocator);
//     defer parser.deinit();
//
//     var program = try parser.parseProgram(allocator);
//     defer program.deinit();
//
//     // var buffer: [input.len * 2]u8 = undefined;
//     // var stream = std.io.fixedBufferStream(&buffer);
//     // program.write(stream.writer());
//     // std.debug.print("Function Literal:\n{s}\n", .{buffer});
//
//     if (parser.errors.items.len > 0) {
//         std.debug.print("Parser failed with {d} errors:\n", .{parser.errors.items.len});
//         for (parser.errors.items) |message| {
//             std.debug.print("- {s}\n", .{message});
//         }
//         try std.testing.expectEqual(@as(usize, 0), parser.errors.items.len);
//     }
//
//     try std.testing.expectEqual(@as(usize, 1), program.statements.items.len);
//
//     const Expected = struct {
//         token: []const u8,
//         parameters: [2][]const u8,
//         body: struct { lhs: []const u8, operator: []const u8, rhs: []const u8 },
//     };
//
//     const expected_values = [_]Expected{
//         .{
//             .token = "fn",
//             .parameters = .{ "x", "y" },
//             .body = .{ .lhs = "x", .operator = "+", .rhs = "y" },
//         },
//     };
//
//     for (expected_values, program.statements.items) |expected, statement| {
//         try std.testing.expectEqualStrings(expected.token, statement.tokenLiteral());
//
//         switch (statement.*) {
//             .ExpressionStatement => |expr_statement| {
//                 switch (expr_statement.expression.*) {
//                     .FunctionLiteral => |function_literal| {
//                         try std.testing.expectEqualStrings(expected.token, function_literal.tokenLiteral());
//                         try std.testing.expectEqual(expected.parameters.len, function_literal.parameters.items.len);
//                         for (expected.parameters, function_literal.parameters.items) |expected_param, function_param| {
//                             try std.testing.expectEqualStrings(expected_param, function_param.value);
//                             try std.testing.expectEqualStrings(expected_param, function_param.tokenLiteral());
//                         }
//                         try std.testing.expectEqual(@as(usize, 1), function_literal.body.statements.items.len);
//                         switch (function_literal.body.statements.items[0].*) {
//                             .ExpressionStatement => |body_statement| {
//                                 switch (body_statement.expression.*) {
//                                     .InfixExpression => |infix_expression| {
//                                         try std.testing.expectEqualStrings(expected.body.operator, infix_expression.operator);
//                                         try std.testing.expectEqualStrings(expected.body.operator, infix_expression.tokenLiteral());
//                                         switch (infix_expression.left_operand.*) {
//                                             .Identifier => |identifier| {
//                                                 try std.testing.expectEqualStrings(expected.body.lhs, identifier.value);
//                                                 try std.testing.expectEqualStrings(expected.body.lhs, identifier.tokenLiteral());
//                                             },
//                                             else => unreachable,
//                                         }
//                                         switch (infix_expression.right_operand.*) {
//                                             .Identifier => |identifier| {
//                                                 try std.testing.expectEqualStrings(expected.body.rhs, identifier.value);
//                                                 try std.testing.expectEqualStrings(expected.body.rhs, identifier.tokenLiteral());
//                                             },
//                                             else => unreachable,
//                                         }
//                                     },
//                                     else => unreachable,
//                                 }
//                             },
//                             else => unreachable,
//                         }
//                     },
//                     else => unreachable,
//                 }
//             },
//             else => unreachable,
//         }
//     }
// }
//
// test "Call Expression" {
//     const input =
//         \\add(1, 2 * 3, 4 + 5)
//     ;
//
//     var lexer = Lexer.init(input);
//
//     // TODO: Memory management...
//     var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//     defer arena.deinit();
//
//     var allocator = arena.allocator();
//
//     var parser = try Parser.init(&lexer, allocator);
//     defer parser.deinit();
//
//     var program = try parser.parseProgram(allocator);
//     defer program.deinit();
//
//     // var buffer: [input.len * 2]u8 = undefined;
//     // var stream = std.io.fixedBufferStream(&buffer);
//     // program.write(stream.writer());
//     // std.debug.print("Call Expression:\n{s}\n", .{buffer});
//
//     if (parser.errors.items.len > 0) {
//         std.debug.print("Parser failed with {d} errors:\n", .{parser.errors.items.len});
//         for (parser.errors.items) |message| {
//             std.debug.print("- {s}\n", .{message});
//         }
//         try std.testing.expectEqual(@as(usize, 0), parser.errors.items.len);
//     }
//
//     try std.testing.expectEqual(@as(usize, 1), program.statements.items.len);
//
//     const Value = struct { literal: []const u8, value: i64 };
//     const ExpectedArgs = struct { lhs: Value, operator: []const u8, rhs: Value };
//     var args = [_]ExpectedArgs{
//         .{ .lhs = .{ .literal = "1", .value = 1 }, .operator = "", .rhs = .{ .literal = "", .value = undefined } },
//         .{ .lhs = .{ .literal = "2", .value = 2 }, .operator = "*", .rhs = .{ .literal = "3", .value = 3 } },
//         .{ .lhs = .{ .literal = "4", .value = 4 }, .operator = "+", .rhs = .{ .literal = "5", .value = 5 } },
//     };
//
//     const Expected = struct { token: []const u8, function: []const u8, arguments: []ExpectedArgs };
//     const expected_values = [_]Expected{
//         .{
//             .token = "add",
//             .function = "add",
//             .arguments = args[0..],
//         },
//     };
//
//     for (expected_values, program.statements.items) |expected, statement| {
//         try std.testing.expectEqualStrings(expected.token, statement.tokenLiteral());
//
//         switch (statement.*) {
//             .ExpressionStatement => |expr_statement| {
//                 switch (expr_statement.expression.*) {
//                     .CallExpression => |call_expression| {
//                         try std.testing.expectEqualStrings("(", call_expression.tokenLiteral());
//                         switch (call_expression.function.*) {
//                             .Identifier => |identifier| {
//                                 try std.testing.expectEqualStrings(expected.function, identifier.value);
//                                 try std.testing.expectEqualStrings(expected.function, identifier.tokenLiteral());
//                             },
//                             else => unreachable,
//                         }
//                         try std.testing.expectEqual(expected.arguments.len, call_expression.arguments.items.len);
//                         for (expected.arguments, call_expression.arguments.items) |expected_arg, call_arg| {
//                             switch (call_arg.*) {
//                                 .Integer => |integer| {
//                                     try std.testing.expectEqual(expected_arg.lhs.value, integer.value);
//                                     try std.testing.expectEqualStrings(expected_arg.lhs.literal, integer.tokenLiteral());
//                                 },
//                                 .InfixExpression => |infix_expression| {
//                                     try std.testing.expectEqualStrings(expected_arg.operator, infix_expression.operator);
//                                     try std.testing.expectEqualStrings(expected_arg.operator, infix_expression.tokenLiteral());
//                                     switch (infix_expression.left_operand.*) {
//                                         .Integer => |integer| {
//                                             try std.testing.expectEqual(expected_arg.lhs.value, integer.value);
//                                             try std.testing.expectEqualStrings(expected_arg.lhs.literal, integer.tokenLiteral());
//                                         },
//                                         else => unreachable,
//                                     }
//                                     switch (infix_expression.right_operand.*) {
//                                         .Integer => |integer| {
//                                             try std.testing.expectEqual(expected_arg.rhs.value, integer.value);
//                                             try std.testing.expectEqualStrings(expected_arg.rhs.literal, integer.tokenLiteral());
//                                         },
//                                         else => unreachable,
//                                     }
//                                 },
//                                 else => unreachable,
//                             }
//                         }
//                     },
//                     else => unreachable,
//                 }
//             },
//             else => unreachable,
//         }
//     }
// }
//
// test "Writing" {
//     var statement = Ast.Statement{
//         .LetStatement = @constCast(&Ast.LetStatement{
//             .token = Token{ .type = .Let, .literal = "let" },
//             .name = @constCast(&Ast.Identifier{
//                 .token = Token{ .type = .Identifier, .literal = "myVar" },
//                 .value = "myVar",
//             }),
//             .value = @constCast(&Expression{
//                 .Identifier = @constCast(&Ast.Identifier{
//                     .token = Token{ .type = .Identifier, .literal = "anotherVar" },
//                     .value = "anotherVar",
//                 }),
//             }),
//         }),
//     };
//
//     // TODO: Memory management...
//     var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//     defer arena.deinit();
//
//     var allocator = arena.allocator();
//
//     var program = try Program.init(allocator);
//     defer program.deinit();
//
//     try program.statements.append(&statement);
//
//     const input = "let myVar = anotherVar;\n";
//     var buffer: [input.len]u8 = undefined;
//     var buf_writer = std.io.fixedBufferStream(&buffer);
//
//     program.write(buf_writer.writer());
//
//     try std.testing.expectEqualStrings(input, &buffer);
// }
