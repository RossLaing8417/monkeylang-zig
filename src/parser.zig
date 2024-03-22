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
nodes: std.ArrayList(Node),
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

pub fn parseProgram(self: *Parser) Error!void {
    while (!self.currentTokenIs(.Eof)) {
        try self.nodes.append(try self.parseStatement());
    }
}

fn parseStatement(self: *Parser) Error!Node {
    const node = switch (self.tokens[self.tok_i].type) {
        .Let => try self.parseLetStatement(),
        .Return => try self.parseReturnStatement(),
        .If => try self.parseIfExpression(),
        else => try self.parseExpressionStatement(),
    };
    _ = self.consumeToken(.SemiColon);
    return node;
}

fn parseExpression(self: *Parser, precedence: Precedence) Error!Node {
    // std.debug.print("TOKEN EXPR: {s} ({s})\n", .{ @tagName(self.tokens[self.tok_i].type), self.tokens[self.tok_i].literal });

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

        // std.debug.print("TOKEN PREC: {s} -> {s} vs {s}\n", .{ @tagName(token.type), @tagName(precedence), @tagName(tokenPrecedence(token)) });

        if (self.currentTokenIs(.SemiColon) or @intFromEnum(precedence) >= @intFromEnum(tokenPrecedence(token))) {
            // std.debug.print("GTFO!\n", .{});
            break;
        }

        // I don't know what the right pattern is here if there is any...?
        // #skillissue
        switch (self.tokens[self.tok_i].type) {
            .Plus,
            .Minus,
            .Asterisk,
            .Slash,
            .Equal,
            .NotEqual,
            .LessThan,
            .GreaterThan,
            .LeftParen,
            => {},
            else => return left_operand,
        }

        left_operand = switch (self.tokens[self.tok_i].type) {
            .LeftParen => try self.parseCallExpression(left_operand),
            else => try self.parseInfixExpression(left_operand),
        };
    }

    return left_operand;
}

fn parseGroupedExpression(self: *Parser) Error!Node {
    var grouped_expression = try self.allocator.create(Ast.GroupedExpression);

    grouped_expression.* = .{
        .token = try self.expectToken(.LeftParen),
        .expression = try self.parseExpression(.Lowest),
    };

    _ = try self.expectToken(.RightParen);

    return .{ .GroupedExpression = grouped_expression };
}

fn parseIfExpression(self: *Parser) Error!Node {
    var if_expression = try self.allocator.create(Ast.IfExpression);
    if_expression.* = Ast.IfExpression{
        .token = try self.expectToken(.If),
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

fn parseFunctionLiteral(self: *Parser) Error!Node {
    var parameters = std.ArrayList(Node).init(self.allocator);
    defer parameters.deinit();

    const token = try self.expectToken(.Function);
    _ = try self.expectToken(.LeftParen);

    while (!self.currentTokenIs(.RightParen)) {
        const tok = try self.expectToken(.Identifier);

        var identifier = try self.allocator.create(Ast.Identifier);
        identifier.* = Ast.Identifier{
            .token = tok,
            .value = tok.literal,
        };

        try parameters.append(.{ .Identifier = identifier });

        _ = self.consumeToken(.Comma);
    }

    _ = try self.expectToken(.RightParen);

    var function_literal = try self.allocator.create(Ast.FunctionLiteral);
    function_literal.* = .{
        .token = token,
        .body = try self.parseBlockStatement(),
        .parameters = try parameters.toOwnedSlice(),
    };

    return .{ .FunctionLiteral = function_literal };
}

fn parseCallExpression(self: *Parser, function: Node) Error!Node {
    var arguments = std.ArrayList(Node).init(self.allocator);
    defer arguments.deinit();

    const token = try self.expectToken(.LeftParen);

    while (!self.currentTokenIs(.RightParen)) {
        try arguments.append(try self.parseExpression(.Lowest));
        _ = self.consumeToken(.Comma);
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

fn parseBlockStatement(self: *Parser) Error!Node {
    var statements = std.ArrayList(Node).init(self.allocator);
    defer statements.deinit();

    const token = try self.expectToken(.LeftBrace);

    while (!self.currentTokenIs(.RightBrace) and !self.currentTokenIs(.Eof)) {
        try statements.append(try self.parseStatement());
    }

    _ = try self.expectToken(.RightBrace);

    var block_statement = try self.allocator.create(Ast.BlockStatement);
    block_statement.* = .{
        .token = token,
        .statements = try statements.toOwnedSlice(),
    };

    return .{ .BlockStatement = block_statement };
}

fn parseLetStatement(self: *Parser) Error!Node {
    var let_statement = try self.allocator.create(Ast.LetStatement);
    let_statement.* = Ast.LetStatement{
        .token = self.nextToken(),
        .name = try self.parseIdentifier(),
    };

    _ = try self.expectToken(.Assign);

    let_statement.value = try self.parseExpression(.Lowest);

    return .{ .LetStatement = let_statement };
}

fn parseReturnStatement(self: *Parser) Error!Node {
    var return_statement = try self.allocator.create(Ast.ReturnStatement);
    return_statement.* = .{
        .token = try self.expectToken(.Return),
        .return_value = try self.parseExpression(.Lowest),
    };

    return .{ .ReturnStatement = return_statement };
}

fn parseExpressionStatement(self: *Parser) Error!Node {
    var expr_statement = try self.allocator.create(Ast.ExpressionStatement);
    expr_statement.* = Ast.ExpressionStatement{
        .token = self.tokens[self.tok_i],
        .expression = try self.parseExpression(.Lowest),
    };

    return .{ .ExpressionStatement = expr_statement };
}

fn parseIdentifier(self: *Parser) Error!Node {
    const token = try self.expectToken(.Identifier);

    var identifier = try self.allocator.create(Ast.Identifier);
    identifier.* = Ast.Identifier{
        .token = token,
        .value = token.literal,
    };

    return .{ .Identifier = identifier };
}

fn parseInteger(self: *Parser) Error!Node {
    const token = try self.expectToken(.Integer);

    var integer = try self.allocator.create(Ast.Integer);
    integer.* = Ast.Integer{
        .token = token,
        .value = try std.fmt.parseInt(i64, token.literal, 10),
    };

    return .{ .Integer = integer };
}

fn parseBoolean(self: *Parser) Error!Node {
    const token = self.consumeToken(.True) orelse
        try self.expectToken(.False);

    var boolean = try self.allocator.create(Ast.Boolean);
    boolean.* = Ast.Boolean{
        .token = token,
        .value = (token.type == .True),
    };

    return .{ .Boolean = boolean };
}

fn parsePrefixExpression(self: *Parser) Error!Node {
    const token = self.nextToken();

    var prefix_expression = try self.allocator.create(Ast.PrefixExpression);
    prefix_expression.* = Ast.PrefixExpression{
        .token = token,
        .operator = token.literal,
        .operand = try self.parseExpression(.Prefix),
    };

    return .{ .PrefixExpression = prefix_expression };
}

fn parseInfixExpression(self: *Parser, left_operand: Node) Error!Node {
    const token = self.nextToken();

    var infix_expression = try self.allocator.create(Ast.InfixExpression);
    infix_expression.* = Ast.InfixExpression{
        .token = token,
        .operator = token.literal,
        .left_operand = left_operand,
        // Decrement precedence param for right-associativity
        .right_operand = try self.parseExpression(tokenPrecedence(token)),
    };

    return .{ .InfixExpression = infix_expression };
}

fn nextToken(self: *Parser) Token {
    std.debug.assert(self.tok_i < self.tokens.len);
    const token = self.tokens[self.tok_i];
    self.tok_i += 1;
    return token;
}

fn expectToken(self: *Parser, token_type: Token.Type) Error!Token {
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

fn tokenPrecedence(token: Token) Precedence {
    return switch (token.type) {
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

    const expected = [_]Node{
        .{
            .LetStatement = &.{
                .token = .{ .type = .Let, .literal = "let" },
                .name = .{ .Identifier = &.{ .token = .{ .type = .Identifier, .literal = "foo" }, .value = "foo" } },
                .value = .{ .Integer = &.{ .token = .{ .type = .Integer, .literal = "10" }, .value = 10 } },
            },
        },
        .{
            .LetStatement = &.{
                .token = .{ .type = .Let, .literal = "let" },
                .name = .{ .Identifier = &.{ .token = .{ .type = .Identifier, .literal = "bar" }, .value = "bar" } },
                .value = .{ .Boolean = &.{ .token = .{ .type = .True, .literal = "true" }, .value = true } },
            },
        },
        .{
            .LetStatement = &.{
                .token = .{ .type = .Let, .literal = "let" },
                .name = .{ .Identifier = &.{ .token = .{ .type = .Identifier, .literal = "baz" }, .value = "baz" } },
                .value = .{ .Identifier = &.{ .token = .{ .type = .Identifier, .literal = "foo" }, .value = "foo" } },
            },
        },
    };

    var allocator = std.testing.allocator;
    var ast = try Ast.parse(allocator, input);
    defer ast.deinit(allocator);

    // var buffer = try ast.write(allocator);
    // defer allocator.free(buffer);
    // std.debug.print("Let Statements:\n{s}\n", .{buffer});

    try expectEqualAst(&expected, &ast);
}

test "Return Statement" {
    const input =
        \\return 5;
        \\return false;
        \\return foobar;
    ;

    const expected = [_]Node{
        .{
            .ReturnStatement = &.{
                .token = .{ .type = .Return, .literal = "return" },
                .return_value = .{ .Integer = &.{ .token = .{ .type = .Integer, .literal = "5" }, .value = 5 } },
            },
        },
        .{
            .ReturnStatement = &.{
                .token = .{ .type = .Return, .literal = "return" },
                .return_value = .{ .Boolean = &.{ .token = .{ .type = .False, .literal = "false" }, .value = false } },
            },
        },
        .{
            .ReturnStatement = &.{
                .token = .{ .type = .Return, .literal = "return" },
                .return_value = .{ .Identifier = &.{ .token = .{ .type = .Identifier, .literal = "foobar" }, .value = "foobar" } },
            },
        },
    };

    var allocator = std.testing.allocator;
    var ast = try Ast.parse(allocator, input);
    defer ast.deinit(allocator);

    // var buffer = try ast.write(allocator);
    // defer allocator.free(buffer);
    // std.debug.print("Return Statements:\n{s}\n", .{buffer});

    try expectEqualAst(&expected, &ast);
}

test "Prefix Operators" {
    const input =
        \\!5;
        \\-15;
    ;

    const expected = [_]Node{
        .{
            .ExpressionStatement = &.{
                .token = .{ .type = .Bang, .literal = "!" },
                .expression = .{ .PrefixExpression = &.{
                    .token = .{ .type = .Bang, .literal = "!" },
                    .operator = "!",
                    .operand = .{ .Integer = &.{ .token = .{ .type = .Integer, .literal = "5" }, .value = 5 } },
                } },
            },
        },
        .{
            .ExpressionStatement = &.{
                .token = .{ .type = .Minus, .literal = "-" },
                .expression = .{ .PrefixExpression = &.{
                    .token = .{ .type = .Minus, .literal = "-" },
                    .operator = "-",
                    .operand = .{ .Integer = &.{ .token = .{ .type = .Integer, .literal = "15" }, .value = 15 } },
                } },
            },
        },
    };

    var allocator = std.testing.allocator;
    var ast = try Ast.parse(allocator, input);
    defer ast.deinit(allocator);

    // var buffer = try ast.write(allocator);
    // defer allocator.free(buffer);
    // std.debug.print("Prefix Operators:\n{s}\n", .{buffer});

    try expectEqualAst(&expected, &ast);
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

    const integer = Node{ .Integer = &.{ .token = .{ .type = .Integer, .literal = "5" }, .value = 5 } };
    const bool_true = Node{ .Boolean = &.{ .token = .{ .type = .True, .literal = "true" }, .value = true } };
    const bool_false = Node{ .Boolean = &.{ .token = .{ .type = .False, .literal = "false" }, .value = false } };
    const expected = [_]Node{
        .{
            .ExpressionStatement = &.{
                .token = integer.token(),
                .expression = .{ .InfixExpression = &.{ .token = .{ .type = .Plus, .literal = "+" }, .operator = "+", .left_operand = integer, .right_operand = integer } },
            },
        },
        .{
            .ExpressionStatement = &.{
                .token = integer.token(),
                .expression = .{ .InfixExpression = &.{ .token = .{ .type = .Minus, .literal = "-" }, .operator = "-", .left_operand = integer, .right_operand = integer } },
            },
        },
        .{
            .ExpressionStatement = &.{
                .token = integer.token(),
                .expression = .{ .InfixExpression = &.{ .token = .{ .type = .Asterisk, .literal = "*" }, .operator = "*", .left_operand = integer, .right_operand = integer } },
            },
        },
        .{
            .ExpressionStatement = &.{
                .token = integer.token(),
                .expression = .{ .InfixExpression = &.{ .token = .{ .type = .Slash, .literal = "/" }, .operator = "/", .left_operand = integer, .right_operand = integer } },
            },
        },
        .{
            .ExpressionStatement = &.{
                .token = integer.token(),
                .expression = .{ .InfixExpression = &.{ .token = .{ .type = .GreaterThan, .literal = ">" }, .operator = ">", .left_operand = integer, .right_operand = integer } },
            },
        },
        .{
            .ExpressionStatement = &.{
                .token = integer.token(),
                .expression = .{ .InfixExpression = &.{ .token = .{ .type = .LessThan, .literal = "<" }, .operator = "<", .left_operand = integer, .right_operand = integer } },
            },
        },
        .{
            .ExpressionStatement = &.{
                .token = integer.token(),
                .expression = .{ .InfixExpression = &.{ .token = .{ .type = .Equal, .literal = "==" }, .operator = "==", .left_operand = integer, .right_operand = integer } },
            },
        },
        .{
            .ExpressionStatement = &.{
                .token = integer.token(),
                .expression = .{ .InfixExpression = &.{ .token = .{ .type = .NotEqual, .literal = "!=" }, .operator = "!=", .left_operand = integer, .right_operand = integer } },
            },
        },
        .{
            .ExpressionStatement = &.{
                .token = bool_true.token(),
                .expression = .{ .InfixExpression = &.{ .token = .{ .type = .Equal, .literal = "==" }, .operator = "==", .left_operand = bool_true, .right_operand = bool_true } },
            },
        },
        .{
            .ExpressionStatement = &.{
                .token = bool_true.token(),
                .expression = .{ .InfixExpression = &.{ .token = .{ .type = .NotEqual, .literal = "!=" }, .operator = "!=", .left_operand = bool_true, .right_operand = bool_false } },
            },
        },
        .{
            .ExpressionStatement = &.{
                .token = bool_false.token(),
                .expression = .{ .InfixExpression = &.{ .token = .{ .type = .Equal, .literal = "==" }, .operator = "==", .left_operand = bool_false, .right_operand = bool_false } },
            },
        },
    };

    var allocator = std.testing.allocator;
    var ast = try Ast.parse(allocator, input);
    defer ast.deinit(allocator);

    // var buffer = try ast.write(allocator);
    // defer allocator.free(buffer);
    // std.debug.print("Infix Operators:\n{s}\n", .{buffer});

    try expectEqualAst(&expected, &ast);
}

test "Operator Precedence" {
    const Test = struct { input: []const u8, expected: []const u8 };
    var tests = [_]Test{
        .{ .input = "-a * b", .expected = "((-a) * b)" },
        .{ .input = "!-a", .expected = "(!(-a))" },
        .{ .input = "a + b + c", .expected = "((a + b) + c)" },
        .{ .input = "a + b - c", .expected = "((a + b) - c)" },
        .{ .input = "a * b * c", .expected = "((a * b) * c)" },
        .{ .input = "a * b / c", .expected = "((a * b) / c)" },
        .{ .input = "a + b / c", .expected = "(a + (b / c))" },
        .{ .input = "a + b * c + d / e - f", .expected = "(((a + (b * c)) + (d / e)) - f)" },
        // .{ .input = "3 + 4; -5 * 5", .expected = "(3 + 4)((-5) * 5)" },
        .{ .input = "5 > 4 == 3 < 4", .expected = "((5 > 4) == (3 < 4))" },
        .{ .input = "5 < 4 != 3 > 4", .expected = "((5 < 4) != (3 > 4))" },
        .{ .input = "3 + 4 * 5 == 3 * 1 + 4 * 5", .expected = "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))" },
        .{ .input = "true", .expected = "true" },
        .{ .input = "false", .expected = "false" },
        .{ .input = "3 > 5 == false", .expected = "((3 > 5) == false)" },
        .{ .input = "3 < 5 == true", .expected = "((3 < 5) == true)" },
        .{ .input = "1 + (2 + 3) + 4", .expected = "((1 + (2 + 3)) + 4)" },
        .{ .input = "(5 + 5) * 2", .expected = "((5 + 5) * 2)" },
        .{ .input = "-(5 + 5)", .expected = "(-(5 + 5))" },
        .{ .input = "!(true == true)", .expected = "(!(true == true))" },
        .{ .input = "a + add(b * c) + d", .expected = "((a + add((b * c))) + d)" },
        .{ .input = "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", .expected = "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))" },
        .{ .input = "add(a + b + c * d / f + g)", .expected = "add((((a + b) + ((c * d) / f)) + g))" },
    };

    var allocator = std.testing.allocator;
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    for (tests) |test_entry| {
        buffer.shrinkRetainingCapacity(0);

        var ast = try Ast.parse(allocator, test_entry.input);
        defer ast.deinit(allocator);

        try std.testing.expectEqual(@as(usize, 1), ast.nodes.len);

        try ast.nodes[0].ExpressionStatement.expression.write(&buffer, .debug_precedence);

        try std.testing.expectEqualStrings(test_entry.expected, buffer.items);
    }
}

test "If Expression" {
    const input =
        \\if (x < y) { x }
        \\if (x < y) { x } else { y }
    ;

    const x = Node{ .Identifier = &.{ .token = .{ .type = .Identifier, .literal = "x" }, .value = "x" } };
    const y = Node{ .Identifier = &.{ .token = .{ .type = .Identifier, .literal = "y" }, .value = "y" } };
    const expected = [_]Node{
        .{
            .IfExpression = &.{
                .token = .{ .type = .If, .literal = "if" },
                .condition = .{ .InfixExpression = &.{ .token = .{ .type = .LessThan, .literal = "<" }, .operator = "<", .left_operand = x, .right_operand = y } },
                .consequence = .{
                    .BlockStatement = &.{
                        .token = .{ .type = .LeftBrace, .literal = "{" }, //}
                        .statements = &[_]Node{.{ .ExpressionStatement = &.{ .token = x.token(), .expression = x } }},
                    },
                },
                .alternative = null,
            },
        },
        .{
            .IfExpression = &.{
                .token = .{ .type = .If, .literal = "if" },
                .condition = .{ .InfixExpression = &.{ .token = .{ .type = .LessThan, .literal = "<" }, .operator = "<", .left_operand = x, .right_operand = y } },
                .consequence = .{
                    .BlockStatement = &.{
                        .token = .{ .type = .LeftBrace, .literal = "{" }, //}
                        .statements = &[_]Node{.{ .ExpressionStatement = &.{ .token = x.token(), .expression = x } }},
                    },
                },
                .alternative = .{
                    .BlockStatement = &.{
                        .token = .{ .type = .LeftBrace, .literal = "{" }, //}
                        .statements = &[_]Node{.{ .ExpressionStatement = &.{ .token = y.token(), .expression = y } }},
                    },
                },
            },
        },
    };

    var allocator = std.testing.allocator;
    var ast = try Ast.parse(allocator, input);
    defer ast.deinit(allocator);

    // var buffer = try ast.write(allocator);
    // defer allocator.free(buffer);
    // std.debug.print("If Expressions:\n{s}\n", .{buffer});

    try expectEqualAst(&expected, &ast);
}

test "Function Literal" {
    const input =
        \\fn(x, y) { x + y; }
    ;

    const x = Node{ .Identifier = &.{ .token = .{ .type = .Identifier, .literal = "x" }, .value = "x" } };
    const y = Node{ .Identifier = &.{ .token = .{ .type = .Identifier, .literal = "y" }, .value = "y" } };
    const expected = [_]Node{
        .{
            .ExpressionStatement = &.{
                .token = .{ .type = .Function, .literal = "fn" },
                .expression = .{
                    .FunctionLiteral = &.{
                        .token = .{ .type = .Function, .literal = "fn" },
                        .parameters = &[_]Node{ x, y },
                        .body = .{
                            .BlockStatement = &.{
                                .token = .{ .type = .LeftBrace, .literal = "{" }, //}
                                .statements = &[_]Node{.{
                                    .ExpressionStatement = &.{
                                        .token = x.token(),
                                        .expression = .{
                                            .InfixExpression = &.{
                                                .token = .{ .type = .Plus, .literal = "+" },
                                                .operator = "+",
                                                .left_operand = x,
                                                .right_operand = y,
                                            },
                                        },
                                    },
                                }},
                            },
                        },
                    },
                },
            },
        },
    };

    var allocator = std.testing.allocator;
    var ast = try Ast.parse(allocator, input);
    defer ast.deinit(allocator);

    try expectEqualAst(&expected, &ast);
}

test "Call Expression" {
    const input =
        \\add(1, 1 * 2, 1 + 2)
    ;

    const add = Node{ .Identifier = &.{ .token = .{ .type = .Identifier, .literal = "add" }, .value = "add" } };
    const one = Node{ .Integer = &.{ .token = .{ .type = .Integer, .literal = "1" }, .value = 1 } };
    const two = Node{ .Integer = &.{ .token = .{ .type = .Integer, .literal = "2" }, .value = 2 } };
    const expected = [_]Node{
        .{
            .ExpressionStatement = &.{
                .token = add.token(),
                .expression = .{
                    .CallExpression = &.{
                        .token = .{ .type = .LeftParen, .literal = "(" }, // )
                        .function = add,
                        .arguments = &[_]Node{
                            one,
                            .{
                                .InfixExpression = &.{
                                    .token = .{ .type = .Asterisk, .literal = "*" },
                                    .operator = "*",
                                    .left_operand = one,
                                    .right_operand = two,
                                },
                            },
                            .{
                                .InfixExpression = &.{
                                    .token = .{ .type = .Plus, .literal = "+" },
                                    .operator = "+",
                                    .left_operand = one,
                                    .right_operand = two,
                                },
                            },
                        },
                    },
                },
            },
        },
    };

    var allocator = std.testing.allocator;
    var ast = try Ast.parse(allocator, input);
    defer ast.deinit(allocator);

    try expectEqualAst(&expected, &ast);
}

const TestError = error{ TestExpectedEqual, TestUnexpectedResult };

fn expectEqualAst(expected_nodes: []const Node, ast: *Ast) !void {
    if (ast.errors.len > 0) {
        std.debug.print("Parse failed with {d} errors:\n", .{ast.errors.len});
        for (ast.errors) |err| {
            std.debug.print("- {s}\n", .{err});
        }
        try std.testing.expect(false);
    }

    try expectEqualNodes(expected_nodes, ast.nodes);
}

fn expectEqualNodes(expected_nodes: []const Node, actual_nodes: []const Node) !void {
    try std.testing.expectEqual(expected_nodes.len, actual_nodes.len);

    for (expected_nodes, actual_nodes) |expected, actual| {
        try expectEqualNode(expected, actual);
    }
}

fn expectEqualNode(expected: Node, actual: Node) TestError!void {
    try std.testing.expectEqualStrings(@tagName(expected), @tagName(actual));
    // try std.testing.expectEqual(@intFromEnum(expected), @intFromEnum(actual));
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

fn expectEqualToken(expected: Token, actual: Token) !void {
    try std.testing.expectEqual(expected.type, actual.type);
    try std.testing.expectEqualStrings(expected.literal, actual.literal);
}

fn expectEqualLetStatement(expected: *const Ast.LetStatement, actual: *const Ast.LetStatement) !void {
    try expectEqualToken(expected.token, actual.token);
    try expectEqualNode(expected.name, actual.name);
    try expectEqualNode(expected.value, actual.value);
}

fn expectEqualReturnStatement(expected: *const Ast.ReturnStatement, actual: *const Ast.ReturnStatement) !void {
    try expectEqualToken(expected.token, actual.token);
    try expectEqualNode(expected.return_value, actual.return_value);
}

fn expectEqualExpressionStatement(expected: *const Ast.ExpressionStatement, actual: *const Ast.ExpressionStatement) !void {
    try expectEqualToken(expected.token, actual.token);
    try expectEqualNode(expected.expression, actual.expression);
}

fn expectEqualBlockStatement(expected: *const Ast.BlockStatement, actual: *const Ast.BlockStatement) !void {
    try expectEqualToken(expected.token, actual.token);
    try expectEqualNodes(expected.statements, actual.statements);
}

fn expectEqualIdentifier(expected: *const Ast.Identifier, actual: *const Ast.Identifier) !void {
    try expectEqualToken(expected.token, actual.token);
    try std.testing.expectEqualStrings(expected.value, actual.value);
}

fn expectEqualInteger(expected: *const Ast.Integer, actual: *const Ast.Integer) !void {
    try expectEqualToken(expected.token, actual.token);
    try std.testing.expectEqual(expected.value, actual.value);
}

fn expectEqualBoolean(expected: *const Ast.Boolean, actual: *const Ast.Boolean) !void {
    try expectEqualToken(expected.token, actual.token);
    try std.testing.expectEqual(expected.value, actual.value);
}

fn expectEqualPrefixExpression(expected: *const Ast.PrefixExpression, actual: *const Ast.PrefixExpression) !void {
    try expectEqualToken(expected.token, actual.token);
    try std.testing.expectEqualStrings(expected.operator, actual.operator);
    try expectEqualNode(expected.operand, actual.operand);
}

fn expectEqualInfixExpression(expected: *const Ast.InfixExpression, actual: *const Ast.InfixExpression) !void {
    try expectEqualToken(expected.token, actual.token);
    try std.testing.expectEqualStrings(expected.operator, actual.operator);
    try expectEqualNode(expected.left_operand, actual.left_operand);
    try expectEqualNode(expected.right_operand, actual.right_operand);
}

fn expectEqualGroupedExpression(expected: *const Ast.GroupedExpression, actual: *const Ast.GroupedExpression) !void {
    try expectEqualToken(expected.token, actual.token);
    try expectEqualNode(expected.expression, actual.expression);
}

fn expectEqualIfExpression(expected: *const Ast.IfExpression, actual: *const Ast.IfExpression) !void {
    try expectEqualToken(expected.token, actual.token);
    try expectEqualNode(expected.condition, actual.condition);
    try expectEqualNode(expected.consequence, actual.consequence);
    if (expected.alternative) |alternative| {
        try std.testing.expect(actual.alternative != null);
        try expectEqualNode(alternative, actual.alternative.?);
    } else {
        try std.testing.expectEqual(expected.alternative, actual.alternative);
    }
}

fn expectEqualFunctionLiteral(expected: *const Ast.FunctionLiteral, actual: *const Ast.FunctionLiteral) !void {
    try expectEqualToken(expected.token, actual.token);
    try std.testing.expectEqual(expected.parameters.len, actual.parameters.len);
    for (expected.parameters, actual.parameters) |expected_param, actual_param| {
        try expectEqualNode(expected_param, actual_param);
    }
    try expectEqualNode(expected.body, actual.body);
}

fn expectEqualCallExpression(expected: *const Ast.CallExpression, actual: *const Ast.CallExpression) !void {
    try expectEqualToken(expected.token, actual.token);
    try expectEqualNode(expected.function, actual.function);
    try expectEqualNodes(expected.arguments, actual.arguments);
}
