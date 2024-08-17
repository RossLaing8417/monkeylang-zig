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
    Index,
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
    var left_operand = switch (self.tokens[self.tok_i].type) {
        .Identifier => try self.parseIdentifier(),
        .Integer => try self.parseInteger(),
        .True => try self.parseBoolean(),
        .False => try self.parseBoolean(),
        .String => try self.parseString(),
        .Bang => try self.parsePrefixExpression(),
        .Minus => try self.parsePrefixExpression(),
        .LeftParen => try self.parseGroupedExpression(),
        .If => try self.parseIfExpression(),
        .Function => try self.parseFunctionLiteral(),
        .LeftBracket => try self.parseArrayLiteral(),
        else => unreachable,
    };

    while (true) {
        const token = self.tokens[self.tok_i];

        if (self.currentTokenIs(.SemiColon) or @intFromEnum(precedence) >= @intFromEnum(tokenPrecedence(token))) {
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
            .LeftBracket,
            => {},
            else => return left_operand,
        }

        left_operand = switch (self.tokens[self.tok_i].type) {
            .LeftParen => try self.parseCallExpression(left_operand),
            .LeftBracket => try self.parseIndexExpression(left_operand),
            else => try self.parseInfixExpression(left_operand),
        };
    }

    return left_operand;
}

fn parseGroupedExpression(self: *Parser) Error!Node {
    const grouped_expression = try self.allocator.create(Ast.GroupedExpression);

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

fn parseCallExpression(self: *Parser, function: Node) Error!Node {
    var arguments = std.ArrayList(Node).init(self.allocator);
    defer arguments.deinit();

    const token = try self.expectToken(.LeftParen);

    while (!self.currentTokenIs(.RightParen)) {
        try arguments.append(try self.parseExpression(.Lowest));
        _ = self.consumeToken(.Comma);
    }

    _ = try self.expectToken(.RightParen);

    const call_expression = try self.allocator.create(Ast.CallExpression);
    call_expression.* = .{
        .token = token,
        .function = function,
        .arguments = try arguments.toOwnedSlice(),
    };

    return .{ .CallExpression = call_expression };
}

fn parseIndexExpression(self: *Parser, expression: Node) Error!Node {
    const token = try self.expectToken(.LeftBracket);
    const index = try self.parseExpression(.Lowest);

    _ = try self.expectToken(.RightBracket);

    const index_expression = try self.allocator.create(Ast.IndexExpression);
    index_expression.* = .{
        .token = token,
        .expression = expression,
        .index = index,
    };

    return .{ .IndexExpression = index_expression };
}

fn parseBlockStatement(self: *Parser) Error!Node {
    var statements = std.ArrayList(Node).init(self.allocator);
    defer statements.deinit();

    const token = try self.expectToken(.LeftBrace);

    while (!self.currentTokenIs(.RightBrace) and !self.currentTokenIs(.Eof)) {
        try statements.append(try self.parseStatement());
    }

    _ = try self.expectToken(.RightBrace);

    const block_statement = try self.allocator.create(Ast.BlockStatement);
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
        .name = (try self.parseIdentifier()).Identifier,
    };

    _ = try self.expectToken(.Assign);

    let_statement.value = try self.parseExpression(.Lowest);

    return .{ .LetStatement = let_statement };
}

fn parseReturnStatement(self: *Parser) Error!Node {
    const return_statement = try self.allocator.create(Ast.ReturnStatement);
    return_statement.* = .{
        .token = try self.expectToken(.Return),
        .return_value = try self.parseExpression(.Lowest),
    };

    return .{ .ReturnStatement = return_statement };
}

fn parseExpressionStatement(self: *Parser) Error!Node {
    const expr_statement = try self.allocator.create(Ast.ExpressionStatement);
    expr_statement.* = Ast.ExpressionStatement{
        .token = self.tokens[self.tok_i],
        .expression = try self.parseExpression(.Lowest),
    };

    return .{ .ExpressionStatement = expr_statement };
}

fn parseIdentifier(self: *Parser) Error!Node {
    const token = try self.expectToken(.Identifier);

    const identifier = try self.allocator.create(Ast.Identifier);
    identifier.* = Ast.Identifier{
        .token = token,
        .value = token.literal,
    };

    return .{ .Identifier = identifier };
}

fn parseInteger(self: *Parser) Error!Node {
    const token = try self.expectToken(.Integer);

    const integer = try self.allocator.create(Ast.Integer);
    integer.* = Ast.Integer{
        .token = token,
        .value = try std.fmt.parseInt(i64, token.literal, 10),
    };

    return .{ .Integer = integer };
}

fn parseBoolean(self: *Parser) Error!Node {
    const token = self.consumeToken(.True) orelse
        try self.expectToken(.False);

    const boolean = try self.allocator.create(Ast.Boolean);
    boolean.* = Ast.Boolean{
        .token = token,
        .value = (token.type == .True),
    };

    return .{ .Boolean = boolean };
}

fn parseString(self: *Parser) Error!Node {
    const token = try self.expectToken(.String);

    const string = try self.allocator.create(Ast.String);
    string.* = .{
        .token = token,
        .value = token.literal,
    };

    return .{ .String = string };
}

fn parseFunctionLiteral(self: *Parser) Error!Node {
    var parameters = std.ArrayList(*Ast.Identifier).init(self.allocator);
    defer parameters.deinit();

    const token = try self.expectToken(.Function);
    _ = try self.expectToken(.LeftParen);

    while (!self.currentTokenIs(.RightParen)) {
        const tok = try self.expectToken(.Identifier);

        const identifier = try self.allocator.create(Ast.Identifier);
        identifier.* = Ast.Identifier{
            .token = tok,
            .value = tok.literal,
        };

        try parameters.append(identifier);

        _ = self.consumeToken(.Comma);
    }

    _ = try self.expectToken(.RightParen);

    const function_literal = try self.allocator.create(Ast.FunctionLiteral);
    function_literal.* = .{
        .token = token,
        .body = (try self.parseBlockStatement()).BlockStatement,
        .parameters = try parameters.toOwnedSlice(),
    };

    return .{ .FunctionLiteral = function_literal };
}

fn parseArrayLiteral(self: *Parser) Error!Node {
    var elements = std.ArrayList(Node).init(self.allocator);
    defer elements.deinit();

    const token = try self.expectToken(.LeftBracket);

    while (!self.currentTokenIs(.RightBracket)) {
        try elements.append(try self.parseExpression(.Lowest));
        _ = self.consumeToken(.Comma);
    }

    _ = try self.expectToken(.RightBracket);

    const array_literal = try self.allocator.create(Ast.ArrayLiteral);
    array_literal.* = .{
        .token = token,
        .elements = try elements.toOwnedSlice(),
    };

    return .{ .ArrayLiteral = array_literal };
}

fn parsePrefixExpression(self: *Parser) Error!Node {
    const token = self.nextToken();

    const prefix_expression = try self.allocator.create(Ast.PrefixExpression);
    prefix_expression.* = Ast.PrefixExpression{
        .token = token,
        .operator = token.literal,
        .operand = try self.parseExpression(.Prefix),
    };

    return .{ .PrefixExpression = prefix_expression };
}

fn parseInfixExpression(self: *Parser, left_operand: Node) Error!Node {
    const token = self.nextToken();

    const infix_expression = try self.allocator.create(Ast.InfixExpression);
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
        .LeftBracket => .Index,
        else => .Lowest,
    };
}

test "Let Statement" {
    const input =
        \\let foo = 10;
        \\let bar = true;
        \\let baz = foo;
        \\let str = "string";
    ;

    const expected = [_]Node{
        .{
            .LetStatement = @constCast(&Ast.LetStatement{
                .token = .{ .type = .Let, .literal = "let" },
                .name = @constCast(&Ast.Identifier{ .token = .{ .type = .Identifier, .literal = "foo" }, .value = "foo" }),
                .value = .{ .Integer = @constCast(&Ast.Integer{ .token = .{ .type = .Integer, .literal = "10" }, .value = 10 }) },
            }),
        },
        .{
            .LetStatement = @constCast(&Ast.LetStatement{
                .token = .{ .type = .Let, .literal = "let" },
                .name = @constCast(&Ast.Identifier{ .token = .{ .type = .Identifier, .literal = "bar" }, .value = "bar" }),
                .value = .{ .Boolean = @constCast(&Ast.Boolean{ .token = .{ .type = .True, .literal = "true" }, .value = true }) },
            }),
        },
        .{
            .LetStatement = @constCast(&Ast.LetStatement{
                .token = .{ .type = .Let, .literal = "let" },
                .name = @constCast(&Ast.Identifier{ .token = .{ .type = .Identifier, .literal = "baz" }, .value = "baz" }),
                .value = .{ .Identifier = @constCast(&Ast.Identifier{ .token = .{ .type = .Identifier, .literal = "foo" }, .value = "foo" }) },
            }),
        },
        .{
            .LetStatement = @constCast(&Ast.LetStatement{
                .token = .{ .type = .Let, .literal = "let" },
                .name = @constCast(&Ast.Identifier{ .token = .{ .type = .Identifier, .literal = "str" }, .value = "str" }),
                .value = .{ .String = @constCast(&Ast.String{ .token = .{ .type = .String, .literal = "string" }, .value = "string" }) },
            }),
        },
    };

    const allocator = std.testing.allocator;
    var ast = try Ast.parse(allocator, input);
    defer ast.deinit(allocator);

    try testAst(&expected, &ast);
}

test "Return Statement" {
    const input =
        \\return 5;
        \\return false;
        \\return foobar;
    ;

    const expected = [_]Node{
        .{
            .ReturnStatement = @constCast(&Ast.ReturnStatement{
                .token = .{ .type = .Return, .literal = "return" },
                .return_value = .{ .Integer = @constCast(&Ast.Integer{ .token = .{ .type = .Integer, .literal = "5" }, .value = 5 }) },
            }),
        },
        .{
            .ReturnStatement = @constCast(&Ast.ReturnStatement{
                .token = .{ .type = .Return, .literal = "return" },
                .return_value = .{ .Boolean = @constCast(&Ast.Boolean{ .token = .{ .type = .False, .literal = "false" }, .value = false }) },
            }),
        },
        .{
            .ReturnStatement = @constCast(&Ast.ReturnStatement{
                .token = .{ .type = .Return, .literal = "return" },
                .return_value = .{ .Identifier = @constCast(&Ast.Identifier{ .token = .{ .type = .Identifier, .literal = "foobar" }, .value = "foobar" }) },
            }),
        },
    };

    const allocator = std.testing.allocator;
    var ast = try Ast.parse(allocator, input);
    defer ast.deinit(allocator);

    try testAst(&expected, &ast);
}

test "Prefix Operators" {
    const input =
        \\!5;
        \\-15;
    ;

    const expected = [_]Node{
        .{
            .ExpressionStatement = @constCast(&Ast.ExpressionStatement{
                .token = .{ .type = .Bang, .literal = "!" },
                .expression = .{ .PrefixExpression = @constCast(&Ast.PrefixExpression{
                    .token = .{ .type = .Bang, .literal = "!" },
                    .operator = "!",
                    .operand = .{ .Integer = @constCast(&Ast.Integer{ .token = .{ .type = .Integer, .literal = "5" }, .value = 5 }) },
                }) },
            }),
        },
        .{
            .ExpressionStatement = @constCast(&Ast.ExpressionStatement{
                .token = .{ .type = .Minus, .literal = "-" },
                .expression = .{ .PrefixExpression = @constCast(&Ast.PrefixExpression{
                    .token = .{ .type = .Minus, .literal = "-" },
                    .operator = "-",
                    .operand = .{ .Integer = @constCast(&Ast.Integer{ .token = .{ .type = .Integer, .literal = "15" }, .value = 15 }) },
                }) },
            }),
        },
    };

    const allocator = std.testing.allocator;
    var ast = try Ast.parse(allocator, input);
    defer ast.deinit(allocator);

    try testAst(&expected, &ast);
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

    const integer = Node{ .Integer = @constCast(&Ast.Integer{ .token = .{ .type = .Integer, .literal = "5" }, .value = 5 }) };
    const bool_true = Node{ .Boolean = @constCast(&Ast.Boolean{ .token = .{ .type = .True, .literal = "true" }, .value = true }) };
    const bool_false = Node{ .Boolean = @constCast(&Ast.Boolean{ .token = .{ .type = .False, .literal = "false" }, .value = false }) };
    const expected = [_]Node{
        .{
            .ExpressionStatement = @constCast(&Ast.ExpressionStatement{
                .token = integer.token(),
                .expression = .{ .InfixExpression = @constCast(&Ast.InfixExpression{ .token = .{ .type = .Plus, .literal = "+" }, .operator = "+", .left_operand = integer, .right_operand = integer }) },
            }),
        },
        .{
            .ExpressionStatement = @constCast(&Ast.ExpressionStatement{
                .token = integer.token(),
                .expression = .{ .InfixExpression = @constCast(&Ast.InfixExpression{ .token = .{ .type = .Minus, .literal = "-" }, .operator = "-", .left_operand = integer, .right_operand = integer }) },
            }),
        },
        .{
            .ExpressionStatement = @constCast(&Ast.ExpressionStatement{
                .token = integer.token(),
                .expression = .{ .InfixExpression = @constCast(&Ast.InfixExpression{ .token = .{ .type = .Asterisk, .literal = "*" }, .operator = "*", .left_operand = integer, .right_operand = integer }) },
            }),
        },
        .{
            .ExpressionStatement = @constCast(&Ast.ExpressionStatement{
                .token = integer.token(),
                .expression = .{ .InfixExpression = @constCast(&Ast.InfixExpression{ .token = .{ .type = .Slash, .literal = "/" }, .operator = "/", .left_operand = integer, .right_operand = integer }) },
            }),
        },
        .{
            .ExpressionStatement = @constCast(&Ast.ExpressionStatement{
                .token = integer.token(),
                .expression = .{ .InfixExpression = @constCast(&Ast.InfixExpression{ .token = .{ .type = .GreaterThan, .literal = ">" }, .operator = ">", .left_operand = integer, .right_operand = integer }) },
            }),
        },
        .{
            .ExpressionStatement = @constCast(&Ast.ExpressionStatement{
                .token = integer.token(),
                .expression = .{ .InfixExpression = @constCast(&Ast.InfixExpression{ .token = .{ .type = .LessThan, .literal = "<" }, .operator = "<", .left_operand = integer, .right_operand = integer }) },
            }),
        },
        .{
            .ExpressionStatement = @constCast(&Ast.ExpressionStatement{
                .token = integer.token(),
                .expression = .{ .InfixExpression = @constCast(&Ast.InfixExpression{ .token = .{ .type = .Equal, .literal = "==" }, .operator = "==", .left_operand = integer, .right_operand = integer }) },
            }),
        },
        .{
            .ExpressionStatement = @constCast(&Ast.ExpressionStatement{
                .token = integer.token(),
                .expression = .{ .InfixExpression = @constCast(&Ast.InfixExpression{ .token = .{ .type = .NotEqual, .literal = "!=" }, .operator = "!=", .left_operand = integer, .right_operand = integer }) },
            }),
        },
        .{
            .ExpressionStatement = @constCast(&Ast.ExpressionStatement{
                .token = bool_true.token(),
                .expression = .{ .InfixExpression = @constCast(&Ast.InfixExpression{ .token = .{ .type = .Equal, .literal = "==" }, .operator = "==", .left_operand = bool_true, .right_operand = bool_true }) },
            }),
        },
        .{
            .ExpressionStatement = @constCast(&Ast.ExpressionStatement{
                .token = bool_true.token(),
                .expression = .{ .InfixExpression = @constCast(&Ast.InfixExpression{ .token = .{ .type = .NotEqual, .literal = "!=" }, .operator = "!=", .left_operand = bool_true, .right_operand = bool_false }) },
            }),
        },
        .{
            .ExpressionStatement = @constCast(&Ast.ExpressionStatement{
                .token = bool_false.token(),
                .expression = .{ .InfixExpression = @constCast(&Ast.InfixExpression{ .token = .{ .type = .Equal, .literal = "==" }, .operator = "==", .left_operand = bool_false, .right_operand = bool_false }) },
            }),
        },
    };

    const allocator = std.testing.allocator;
    var ast = try Ast.parse(allocator, input);
    defer ast.deinit(allocator);

    try testAst(&expected, &ast);
}

test "Operator Precedence" {
    const Test = struct { input: []const u8, expected: []const u8 };
    const tests = [_]Test{
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
        .{ .input = "a * [1, 2, 3, 4][b * c] * d", .expected = "((a * ([1, 2, 3, 4][(b * c)])) * d)" },
        .{ .input = "add(a * b[2], b[1], 2 * [1, 2][1])", .expected = "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))" },
    };

    const allocator = std.testing.allocator;
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    const writer = buffer.writer().any();

    for (tests) |test_entry| {
        buffer.shrinkRetainingCapacity(0);

        var ast = try Ast.parse(allocator, test_entry.input);
        defer ast.deinit(allocator);

        try std.testing.expectEqual(@as(usize, 1), ast.nodes.len);

        try ast.nodes[0].ExpressionStatement.expression.write(writer, .debug_precedence);

        try std.testing.expectEqualStrings(test_entry.expected, buffer.items);
    }
}

test "If Expression" {
    const input =
        \\if (x < y) { x }
        \\if (x < y) { x } else { y }
    ;

    const x = Node{ .Identifier = @constCast(&Ast.Identifier{ .token = .{ .type = .Identifier, .literal = "x" }, .value = "x" }) };
    const y = Node{ .Identifier = @constCast(&Ast.Identifier{ .token = .{ .type = .Identifier, .literal = "y" }, .value = "y" }) };
    const expected = [_]Node{
        .{
            .IfExpression = @constCast(&Ast.IfExpression{
                .token = .{ .type = .If, .literal = "if" },
                .condition = .{ .InfixExpression = @constCast(&Ast.InfixExpression{ .token = .{ .type = .LessThan, .literal = "<" }, .operator = "<", .left_operand = x, .right_operand = y }) },
                .consequence = .{
                    .BlockStatement = @constCast(&Ast.BlockStatement{
                        .token = .{ .type = .LeftBrace, .literal = "{" }, //}
                        .statements = &[_]Node{.{ .ExpressionStatement = @constCast(&Ast.ExpressionStatement{ .token = x.token(), .expression = x }) }},
                    }),
                },
                .alternative = null,
            }),
        },
        .{
            .IfExpression = @constCast(&Ast.IfExpression{
                .token = .{ .type = .If, .literal = "if" },
                .condition = .{ .InfixExpression = @constCast(&Ast.InfixExpression{ .token = .{ .type = .LessThan, .literal = "<" }, .operator = "<", .left_operand = x, .right_operand = y }) },
                .consequence = .{
                    .BlockStatement = @constCast(&Ast.BlockStatement{
                        .token = .{ .type = .LeftBrace, .literal = "{" }, //}
                        .statements = &[_]Node{.{ .ExpressionStatement = @constCast(&Ast.ExpressionStatement{ .token = x.token(), .expression = x }) }},
                    }),
                },
                .alternative = .{
                    .BlockStatement = @constCast(&Ast.BlockStatement{
                        .token = .{ .type = .LeftBrace, .literal = "{" }, //}
                        .statements = &[_]Node{.{ .ExpressionStatement = @constCast(&Ast.ExpressionStatement{ .token = y.token(), .expression = y }) }},
                    }),
                },
            }),
        },
    };

    const allocator = std.testing.allocator;
    var ast = try Ast.parse(allocator, input);
    defer ast.deinit(allocator);

    try testAst(&expected, &ast);
}

test "Function Literal" {
    const input =
        \\fn(x, y) { x + y; }
    ;

    const x = Node{ .Identifier = @constCast(&Ast.Identifier{ .token = .{ .type = .Identifier, .literal = "x" }, .value = "x" }) };
    const y = Node{ .Identifier = @constCast(&Ast.Identifier{ .token = .{ .type = .Identifier, .literal = "y" }, .value = "y" }) };
    const expected = [_]Node{
        .{
            .ExpressionStatement = @constCast(&Ast.ExpressionStatement{
                .token = .{ .type = .Function, .literal = "fn" },
                .expression = .{
                    .FunctionLiteral = @constCast(&Ast.FunctionLiteral{
                        .token = .{ .type = .Function, .literal = "fn" },
                        .parameters = &[_]*Ast.Identifier{ @constCast(x.Identifier), @constCast(y.Identifier) },
                        .body = @constCast(&Ast.BlockStatement{
                            .token = .{ .type = .LeftBrace, .literal = "{" }, //}
                            .statements = &[_]Node{.{
                                .ExpressionStatement = @constCast(&Ast.ExpressionStatement{
                                    .token = x.token(),
                                    .expression = .{
                                        .InfixExpression = @constCast(&Ast.InfixExpression{
                                            .token = .{ .type = .Plus, .literal = "+" },
                                            .operator = "+",
                                            .left_operand = x,
                                            .right_operand = y,
                                        }),
                                    },
                                }),
                            }},
                        }),
                    }),
                },
            }),
        },
    };

    const allocator = std.testing.allocator;
    var ast = try Ast.parse(allocator, input);
    defer ast.deinit(allocator);

    try testAst(&expected, &ast);
}

test "Call Expression" {
    const input =
        \\add(1, 1 * 2, 1 + 2)
    ;

    const add = Node{ .Identifier = @constCast(&Ast.Identifier{ .token = .{ .type = .Identifier, .literal = "add" }, .value = "add" }) };
    const one = Node{ .Integer = @constCast(&Ast.Integer{ .token = .{ .type = .Integer, .literal = "1" }, .value = 1 }) };
    const two = Node{ .Integer = @constCast(&Ast.Integer{ .token = .{ .type = .Integer, .literal = "2" }, .value = 2 }) };
    const expected = [_]Node{
        .{
            .ExpressionStatement = @constCast(&Ast.ExpressionStatement{
                .token = add.token(),
                .expression = .{
                    .CallExpression = @constCast(&Ast.CallExpression{
                        .token = .{ .type = .LeftParen, .literal = "(" }, // )
                        .function = add,
                        .arguments = &[_]Node{
                            one,
                            .{
                                .InfixExpression = @constCast(&Ast.InfixExpression{
                                    .token = .{ .type = .Asterisk, .literal = "*" },
                                    .operator = "*",
                                    .left_operand = one,
                                    .right_operand = two,
                                }),
                            },
                            .{
                                .InfixExpression = @constCast(&Ast.InfixExpression{
                                    .token = .{ .type = .Plus, .literal = "+" },
                                    .operator = "+",
                                    .left_operand = one,
                                    .right_operand = two,
                                }),
                            },
                        },
                    }),
                },
            }),
        },
    };

    const allocator = std.testing.allocator;
    var ast = try Ast.parse(allocator, input);
    defer ast.deinit(allocator);

    try testAst(&expected, &ast);
}

test "Arrays" {
    const input =
        \\[1, 2 * 2, 3 + 3]
        \\myArray[1 + 1]
    ;

    const one = Node{ .Integer = @constCast(&Ast.Integer{ .token = .{ .type = .Integer, .literal = "1" }, .value = 1 }) };
    const two = Node{ .Integer = @constCast(&Ast.Integer{ .token = .{ .type = .Integer, .literal = "2" }, .value = 2 }) };
    const three = Node{ .Integer = @constCast(&Ast.Integer{ .token = .{ .type = .Integer, .literal = "3" }, .value = 3 }) };
    const expected = [_]Node{
        .{
            .ExpressionStatement = @constCast(&Ast.ExpressionStatement{
                .token = .{ .type = .LeftBracket, .literal = "[" }, // ]
                .expression = .{
                    .ArrayLiteral = @constCast(&Ast.ArrayLiteral{
                        .token = .{ .type = .LeftBracket, .literal = "[" }, // ]
                        .elements = &[_]Node{
                            one,
                            .{
                                .InfixExpression = @constCast(&Ast.InfixExpression{
                                    .token = .{ .type = .Asterisk, .literal = "*" },
                                    .operator = "*",
                                    .left_operand = two,
                                    .right_operand = two,
                                }),
                            },
                            .{
                                .InfixExpression = @constCast(&Ast.InfixExpression{
                                    .token = .{ .type = .Plus, .literal = "+" },
                                    .operator = "+",
                                    .left_operand = three,
                                    .right_operand = three,
                                }),
                            },
                        },
                    }),
                },
            }),
        },
        .{
            .ExpressionStatement = @constCast(&Ast.ExpressionStatement{
                .token = .{ .type = .Identifier, .literal = "myArray" },
                .expression = .{
                    .IndexExpression = @constCast(&Ast.IndexExpression{
                        .token = .{ .type = .LeftBracket, .literal = "[" }, // ]
                        .expression = .{
                            .Identifier = @constCast(&Ast.Identifier{
                                .token = .{ .type = .Identifier, .literal = "myArray" },
                                .value = "myArray",
                            }),
                        },
                        .index = .{
                            .InfixExpression = @constCast(&Ast.InfixExpression{
                                .token = .{ .type = .Plus, .literal = "+" },
                                .operator = "+",
                                .left_operand = one,
                                .right_operand = one,
                            }),
                        },
                    }),
                },
            }),
        },
    };

    const allocator = std.testing.allocator;
    var ast = try Ast.parse(allocator, input);
    defer ast.deinit(allocator);

    try testAst(&expected, &ast);
}

const TestError = error{ TestExpectedEqual, TestUnexpectedResult };

fn testAst(expected_nodes: []const Node, ast: *Ast) !void {
    if (ast.errors.len > 0) {
        std.debug.print("Parse failed with {d} errors:\n", .{ast.errors.len});
        for (ast.errors) |err| {
            std.debug.print("- {s}\n", .{err});
        }
        try std.testing.expect(false);
    }

    try expectEqualNodeSlices(expected_nodes, ast.nodes);
}

fn expectEqualNodeSlices(expected_nodes: []const Node, actual_nodes: []const Node) !void {
    try std.testing.expectEqual(expected_nodes.len, actual_nodes.len);

    for (expected_nodes, actual_nodes) |expected, actual| {
        try expectEqualNodes(expected, actual);
    }
}

fn expectEqualNodes(expected: Node, actual: Node) TestError!void {
    try std.testing.expectEqualStrings(@tagName(expected), @tagName(actual));
    switch (expected) {
        // Statements
        .LetStatement => |node| try expectEqualLetStatements(node, actual.LetStatement),
        .ReturnStatement => |node| try expectEqualReturnStatements(node, actual.ReturnStatement),
        .ExpressionStatement => |node| try expectEqualExpressionStatements(node, actual.ExpressionStatement),

        .BlockStatement => |node| try expectEqualBlockStatements(node, actual.BlockStatement),

        // Expressions
        .Identifier => |node| try expectEqualIdentifiers(node, actual.Identifier),
        .Integer => |node| try expectEqualIntegers(node, actual.Integer),
        .Boolean => |node| try expectEqualBooleans(node, actual.Boolean),
        .String => |node| try expectEqualStrings(node, actual.String),

        .FunctionLiteral => |node| try expectEqualFunctionLiterals(node, actual.FunctionLiteral),
        .ArrayLiteral => |node| try expectEqualArrayLiterals(node, actual.ArrayLiteral),

        .PrefixExpression => |node| try expectEqualPrefixExpressions(node, actual.PrefixExpression),
        .InfixExpression => |node| try expectEqualInfixExpressions(node, actual.InfixExpression),
        .GroupedExpression => |node| try expectEqualGroupedExpressions(node, actual.GroupedExpression),
        .IfExpression => |node| try expectEqualIfExpressions(node, actual.IfExpression),
        .CallExpression => |node| try expectEqualCallExpressions(node, actual.CallExpression),
        .IndexExpression => |node| try expectEqualIndexExpressions(node, actual.IndexExpression),
    }
}

fn expectEqualTokens(expected: Token, actual: Token) !void {
    try std.testing.expectEqual(expected.type, actual.type);
    try std.testing.expectEqualStrings(expected.literal, actual.literal);
}

fn expectEqualLetStatements(expected: *const Ast.LetStatement, actual: *const Ast.LetStatement) !void {
    try expectEqualTokens(expected.token, actual.token);
    try expectEqualIdentifiers(expected.name, actual.name);
    try expectEqualNodes(expected.value, actual.value);
}

fn expectEqualReturnStatements(expected: *const Ast.ReturnStatement, actual: *const Ast.ReturnStatement) !void {
    try expectEqualTokens(expected.token, actual.token);
    try expectEqualNodes(expected.return_value, actual.return_value);
}

fn expectEqualExpressionStatements(expected: *const Ast.ExpressionStatement, actual: *const Ast.ExpressionStatement) !void {
    try expectEqualTokens(expected.token, actual.token);
    try expectEqualNodes(expected.expression, actual.expression);
}

fn expectEqualBlockStatements(expected: *const Ast.BlockStatement, actual: *const Ast.BlockStatement) !void {
    try expectEqualTokens(expected.token, actual.token);
    try expectEqualNodeSlices(expected.statements, actual.statements);
}

fn expectEqualIdentifiers(expected: *const Ast.Identifier, actual: *const Ast.Identifier) !void {
    try expectEqualTokens(expected.token, actual.token);
    try std.testing.expectEqualStrings(expected.value, actual.value);
}

fn expectEqualIntegers(expected: *const Ast.Integer, actual: *const Ast.Integer) !void {
    try expectEqualTokens(expected.token, actual.token);
    try std.testing.expectEqual(expected.value, actual.value);
}

fn expectEqualBooleans(expected: *const Ast.Boolean, actual: *const Ast.Boolean) !void {
    try expectEqualTokens(expected.token, actual.token);
    try std.testing.expectEqual(expected.value, actual.value);
}

fn expectEqualStrings(expected: *const Ast.String, actual: *const Ast.String) !void {
    try expectEqualTokens(expected.token, actual.token);
    try std.testing.expectEqualStrings(expected.value, actual.value);
}

fn expectEqualPrefixExpressions(expected: *const Ast.PrefixExpression, actual: *const Ast.PrefixExpression) !void {
    try expectEqualTokens(expected.token, actual.token);
    try std.testing.expectEqualStrings(expected.operator, actual.operator);
    try expectEqualNodes(expected.operand, actual.operand);
}

fn expectEqualInfixExpressions(expected: *const Ast.InfixExpression, actual: *const Ast.InfixExpression) !void {
    try expectEqualTokens(expected.token, actual.token);
    try std.testing.expectEqualStrings(expected.operator, actual.operator);
    try expectEqualNodes(expected.left_operand, actual.left_operand);
    try expectEqualNodes(expected.right_operand, actual.right_operand);
}

fn expectEqualGroupedExpressions(expected: *const Ast.GroupedExpression, actual: *const Ast.GroupedExpression) !void {
    try expectEqualTokens(expected.token, actual.token);
    try expectEqualNodes(expected.expression, actual.expression);
}

fn expectEqualIfExpressions(expected: *const Ast.IfExpression, actual: *const Ast.IfExpression) !void {
    try expectEqualTokens(expected.token, actual.token);
    try expectEqualNodes(expected.condition, actual.condition);
    try expectEqualNodes(expected.consequence, actual.consequence);
    if (expected.alternative) |alternative| {
        try std.testing.expect(actual.alternative != null);
        try expectEqualNodes(alternative, actual.alternative.?);
    } else {
        try std.testing.expectEqual(expected.alternative, actual.alternative);
    }
}

fn expectEqualFunctionLiterals(expected: *const Ast.FunctionLiteral, actual: *const Ast.FunctionLiteral) !void {
    try expectEqualTokens(expected.token, actual.token);
    try std.testing.expectEqual(expected.parameters.len, actual.parameters.len);
    for (expected.parameters, actual.parameters) |expected_param, actual_param| {
        try expectEqualIdentifiers(expected_param, actual_param);
    }
    try expectEqualBlockStatements(expected.body, actual.body);
}

fn expectEqualArrayLiterals(expected: *const Ast.ArrayLiteral, actual: *const Ast.ArrayLiteral) !void {
    try expectEqualTokens(expected.token, actual.token);
    try expectEqualNodeSlices(expected.elements, actual.elements);
}

fn expectEqualCallExpressions(expected: *const Ast.CallExpression, actual: *const Ast.CallExpression) !void {
    try expectEqualTokens(expected.token, actual.token);
    try expectEqualNodes(expected.function, actual.function);
    try expectEqualNodeSlices(expected.arguments, actual.arguments);
}

fn expectEqualIndexExpressions(expected: *const Ast.IndexExpression, actual: *const Ast.IndexExpression) !void {
    try expectEqualTokens(expected.token, actual.token);
    try expectEqualNodes(expected.expression, actual.expression);
    try expectEqualNodes(expected.index, actual.index);
}
