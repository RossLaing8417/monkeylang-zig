const std = @import("std");

const Evaluator = @This();

const Parser = @import("parser.zig");
const Lexer = @import("lexer.zig");
const Ast = @import("ast.zig");
const ObjectType = @import("object.zig");
const Environment = @import("environment.zig");

const Object = ObjectType.Object;

const NULL = Object{ .Literal = .{ .Null = .{} } };

allocator: std.mem.Allocator,
last_error: std.ArrayList(u8),
/// This exists as I don't have a solid solution to closures yet
closure_bin: std.ArrayList(*Environment),

pub fn init(allocator: std.mem.Allocator) !*Evaluator {
    var evaluator = try allocator.create(Evaluator);
    evaluator.* = .{
        .allocator = allocator,
        .last_error = try std.ArrayList(u8).initCapacity(allocator, 64),
        .closure_bin = std.ArrayList(*Environment).init(allocator),
    };
    return evaluator;
}

pub fn deinit(self: *Evaluator) void {
    for (self.closure_bin.items) |environment| {
        environment.deinit(self.allocator);
    }
    self.closure_bin.deinit();
    self.last_error.deinit();
    self.allocator.destroy(self);
}

pub fn evalAst(self: *Evaluator, ast: *Ast, environment: *Environment) !Object {
    return unwrapReturn(try self.evalNodes(ast.nodes, environment));
}

pub fn evalNodes(self: *Evaluator, nodes: []const Ast.Node, environment: *Environment) !Object {
    var result = NULL;
    for (nodes) |node| {
        result = try self.eval(node, environment);
        if (result == .ReturnValue or result == .Error) {
            break;
        }
    }
    return result;
}

pub fn eval(self: *Evaluator, node: Ast.Node, environment: *Environment) error{OutOfMemory}!Object {
    switch (node) {
        // Statements

        .LetStatement => |let_statement| {
            return try self.evalLetStatement(let_statement, environment);
        },
        .ReturnStatement => |return_statement| {
            return try self.evalReturnStatement(return_statement, environment);
        },
        .ExpressionStatement => |expr_statement| {
            return try self.eval(expr_statement.expression, environment);
        },

        .BlockStatement => |block_statement| {
            return try self.evalNodes(block_statement.statements, environment);
        },

        // Expressions

        .Identifier => |identifier| {
            return try self.evalIdentifier(identifier, environment);
        },
        .Integer => |integer| {
            return .{ .Literal = .{ .Integer = .{ .value = integer.value } } };
        },
        .Boolean => |boolean| {
            return .{ .Literal = .{ .Boolean = .{ .value = boolean.value } } };
        },

        .PrefixExpression => |prefix_expr| {
            return try self.evalPrefixExpression(
                prefix_expr.token,
                try self.eval(prefix_expr.operand, environment),
            );
        },
        .InfixExpression => |infix_expr| {
            return try self.evalInfixExpression(
                infix_expr.token,
                try self.eval(infix_expr.left_operand, environment),
                try self.eval(infix_expr.right_operand, environment),
            );
        },
        .GroupedExpression => |grouped_expr| {
            return try self.eval(grouped_expr.expression, environment);
        },
        .IfExpression => |if_expr| {
            return try self.evalIfExpression(if_expr, environment);
        },
        .FunctionLiteral => |function_literal| {
            return .{ .Literal = .{ .Function = .{
                .parameters = function_literal.parameters,
                .body = function_literal.body,
                .environment = environment,
            } } };
        },
        .CallExpression => |call_expr| {
            return try self.evalCallExpression(call_expr, environment);
        },
    }

    return NULL;
}

fn evalLetStatement(self: *Evaluator, let_statement: *const Ast.LetStatement, environment: *Environment) !Object {
    const result = try self.eval(let_statement.value, environment);
    if (result == .Error) {
        return result;
    }
    _ = try environment.set(let_statement.name.value, result);
    return result;
}

fn evalReturnStatement(self: *Evaluator, return_statement: *const Ast.ReturnStatement, environment: *Environment) !Object {
    var result = try self.eval(return_statement.return_value, environment);
    switch (result) {
        .Literal => |literal| return .{ .ReturnValue = literal },
        .Error => return result,
        else => unreachable,
    }
}

fn evalIdentifier(self: *Evaluator, identifier: *const Ast.Identifier, environment: *Environment) !Object {
    if (environment.get(identifier.value)) |object| {
        return object.*;
    }

    return try self.evalError("Unknown identifier: '{s}'", .{identifier.value});
}

fn evalPrefixExpression(self: *Evaluator, operator: Lexer.Token, operand: Object) !Object {
    switch (operator.type) {
        .Bang => return self.evalBangOperator(operand),
        .Minus => return self.evalNegativeOperator(operand),
        else => return try self.evalError("Invalid prefix operator: {s}", .{@tagName(operator.type)}),
    }
}

fn evalBangOperator(self: *Evaluator, operand: Object) !Object {
    switch (operand) {
        .Literal => |literal| switch (literal) {
            .Integer => |integer| return .{ .Literal = .{ .Boolean = .{ .value = integer.value == 0 } } },
            .Boolean => |boolean| return .{ .Literal = .{ .Boolean = .{ .value = !boolean.value } } },
            .Null => return NULL,
            else => return try self.evalError("Invalid literal '{s}' for operand", .{@tagName(literal)}),
        },
        .Error => return operand,
        else => return try self.evalError("Invalid type. Expected 'Literal' but found '{s}'", .{@tagName(operand)}),
    }
}

fn evalNegativeOperator(self: *Evaluator, operand: Object) !Object {
    switch (operand) {
        .Literal => |literal| switch (literal) {
            .Integer => |integer| return .{ .Literal = .{ .Integer = .{ .value = -integer.value } } },
            .Null => return NULL,
            else => return try self.evalError("Operand type mismatch. Invalid operation: -'{s}'", .{@tagName(literal)}),
        },
        .Error => return operand,
        else => return try self.evalError("Invalid type. Expected 'Literal' but found '{s}'", .{@tagName(operand)}),
    }
}

fn evalInfixExpression(self: *Evaluator, operator: Lexer.Token, left_operand: Object, right_operand: Object) !Object {
    switch (left_operand) {
        .Literal => |left_literal| switch (right_operand) {
            .Literal => |right_literal| switch (left_literal) {
                .Integer => |left_integer| {
                    switch (right_literal) {
                        .Integer => |right_integer| return self.evalIntegerInfixExpression(
                            operator,
                            left_integer.value,
                            right_integer.value,
                        ),
                        .Null => return NULL,
                        else => return try self.evalError(
                            "Right operand type mismatch. Invalid operation: 'Integer' {s} '{s}'",
                            .{ operator.literal, @tagName(right_literal) },
                        ),
                    }
                },
                .Boolean => |left_boolean| {
                    switch (right_literal) {
                        .Boolean => |right_boolean| return self.evalBooleanInfixExpression(
                            operator,
                            left_boolean.value,
                            right_boolean.value,
                        ),
                        .Null => return NULL,
                        else => return try self.evalError(
                            "Right operand type mismatch. Invalid operation: 'Boolean' {s} '{s}'",
                            .{ operator.literal, @tagName(right_literal) },
                        ),
                    }
                },
                .Null => return NULL,
                else => return try self.evalError(
                    "Invalid literal '{s}' for left operand",
                    .{@tagName(left_literal)},
                ),
            },
            .Error => return right_operand,
            else => return try self.evalError(
                "Right operand type mismatch. Expected 'Literal' but found '{s}'",
                .{@tagName(right_operand)},
            ),
        },
        .Error => return left_operand,
        else => return try self.evalError(
            "Right operand type mismatch. Expected 'Literal' but found '{s}'",
            .{@tagName(left_operand)},
        ),
    }
}

fn evalIntegerInfixExpression(self: *Evaluator, operator: Lexer.Token, left_operand: i64, right_operand: i64) !Object {
    switch (operator.type) {
        .Plus => return .{ .Literal = .{ .Integer = .{ .value = left_operand + right_operand } } },
        .Minus => return .{ .Literal = .{ .Integer = .{ .value = left_operand - right_operand } } },
        .Asterisk => return .{ .Literal = .{ .Integer = .{ .value = left_operand * right_operand } } },
        .Slash => return .{ .Literal = .{ .Integer = .{ .value = @divExact(left_operand, right_operand) } } },
        .Equal => return .{ .Literal = .{ .Boolean = .{ .value = left_operand == right_operand } } },
        .NotEqual => return .{ .Literal = .{ .Boolean = .{ .value = left_operand != right_operand } } },
        .LessThan => return .{ .Literal = .{ .Boolean = .{ .value = left_operand < right_operand } } },
        .GreaterThan => return .{ .Literal = .{ .Boolean = .{ .value = left_operand > right_operand } } },
        else => return try self.evalError("Invalid operation: 'Integer' {s} 'Integer'", .{operator.literal}),
    }
}

fn evalBooleanInfixExpression(self: *Evaluator, operator: Lexer.Token, left_operand: bool, right_operand: bool) !Object {
    switch (operator.type) {
        .Equal => return .{ .Literal = .{ .Boolean = .{ .value = left_operand == right_operand } } },
        .NotEqual => return .{ .Literal = .{ .Boolean = .{ .value = left_operand != right_operand } } },
        else => return try self.evalError("Invalid operation: 'Boolean' {s} 'Boolean'", .{operator.literal}),
    }
}

fn evalIfExpression(self: *Evaluator, if_expr: *const Ast.IfExpression, environment: *Environment) !Object {
    const condition = try self.eval(if_expr.condition, environment);
    if (condition == .Error) {
        return condition;
    }

    if (isTruthy(condition)) {
        return try self.eval(if_expr.consequence, environment);
    } else if (if_expr.alternative) |alternative| {
        return try self.eval(alternative, environment);
    }

    return NULL;
}

fn evalCallExpression(self: *Evaluator, call_expr: *const Ast.CallExpression, environment: *Environment) !Object {
    const function = blk: {
        const function = try self.eval(call_expr.function, environment);
        if (function == .Error) {
            return function;
        }

        std.debug.assert(function != .ReturnValue);

        if (function.Literal != .Function) {
            return self.evalError(
                "Call expression type mismatch. Expected 'Function' but found '{s}'",
                .{@tagName(function.Literal)},
            );
        }

        break :blk function.Literal.Function;
    };

    std.debug.assert(call_expr.arguments.len == function.parameters.len);

    var enclosed_env = try Environment.initEnclosed(self.allocator, function.environment);
    errdefer enclosed_env.deinit(self.allocator);

    for (function.parameters, call_expr.arguments) |param, arg| {
        const result = try self.eval(arg, environment);
        if (result == .Error) {
            return result;
        }
        try enclosed_env.set(param.value, result);
    }

    var result = unwrapReturn(try self.eval(.{ .BlockStatement = function.body }, enclosed_env));

    // Keeping the closure scope alive until I can figure out an elegant solution
    if (result == .Literal and result.Literal == .Function) {
        try self.closure_bin.append(enclosed_env);
    } else {
        enclosed_env.deinit(self.allocator);
    }

    return result;
}

fn isTruthy(object: Object) bool {
    switch (object) {
        .Literal => |literal| switch (literal) {
            .Integer => |integer| return integer.value != 0,
            .Boolean => |boolean| return boolean.value,
            else => return false,
        },
        else => return false,
    }
}

fn evalError(self: *Evaluator, comptime message: []const u8, args: anytype) !Object {
    self.last_error.shrinkRetainingCapacity(0);
    try self.last_error.writer().print(message, args);
    return .{ .Error = .{ .value = self.last_error.items } };
}

fn unwrapReturn(value: Object) Object {
    return switch (value) {
        .ReturnValue => |return_value| .{ .Literal = return_value },
        else => value,
    };
}

const TestInput = struct {
    input: []const u8,
    expected: Object,
};

test "Eval Integer Expression" {
    const input = [_]TestInput{
        .{ .input = "5", .expected = .{ .Literal = .{ .Integer = .{ .value = 5 } } } },
        .{ .input = "10", .expected = .{ .Literal = .{ .Integer = .{ .value = 10 } } } },
        .{ .input = "-5", .expected = .{ .Literal = .{ .Integer = .{ .value = -5 } } } },
        .{ .input = "-10", .expected = .{ .Literal = .{ .Integer = .{ .value = -10 } } } },
        .{ .input = "5 + 5 + 5 + 5 - 10", .expected = .{ .Literal = .{ .Integer = .{ .value = 10 } } } },
        .{ .input = "2 * 2 * 2 * 2 * 2", .expected = .{ .Literal = .{ .Integer = .{ .value = 32 } } } },
        .{ .input = "-50 + 100 + -50", .expected = .{ .Literal = .{ .Integer = .{ .value = 0 } } } },
        .{ .input = "5 * 2 + 10", .expected = .{ .Literal = .{ .Integer = .{ .value = 20 } } } },
        .{ .input = "5 + 2 * 10", .expected = .{ .Literal = .{ .Integer = .{ .value = 25 } } } },
        .{ .input = "20 + 2 * -10", .expected = .{ .Literal = .{ .Integer = .{ .value = 0 } } } },
        .{ .input = "50 / 2 * 2 + 10", .expected = .{ .Literal = .{ .Integer = .{ .value = 60 } } } },
        .{ .input = "2 * (5 + 10)", .expected = .{ .Literal = .{ .Integer = .{ .value = 30 } } } },
        .{ .input = "3 * 3 * 3 + 10", .expected = .{ .Literal = .{ .Integer = .{ .value = 37 } } } },
        .{ .input = "3 * (3 * 3) + 10", .expected = .{ .Literal = .{ .Integer = .{ .value = 37 } } } },
        .{ .input = "(5 + 10 * 2 + 15 / 3) * 2 + -10", .expected = .{ .Literal = .{ .Integer = .{ .value = 50 } } } },
    };

    var allocator = std.testing.allocator;
    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    for (input) |test_input| {
        var ast = try Ast.parse(allocator, test_input.input);
        defer ast.deinit(allocator);

        try testAst(&ast);

        var environment = try Environment.init(allocator);
        defer environment.deinit(allocator);

        var result = try evaluator.evalAst(&ast, environment);
        try expectEqualObjects(test_input.expected, result);
    }
}

test "Eval Boolean Expression" {
    const input = [_]TestInput{
        .{ .input = "true", .expected = .{ .Literal = .{ .Boolean = .{ .value = true } } } },
        .{ .input = "false", .expected = .{ .Literal = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "true", .expected = .{ .Literal = .{ .Boolean = .{ .value = true } } } },
        .{ .input = "false", .expected = .{ .Literal = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "1 < 2", .expected = .{ .Literal = .{ .Boolean = .{ .value = true } } } },
        .{ .input = "1 > 2", .expected = .{ .Literal = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "1 < 1", .expected = .{ .Literal = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "1 > 1", .expected = .{ .Literal = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "1 == 1", .expected = .{ .Literal = .{ .Boolean = .{ .value = true } } } },
        .{ .input = "1 != 1", .expected = .{ .Literal = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "1 == 2", .expected = .{ .Literal = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "1 != 2", .expected = .{ .Literal = .{ .Boolean = .{ .value = true } } } },
        .{ .input = "true == true", .expected = .{ .Literal = .{ .Boolean = .{ .value = true } } } },
        .{ .input = "false == false", .expected = .{ .Literal = .{ .Boolean = .{ .value = true } } } },
        .{ .input = "true == false", .expected = .{ .Literal = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "true != false", .expected = .{ .Literal = .{ .Boolean = .{ .value = true } } } },
        .{ .input = "false != true", .expected = .{ .Literal = .{ .Boolean = .{ .value = true } } } },
        .{ .input = "(1 < 2) == true", .expected = .{ .Literal = .{ .Boolean = .{ .value = true } } } },
        .{ .input = "(1 < 2) == false", .expected = .{ .Literal = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "(1 > 2) == true", .expected = .{ .Literal = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "(1 > 2) == false", .expected = .{ .Literal = .{ .Boolean = .{ .value = true } } } },
    };

    var allocator = std.testing.allocator;
    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    for (input) |test_input| {
        var ast = try Ast.parse(allocator, test_input.input);
        defer ast.deinit(allocator);

        try testAst(&ast);

        var environment = try Environment.init(allocator);
        defer environment.deinit(allocator);

        var result = try evaluator.evalAst(&ast, environment);
        try expectEqualObjects(test_input.expected, result);
    }
}

test "Eval Bang Operator" {
    const input = [_]TestInput{
        .{ .input = "!true", .expected = .{ .Literal = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "!false", .expected = .{ .Literal = .{ .Boolean = .{ .value = true } } } },
        .{ .input = "!5", .expected = .{ .Literal = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "!!true", .expected = .{ .Literal = .{ .Boolean = .{ .value = true } } } },
        .{ .input = "!!false", .expected = .{ .Literal = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "!!5", .expected = .{ .Literal = .{ .Boolean = .{ .value = true } } } },
    };

    var allocator = std.testing.allocator;
    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    for (input) |test_input| {
        var ast = try Ast.parse(allocator, test_input.input);
        defer ast.deinit(allocator);

        try testAst(&ast);

        var environment = try Environment.init(allocator);
        defer environment.deinit(allocator);

        var result = try evaluator.evalAst(&ast, environment);
        try expectEqualObjects(test_input.expected, result);
    }
}

test "Eval If Else Expression" {
    const input = [_]TestInput{
        .{ .input = "if (true) { 10 }", .expected = .{ .Literal = .{ .Integer = .{ .value = 10 } } } },
        .{ .input = "if (false) { 10 }", .expected = .{ .Literal = .{ .Null = .{} } } },
        .{ .input = "if (1) { 10 }", .expected = .{ .Literal = .{ .Integer = .{ .value = 10 } } } },
        .{ .input = "if (1 < 2) { 10 }", .expected = .{ .Literal = .{ .Integer = .{ .value = 10 } } } },
        .{ .input = "if (1 > 2) { 10 }", .expected = .{ .Literal = .{ .Null = .{} } } },
        .{ .input = "if (1 > 2) { 10 } else { 20 }", .expected = .{ .Literal = .{ .Integer = .{ .value = 20 } } } },
        .{ .input = "if (1 < 2) { 10 } else { 20 }", .expected = .{ .Literal = .{ .Integer = .{ .value = 10 } } } },
    };

    var allocator = std.testing.allocator;
    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    for (input) |test_input| {
        var ast = try Ast.parse(allocator, test_input.input);
        defer ast.deinit(allocator);

        try testAst(&ast);

        var environment = try Environment.init(allocator);
        defer environment.deinit(allocator);

        var result = try evaluator.evalAst(&ast, environment);
        try expectEqualObjects(test_input.expected, result);
    }
}

test "Eval Return Statement" {
    const input = [_]TestInput{
        .{ .input = "return 10;", .expected = .{ .Literal = .{ .Integer = .{ .value = 10 } } } },
        .{ .input = "return 10; 9;", .expected = .{ .Literal = .{ .Integer = .{ .value = 10 } } } },
        .{ .input = "return 2 * 5; 9;", .expected = .{ .Literal = .{ .Integer = .{ .value = 10 } } } },
        .{ .input = "9; return 2 * 5; 9;", .expected = .{ .Literal = .{ .Integer = .{ .value = 10 } } } },
        .{
            .input =
            \\if (10 > 1) {
            \\    if (10 > 1) {
            \\        return 10;
            \\    }
            \\    return 1;
            \\}
            ,
            .expected = .{ .Literal = .{ .Integer = .{ .value = 10 } } },
        },
    };

    var allocator = std.testing.allocator;
    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    for (input) |test_input| {
        var ast = try Ast.parse(allocator, test_input.input);
        defer ast.deinit(allocator);

        try testAst(&ast);

        var environment = try Environment.init(allocator);
        defer environment.deinit(allocator);

        var result = try evaluator.evalAst(&ast, environment);
        try expectEqualObjects(test_input.expected, result);
    }
}

test "Eval Error Handling" {
    const input = [_]TestInput{
        .{ .input = "5 + true;", .expected = .{ .Error = .{ .value = "Right operand type mismatch. Invalid operation: 'Integer' + 'Boolean'" } } },
        .{ .input = "5 + true; 5;", .expected = .{ .Error = .{ .value = "Right operand type mismatch. Invalid operation: 'Integer' + 'Boolean'" } } },
        .{ .input = "-true", .expected = .{ .Error = .{ .value = "Operand type mismatch. Invalid operation: -'Boolean'" } } },
        .{ .input = "true + false;", .expected = .{ .Error = .{ .value = "Invalid operation: 'Boolean' + 'Boolean'" } } },
        .{ .input = "5; true + false; 5", .expected = .{ .Error = .{ .value = "Invalid operation: 'Boolean' + 'Boolean'" } } },
        .{ .input = "if (10 > 1) { true + false; }", .expected = .{ .Error = .{ .value = "Invalid operation: 'Boolean' + 'Boolean'" } } },
        .{
            .input =
            \\if (10 > 1) {
            \\    if (10 > 1) {
            \\        return true + false;
            \\    } return 1;
            \\}
            ,
            .expected = .{ .Error = .{ .value = "Invalid operation: 'Boolean' + 'Boolean'" } },
        },
        .{ .input = "foobar", .expected = .{ .Error = .{ .value = "Unknown identifier: 'foobar'" } } },
    };

    var allocator = std.testing.allocator;
    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    for (input) |test_input| {
        var ast = try Ast.parse(allocator, test_input.input);
        defer ast.deinit(allocator);

        try testAst(&ast);

        var environment = try Environment.init(allocator);
        defer environment.deinit(allocator);

        var result = try evaluator.evalAst(&ast, environment);
        try expectEqualObjects(test_input.expected, result);
    }
}

test "Eval Let Statement" {
    const input = [_]TestInput{
        .{ .input = "let a = 5; a;", .expected = .{ .Literal = .{ .Integer = .{ .value = 5 } } } },
        .{ .input = "let a = 5 * 5; a;", .expected = .{ .Literal = .{ .Integer = .{ .value = 25 } } } },
        .{ .input = "let a = 5; let b = a; b;", .expected = .{ .Literal = .{ .Integer = .{ .value = 5 } } } },
        .{ .input = "let a = 5; let b = a; let c = a + b + 5; c;", .expected = .{ .Literal = .{ .Integer = .{ .value = 15 } } } },
    };

    var allocator = std.testing.allocator;
    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    for (input) |test_input| {
        var ast = try Ast.parse(allocator, test_input.input);
        defer ast.deinit(allocator);

        try testAst(&ast);

        var environment = try Environment.init(allocator);
        defer environment.deinit(allocator);

        var result = try evaluator.evalAst(&ast, environment);
        try expectEqualObjects(test_input.expected, result);
    }
}

test "Eval Function Literal" {
    const input = "fn(x) { x + 2; }";

    var allocator = std.testing.allocator;
    var ast = try Ast.parse(allocator, input);
    defer ast.deinit(allocator);

    try testAst(&ast);

    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    var environment = try Environment.init(allocator);
    defer environment.deinit(allocator);

    const function = ast.nodes[0].ExpressionStatement.expression.FunctionLiteral;
    const expected = Object{ .Literal = .{ .Function = .{
        .parameters = function.parameters,
        .body = function.body,
        .environment = environment,
    } } };

    var result = try evaluator.evalAst(&ast, environment);
    try expectEqualObjects(expected, result);
}

test "Eval Call Expression" {
    const input = [_]TestInput{
        .{ .input = "let identity = fn(x) { x; }; identity(5);", .expected = .{ .Literal = .{ .Integer = .{ .value = 5 } } } },
        .{ .input = "let identity = fn(x) { return x; }; identity(5);", .expected = .{ .Literal = .{ .Integer = .{ .value = 5 } } } },
        .{ .input = "let double = fn(x) { x * 2; }; double(5);", .expected = .{ .Literal = .{ .Integer = .{ .value = 10 } } } },
        .{ .input = "let add = fn(x, y) { x + y; }; add(5, 5);", .expected = .{ .Literal = .{ .Integer = .{ .value = 10 } } } },
        .{ .input = "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", .expected = .{ .Literal = .{ .Integer = .{ .value = 20 } } } },
        .{ .input = "fn(x) { x; }(5)", .expected = .{ .Literal = .{ .Integer = .{ .value = 5 } } } },
    };

    var allocator = std.testing.allocator;
    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    for (input) |test_input| {
        var ast = try Ast.parse(allocator, test_input.input);
        defer ast.deinit(allocator);

        try testAst(&ast);

        var environment = try Environment.init(allocator);
        defer environment.deinit(allocator);

        var result = try evaluator.evalAst(&ast, environment);
        try expectEqualObjects(test_input.expected, result);
    }
}

test "Eval Closure" {
    const input =
        \\let newAdder = fn(x) {
        \\    fn(y) { x + y };
        \\};
        \\let addTwo = newAdder(2);
        \\addTwo(2);
    ;

    var allocator = std.testing.allocator;
    var ast = try Ast.parse(allocator, input);
    defer ast.deinit(allocator);

    try testAst(&ast);

    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    var environment = try Environment.init(allocator);
    defer environment.deinit(allocator);

    const expected = Object{ .Literal = .{ .Integer = .{ .value = 4 } } };

    var result = try evaluator.evalAst(&ast, environment);
    try expectEqualObjects(expected, result);
}

fn testAst(ast: *Ast) !void {
    if (ast.errors.len > 0) {
        std.debug.print("Parse failed with {d} errors:\n", .{ast.errors.len});
        for (ast.errors) |err| {
            std.debug.print("- {s}\n", .{err});
        }
        try std.testing.expect(false);
    }
}

fn expectEqualObjects(expected: Object, actual: Object) !void {
    try std.testing.expectEqualStrings(@tagName(expected), @tagName(actual));
    switch (expected) {
        .Literal => |literal| try expectEqualLiterals(literal, actual.Literal),
        .ReturnValue => |return_value| try expectEqualLiterals(return_value, actual.ReturnValue),
        .Error => |err| try std.testing.expectEqualStrings(err.value, actual.Error.value),
    }
}

fn expectEqualLiterals(expected: ObjectType.Literal, actual: ObjectType.Literal) !void {
    try std.testing.expectEqualStrings(@tagName(expected), @tagName(actual));
    switch (expected) {
        .Integer => |integer| try std.testing.expectEqual(integer.value, actual.Integer.value),
        .Boolean => |boolean| try std.testing.expectEqual(boolean.value, actual.Boolean.value),
        .Function => |function| try expectEqualFunctions(function, actual.Function),
        .Null => {},
    }
}

fn expectEqualFunctions(expected: ObjectType.Function, actual: ObjectType.Function) !void {
    try std.testing.expectEqual(expected.parameters.len, actual.parameters.len);
    for (expected.parameters, actual.parameters) |expected_param, actual_param| {
        try std.testing.expectEqualStrings(expected_param.value, actual_param.value);
    }
    try std.testing.expectEqual(expected.body, actual.body);
    try std.testing.expectEqual(expected.environment, actual.environment);
}
