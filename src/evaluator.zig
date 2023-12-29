const std = @import("std");

const Parser = @import("parser.zig");
const Lexer = @import("lexer.zig");
const Ast = @import("ast.zig");

const Object = @import("object.zig");

const NULL = Object.Object{ .Literal = .{ .Null = .{} } };

pub fn eval(node: Ast.Node) Object.Object {
    switch (node) {
        .Statement => |statement| {
            switch (statement.*) {
                .Program => |program| {
                    return evalProgram(program);
                },
                .ExpressionStatement => |expr_statement| {
                    return eval(.{ .Expression = expr_statement.expression });
                },
                .ReturnStatement => |return_statement| {
                    return .{ .ReturnValue = eval(.{ .Expression = return_statement.return_value }).Literal };
                },
                else => unreachable,
            }
        },
        .Expression => |expression| {
            switch (expression.*) {
                .Integer => |integer| {
                    return .{ .Literal = .{ .Integer = .{ .value = integer.value } } };
                },
                .Boolean => |boolean| {
                    return .{ .Literal = .{ .Boolean = .{ .value = boolean.value } } };
                },
                .PrefixExpression => |prefix_expr| {
                    var operand = eval(.{ .Expression = prefix_expr.operand });
                    return evalPrefixExpression(prefix_expr.token, operand);
                },
                .InfixExpression => |infix_expr| {
                    var left_operand = eval(.{ .Expression = infix_expr.left_operand });
                    var right_operand = eval(.{ .Expression = infix_expr.right_operand });
                    return evalInfixExpression(infix_expr.token, left_operand, right_operand);
                },
                .IfExpression => |if_expr| {
                    return evalIfExpression(if_expr);
                },
                else => unreachable,
            }
        },
    }

    return NULL;
}

fn evalStatements(statements: []*Ast.Statement) Object.Object {
    var result = NULL;

    for (statements) |statement| {
        result = eval(.{ .Statement = statement });

        if (result == .ReturnValue) {
            break;
        }
    }

    return result;
}

fn evalProgram(program: *Ast.Program) Object.Object {
    var result = evalStatements(program.statements.items);

    if (result == .ReturnValue) {
        return .{ .Literal = result.ReturnValue };
    }

    return result;
}

fn evalPrefixExpression(operator: Lexer.Token, operand: Object.Object) Object.Object {
    switch (operator.type) {
        .Bang => return evalBangOperator(operand),
        .Minus => return evalNegativeOperator(operand),
        else => unreachable,
    }
}

fn evalBangOperator(operand: Object.Object) Object.Object {
    switch (operand) {
        .Literal => |literal| switch (literal) {
            .Integer => |integer| return .{ .Literal = .{ .Boolean = .{ .value = integer.value == 0 } } },
            .Boolean => |boolean| return .{ .Literal = .{ .Boolean = .{ .value = !boolean.value } } },
            .Null => return NULL,
        },
        else => unreachable,
    }
}

fn evalNegativeOperator(operand: Object.Object) Object.Object {
    switch (operand) {
        .Literal => |literal| switch (literal) {
            .Integer => |integer| return .{ .Literal = .{ .Integer = .{ .value = -integer.value } } },
            else => return NULL,
        },
        else => unreachable,
    }
}

fn evalInfixExpression(operator: Lexer.Token, left_operand: Object.Object, right_operand: Object.Object) Object.Object {
    if (left_operand == .Literal and left_operand.Literal == .Integer and right_operand == .Literal and right_operand.Literal == .Integer) {
        return evalIntegerInfixExpression(operator, left_operand.Literal.Integer.value, right_operand.Literal.Integer.value);
    }

    if (left_operand == .Literal and left_operand.Literal == .Boolean and right_operand == .Literal and right_operand.Literal == .Boolean) {
        return evalBooleanInfixExpression(operator, left_operand.Literal.Boolean.value, right_operand.Literal.Boolean.value);
    }

    return NULL;
}

fn evalIntegerInfixExpression(operator: Lexer.Token, left_operand: i64, right_operand: i64) Object.Object {
    switch (operator.type) {
        .Plus => return .{ .Literal = .{ .Integer = .{ .value = left_operand + right_operand } } },
        .Minus => return .{ .Literal = .{ .Integer = .{ .value = left_operand - right_operand } } },
        .Asterisk => return .{ .Literal = .{ .Integer = .{ .value = left_operand * right_operand } } },
        .Slash => return .{ .Literal = .{ .Integer = .{ .value = @divExact(left_operand, right_operand) } } },
        .Equal => return .{ .Literal = .{ .Boolean = .{ .value = left_operand == right_operand } } },
        .NotEqual => return .{ .Literal = .{ .Boolean = .{ .value = left_operand != right_operand } } },
        .LessThan => return .{ .Literal = .{ .Boolean = .{ .value = left_operand < right_operand } } },
        .GreaterThan => return .{ .Literal = .{ .Boolean = .{ .value = left_operand > right_operand } } },
        else => unreachable,
    }
}

fn evalBooleanInfixExpression(operator: Lexer.Token, left_operand: bool, right_operand: bool) Object.Object {
    switch (operator.type) {
        .Equal => return .{ .Literal = .{ .Boolean = .{ .value = left_operand == right_operand } } },
        .NotEqual => return .{ .Literal = .{ .Boolean = .{ .value = left_operand != right_operand } } },
        else => unreachable,
    }
}

fn evalIfExpression(if_expr: *Ast.IfExpression) Object.Object {
    var condition = eval(.{ .Expression = if_expr.condition });

    if (isTruthy(condition)) {
        return evalStatements(if_expr.consequence.statements.items);
    } else if (if_expr.alternative) |alternative| {
        return evalStatements(alternative.statements.items);
    }

    return NULL;
}

fn isTruthy(object: Object.Object) bool {
    switch (object) {
        .Literal => |literal| switch (literal) {
            .Integer => |integer| return integer.value != 0,
            .Boolean => |boolean| return boolean.value,
            else => return false,
        },
        else => return false,
    }
}

test "Eval Integer Expression" {
    const Input = struct { input: []const u8, expected: i64 };
    const input = [_]Input{
        .{ .input = "5", .expected = 5 },
        .{ .input = "10", .expected = 10 },
        .{ .input = "-5", .expected = -5 },
        .{ .input = "-10", .expected = -10 },
        .{ .input = "5 + 5 + 5 + 5 - 10", .expected = 10 },
        .{ .input = "2 * 2 * 2 * 2 * 2", .expected = 32 },
        .{ .input = "-50 + 100 + -50", .expected = 0 },
        .{ .input = "5 * 2 + 10", .expected = 20 },
        .{ .input = "5 + 2 * 10", .expected = 25 },
        .{ .input = "20 + 2 * -10", .expected = 0 },
        .{ .input = "50 / 2 * 2 + 10", .expected = 60 },
        .{ .input = "2 * (5 + 10)", .expected = 30 },
        .{ .input = "3 * 3 * 3 + 10", .expected = 37 },
        .{ .input = "3 * (3 * 3) + 10", .expected = 37 },
        .{ .input = "(5 + 10 * 2 + 15 / 3) * 2 + -10", .expected = 50 },
    };

    for (input) |test_input| {
        var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
        defer arena.deinit();

        var allocator = arena.allocator();

        var lexer = Lexer.init(test_input.input);

        var parser = try Parser.init(&lexer, allocator);
        defer parser.deinit();

        var program = try parser.parseProgram(allocator);
        defer program.deinit();

        var statement = Ast.Statement{ .Program = program };
        var node = Ast.Node{ .Statement = &statement };

        if (parser.errors.items.len > 0) {
            std.debug.print("Parser failed with {d} errors:\n", .{parser.errors.items.len});
            for (parser.errors.items) |message| {
                std.debug.print("- {s}\n", .{message});
            }
            try std.testing.expectEqual(@as(usize, 0), parser.errors.items.len);
        }

        var object = eval(node);

        switch (object) {
            .Literal => |literal| switch (literal) {
                .Integer => |integer| try std.testing.expectEqual(test_input.expected, integer.value),
                else => unreachable,
            },
            else => unreachable,
        }
    }
}

test "Eval Boolean Expression" {
    const Input = struct { input: []const u8, expected: bool };
    const input = [_]Input{
        .{ .input = "true", .expected = true },
        .{ .input = "false", .expected = false },
        .{ .input = "true", .expected = true },
        .{ .input = "false", .expected = false },
        .{ .input = "1 < 2", .expected = true },
        .{ .input = "1 > 2", .expected = false },
        .{ .input = "1 < 1", .expected = false },
        .{ .input = "1 > 1", .expected = false },
        .{ .input = "1 == 1", .expected = true },
        .{ .input = "1 != 1", .expected = false },
        .{ .input = "1 == 2", .expected = false },
        .{ .input = "1 != 2", .expected = true },
        .{ .input = "true == true", .expected = true },
        .{ .input = "false == false", .expected = true },
        .{ .input = "true == false", .expected = false },
        .{ .input = "true != false", .expected = true },
        .{ .input = "false != true", .expected = true },
        .{ .input = "(1 < 2) == true", .expected = true },
        .{ .input = "(1 < 2) == false", .expected = false },
        .{ .input = "(1 > 2) == true", .expected = false },
        .{ .input = "(1 > 2) == false", .expected = true },
    };

    for (input) |test_input| {
        var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
        defer arena.deinit();

        var allocator = arena.allocator();

        var lexer = Lexer.init(test_input.input);

        var parser = try Parser.init(&lexer, allocator);
        defer parser.deinit();

        var program = try parser.parseProgram(allocator);
        defer program.deinit();

        var statement = Ast.Statement{ .Program = program };
        var node = Ast.Node{ .Statement = &statement };

        if (parser.errors.items.len > 0) {
            std.debug.print("Parser failed with {d} errors:\n", .{parser.errors.items.len});
            for (parser.errors.items) |message| {
                std.debug.print("- {s}\n", .{message});
            }
            try std.testing.expectEqual(@as(usize, 0), parser.errors.items.len);
        }

        var object = eval(node);

        switch (object) {
            .Literal => |literal| switch (literal) {
                .Boolean => |boolean| try std.testing.expectEqual(test_input.expected, boolean.value),
                else => unreachable,
            },
            else => unreachable,
        }
    }
}

test "Eval Bang Operator" {
    const Input = struct { input: []const u8, expected: bool };
    const input = [_]Input{
        .{ .input = "!true", .expected = false },
        .{ .input = "!false", .expected = true },
        .{ .input = "!5", .expected = false },
        .{ .input = "!!true", .expected = true },
        .{ .input = "!!false", .expected = false },
        .{ .input = "!!5", .expected = true },
    };

    for (input) |test_input| {
        var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
        defer arena.deinit();

        var allocator = arena.allocator();

        var lexer = Lexer.init(test_input.input);

        var parser = try Parser.init(&lexer, allocator);
        defer parser.deinit();

        var program = try parser.parseProgram(allocator);
        defer program.deinit();

        var statement = Ast.Statement{ .Program = program };
        var node = Ast.Node{ .Statement = &statement };

        if (parser.errors.items.len > 0) {
            std.debug.print("Parser failed with {d} errors:\n", .{parser.errors.items.len});
            for (parser.errors.items) |message| {
                std.debug.print("- {s}\n", .{message});
            }
            try std.testing.expectEqual(@as(usize, 0), parser.errors.items.len);
        }

        var object = eval(node);

        switch (object) {
            .Literal => |literal| switch (literal) {
                .Boolean => |boolean| try std.testing.expectEqual(test_input.expected, boolean.value),
                else => unreachable,
            },
            else => unreachable,
        }
    }
}

test "Eval If Else Expression" {
    const Input = struct { input: []const u8, expected: ?i64 };
    const input = [_]Input{
        .{ .input = "if (true) { 10 }", .expected = 10 },
        .{ .input = "if (false) { 10 }", .expected = null },
        .{ .input = "if (1) { 10 }", .expected = 10 },
        .{ .input = "if (1 < 2) { 10 }", .expected = 10 },
        .{ .input = "if (1 > 2) { 10 }", .expected = null },
        .{ .input = "if (1 > 2) { 10 } else { 20 }", .expected = 20 },
        .{ .input = "if (1 < 2) { 10 } else { 20 }", .expected = 10 },
    };

    for (input) |test_input| {
        var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
        defer arena.deinit();

        var allocator = arena.allocator();

        var lexer = Lexer.init(test_input.input);

        var parser = try Parser.init(&lexer, allocator);
        defer parser.deinit();

        var program = try parser.parseProgram(allocator);
        defer program.deinit();

        var statement = Ast.Statement{ .Program = program };
        var node = Ast.Node{ .Statement = &statement };

        if (parser.errors.items.len > 0) {
            std.debug.print("Parser failed with {d} errors:\n", .{parser.errors.items.len});
            for (parser.errors.items) |message| {
                std.debug.print("- {s}\n", .{message});
            }
            try std.testing.expectEqual(@as(usize, 0), parser.errors.items.len);
        }

        var object = eval(node);

        switch (object) {
            .Literal => |literal| switch (literal) {
                .Integer => |integer| try std.testing.expectEqual(test_input.expected, integer.value),
                .Null => try std.testing.expectEqual(@as(@TypeOf(test_input.expected), null), test_input.expected),
                else => unreachable,
            },
            else => unreachable,
        }
    }
}

test "Eval Return Statement" {
    const Input = struct { input: []const u8, expected: ?i64 };
    const input = [_]Input{
        .{ .input = "return 10;", .expected = 10 },
        .{ .input = "return 10; 9;", .expected = 10 },
        .{ .input = "return 2 * 5; 9;", .expected = 10 },
        .{ .input = "9; return 2 * 5; 9;", .expected = 10 },
        .{
            .input =
            \\if (10 > 1) {
            \\    if (10 > 1) {
            \\        return 10;
            \\    }
            \\    return 1;
            \\}
            ,
            .expected = 10,
        },
    };

    for (input) |test_input| {
        var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
        defer arena.deinit();

        var allocator = arena.allocator();

        var lexer = Lexer.init(test_input.input);

        var parser = try Parser.init(&lexer, allocator);
        defer parser.deinit();

        var program = try parser.parseProgram(allocator);
        defer program.deinit();

        var statement = Ast.Statement{ .Program = program };
        var node = Ast.Node{ .Statement = &statement };

        if (parser.errors.items.len > 0) {
            std.debug.print("Parser failed with {d} errors:\n", .{parser.errors.items.len});
            for (parser.errors.items) |message| {
                std.debug.print("- {s}\n", .{message});
            }
            try std.testing.expectEqual(@as(usize, 0), parser.errors.items.len);
        }

        var object = eval(node);

        switch (object) {
            .Literal => |literal| switch (literal) {
                .Integer => |integer| try std.testing.expectEqual(test_input.expected, integer.value),
                else => unreachable,
            },
            else => unreachable,
        }
    }
}
