const std = @import("std");

const Parser = @import("parser.zig");
const Lexer = @import("lexer.zig");
const Ast = @import("ast.zig");

const Object = @import("object.zig");

pub fn eval(node: Ast.Node) Object.Object {
    switch (node) {
        .Statement => |statement| {
            switch (statement.*) {
                .Program => |program| {
                    return evalStatements(program.statements.items);
                },
                .ExpressionStatement => |expr_statement| {
                    return eval(.{ .Expression = expr_statement.expression });
                },
                else => unreachable,
            }
        },
        .Expression => |expression| {
            switch (expression.*) {
                .Integer => |integer| {
                    return .{ .Integer = .{ .value = integer.value } };
                },
                .Boolean => |boolean| {
                    return .{ .Boolean = .{ .value = boolean.value } };
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
                else => unreachable,
            }
        },
    }

    return .{ .Null = .{} };
}

fn evalStatements(statements: []*Ast.Statement) Object.Object {
    var result: Object.Object = undefined;

    for (statements) |statement| {
        result = eval(.{ .Statement = statement });
    }

    return result;
}

fn evalPrefixExpression(operator: Lexer.Token, operand: Object.Object) Object.Object {
    switch (operator.type) {
        .Bang => return evalBangOperator(operand),
        .Minus => return evalNegativeOperator(operand),
        inline else => unreachable,
    }
}

fn evalInfixExpression(operator: Lexer.Token, left_operand: Object.Object, right_operand: Object.Object) Object.Object {
    if (left_operand == .Integer and right_operand == .Integer) {
        return evalIntegerInfixExpression(operator, left_operand, right_operand);
    }
    if (left_operand == .Boolean and right_operand == .Boolean) {
        return evalBooleanInfixExpression(operator, left_operand, right_operand);
    }

    return .{ .Null = .{} };
}

fn evalIntegerInfixExpression(operator: Lexer.Token, left_operand: Object.Object, right_operand: Object.Object) Object.Object {
    switch (operator.type) {
        .Plus => return .{ .Integer = .{ .value = left_operand.Integer.value + right_operand.Integer.value } },
        .Minus => return .{ .Integer = .{ .value = left_operand.Integer.value - right_operand.Integer.value } },
        .Asterisk => return .{ .Integer = .{ .value = left_operand.Integer.value * right_operand.Integer.value } },
        .Slash => return .{ .Integer = .{ .value = @divExact(left_operand.Integer.value, right_operand.Integer.value) } },
        .Equal => return .{ .Boolean = .{ .value = left_operand.Integer.value == right_operand.Integer.value } },
        .NotEqual => return .{ .Boolean = .{ .value = left_operand.Integer.value != right_operand.Integer.value } },
        .LessThan => return .{ .Boolean = .{ .value = left_operand.Integer.value < right_operand.Integer.value } },
        .GreaterThan => return .{ .Boolean = .{ .value = left_operand.Integer.value > right_operand.Integer.value } },
        inline else => unreachable,
    }
}

fn evalBooleanInfixExpression(operator: Lexer.Token, left_operand: Object.Object, right_operand: Object.Object) Object.Object {
    switch (operator.type) {
        .Equal => return .{ .Boolean = .{ .value = left_operand.Boolean.value == right_operand.Boolean.value } },
        .NotEqual => return .{ .Boolean = .{ .value = left_operand.Boolean.value != right_operand.Boolean.value } },
        inline else => unreachable,
    }
}

fn evalBangOperator(operand: Object.Object) Object.Object {
    switch (operand) {
        .Boolean => |boolean| return .{ .Boolean = .{ .value = !boolean.value } },
        .Integer => |integer| return .{ .Boolean = .{ .value = integer.value == 0 } },
        .Null => return .{ .Null = .{} },
    }
}

fn evalNegativeOperator(operand: Object.Object) Object.Object {
    switch (operand) {
        .Integer => return .{ .Integer = .{ .value = -operand.Integer.value } },
        inline else => return .{ .Null = .{} },
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
            .Integer => |integer| try std.testing.expectEqual(test_input.expected, integer.value),
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
            .Boolean => |boolean| try std.testing.expectEqual(test_input.expected, boolean.value),
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
            .Boolean => |boolean| try std.testing.expectEqual(test_input.expected, boolean.value),
            else => unreachable,
        }
    }
}
