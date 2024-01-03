const std = @import("std");

const Evaluator = @This();

const Parser = @import("parser.zig");
const Lexer = @import("lexer.zig");
const Ast = @import("ast.zig");
const Object = @import("object.zig");
const Environment = @import("environment.zig");

const NULL = Object.Object{ .Literal = .{ .Null = .{} } };

arena: std.mem.Allocator,
environment: *Environment,

pub fn init(arena: std.mem.Allocator, environment: *Environment) Evaluator {
    return .{
        .arena = arena,
        .environment = environment,
    };
}

pub fn eval(self: *Evaluator, node: Ast.Node) Object.Object {
    switch (node) {
        .Statement => |statement| {
            switch (statement.*) {
                .Program => |program| {
                    return self.evalProgram(program);
                },
                .ExpressionStatement => |expr_statement| {
                    return self.eval(.{ .Expression = expr_statement.expression });
                },
                .ReturnStatement => |return_statement| {
                    return self.evalReturnStatement(return_statement);
                },
                .LetStatement => |let_statement| {
                    return self.evalLetStatement(let_statement);
                },
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
                .Identifier => |identifier| {
                    return self.evalIdentifier(identifier);
                },
                .PrefixExpression => |prefix_expr| {
                    var operand = self.eval(.{ .Expression = prefix_expr.operand });
                    return self.evalPrefixExpression(prefix_expr.token, operand);
                },
                .InfixExpression => |infix_expr| {
                    var left_operand = self.eval(.{ .Expression = infix_expr.left_operand });
                    var right_operand = self.eval(.{ .Expression = infix_expr.right_operand });
                    return self.evalInfixExpression(infix_expr.token, left_operand, right_operand);
                },
                .IfExpression => |if_expr| {
                    return self.evalIfExpression(if_expr);
                },
                else => unreachable,
            }
        },
    }

    return NULL;
}

fn evalStatements(self: *Evaluator, statements: []*Ast.Statement) Object.Object {
    var result = NULL;

    for (statements) |statement| {
        result = self.eval(.{ .Statement = statement });

        if (result == .ReturnValue or result == .Error) {
            break;
        }
    }

    return result;
}

fn evalProgram(self: *Evaluator, program: *Ast.Program) Object.Object {
    var result = self.evalStatements(program.statements.items);

    if (result == .ReturnValue) {
        return .{ .Literal = result.ReturnValue };
    }

    return result;
}

fn evalReturnStatement(self: *Evaluator, return_statement: *Ast.ReturnStatement) Object.Object {
    var result = self.eval(.{ .Expression = return_statement.return_value });
    switch (result) {
        .Literal => |literal| return .{ .ReturnValue = literal },
        .Error => return result,
        else => unreachable,
    }
}

fn evalLetStatement(self: *Evaluator, let_statement: *Ast.LetStatement) Object.Object {
    var result = self.eval(.{ .Expression = let_statement.value });

    if (result == .Error) {
        return result;
    }

    _ = self.environment.set(let_statement.name.value, result) catch unreachable;

    return result;
}

fn evalIdentifier(self: *Evaluator, identifier: *Ast.Identifier) Object.Object {
    if (self.environment.get(identifier.value)) |object| {
        return object.*;
    }

    return newError(self.arena, "Unknown identifier: '{s}'", .{identifier.value});
}

fn evalPrefixExpression(self: *Evaluator, operator: Lexer.Token, operand: Object.Object) Object.Object {
    switch (operator.type) {
        .Bang => return self.evalBangOperator(operand),
        .Minus => return self.evalNegativeOperator(operand),
        else => return newError(self.arena, "Invalid prefix operator: {s}", .{@tagName(operator.type)}),
    }
}

fn evalBangOperator(self: *Evaluator, operand: Object.Object) Object.Object {
    switch (operand) {
        .Literal => |literal| switch (literal) {
            .Integer => |integer| return .{ .Literal = .{ .Boolean = .{ .value = integer.value == 0 } } },
            .Boolean => |boolean| return .{ .Literal = .{ .Boolean = .{ .value = !boolean.value } } },
            .Null => return NULL,
        },
        .Error => return operand,
        else => return newError(self.arena, "Invalid type. Expected 'Literal' but found '{s}'", .{@tagName(operand)}),
    }
}

fn evalNegativeOperator(self: *Evaluator, operand: Object.Object) Object.Object {
    switch (operand) {
        .Literal => |literal| switch (literal) {
            .Integer => |integer| return .{ .Literal = .{ .Integer = .{ .value = -integer.value } } },
            .Null => return NULL,
            else => return newError(self.arena, "Operand type mismatch. Invalid operation: -'{s}'", .{@tagName(literal)}),
        },
        .Error => return operand,
        else => return newError(self.arena, "Invalid type. Expected 'Literal' but found '{s}'", .{@tagName(operand)}),
    }
}

fn evalInfixExpression(self: *Evaluator, operator: Lexer.Token, left_operand: Object.Object, right_operand: Object.Object) Object.Object {
    switch (left_operand) {
        .Literal => |left_literal| switch (right_operand) {
            .Literal => |right_literal| switch (left_literal) {
                .Integer => |left_integer| {
                    switch (right_literal) {
                        .Integer => |right_integer| return self.evalIntegerInfixExpression(operator, left_integer.value, right_integer.value),
                        .Null => return NULL,
                        else => return newError(self.arena, "Right operand type mismatch. Invalid operation: 'Integer' {s} '{s}'", .{ operator.literal, @tagName(right_literal) }),
                    }
                },
                .Boolean => |left_boolean| {
                    switch (right_literal) {
                        .Boolean => |right_boolean| return self.evalBooleanInfixExpression(operator, left_boolean.value, right_boolean.value),
                        .Null => return NULL,
                        else => return newError(self.arena, "Right operand type mismatch. Invalid operation: 'Boolean' {s} '{s}'", .{ operator.literal, @tagName(right_literal) }),
                    }
                },
                .Null => return NULL,
            },
            .Error => return right_operand,
            else => return newError(self.arena, "Right operand type mismatch. Expected 'Literal' but found '{s}'", .{@tagName(right_operand)}),
        },
        .Error => return left_operand,
        else => return newError(self.arena, "Right operand type mismatch. Expected 'Literal' but found '{s}'", .{@tagName(left_operand)}),
    }
}

fn evalIntegerInfixExpression(self: *Evaluator, operator: Lexer.Token, left_operand: i64, right_operand: i64) Object.Object {
    switch (operator.type) {
        .Plus => return .{ .Literal = .{ .Integer = .{ .value = left_operand + right_operand } } },
        .Minus => return .{ .Literal = .{ .Integer = .{ .value = left_operand - right_operand } } },
        .Asterisk => return .{ .Literal = .{ .Integer = .{ .value = left_operand * right_operand } } },
        .Slash => return .{ .Literal = .{ .Integer = .{ .value = @divExact(left_operand, right_operand) } } },
        .Equal => return .{ .Literal = .{ .Boolean = .{ .value = left_operand == right_operand } } },
        .NotEqual => return .{ .Literal = .{ .Boolean = .{ .value = left_operand != right_operand } } },
        .LessThan => return .{ .Literal = .{ .Boolean = .{ .value = left_operand < right_operand } } },
        .GreaterThan => return .{ .Literal = .{ .Boolean = .{ .value = left_operand > right_operand } } },
        else => return newError(self.arena, "Invalid operation: 'Integer' {s} 'Integer'", .{operator.literal}),
    }
}

fn evalBooleanInfixExpression(self: *Evaluator, operator: Lexer.Token, left_operand: bool, right_operand: bool) Object.Object {
    switch (operator.type) {
        .Equal => return .{ .Literal = .{ .Boolean = .{ .value = left_operand == right_operand } } },
        .NotEqual => return .{ .Literal = .{ .Boolean = .{ .value = left_operand != right_operand } } },
        else => return newError(self.arena, "Invalid operation: 'Boolean' {s} 'Boolean'", .{operator.literal}),
    }
}

fn evalIfExpression(self: *Evaluator, if_expr: *Ast.IfExpression) Object.Object {
    var condition = self.eval(.{ .Expression = if_expr.condition });

    if (condition == .Error) {
        return condition;
    }

    if (isTruthy(condition)) {
        return self.evalStatements(if_expr.consequence.statements.items);
    } else if (if_expr.alternative) |alternative| {
        return self.evalStatements(alternative.statements.items);
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

fn newError(arena: std.mem.Allocator, comptime message: []const u8, args: anytype) Object.Object {
    var buffer = std.ArrayList(u8).initCapacity(arena, message.len) catch unreachable;
    var writer = buffer.writer();
    std.fmt.format(writer, message, args) catch unreachable;
    return .{ .Error = .{ .value = buffer.toOwnedSlice() catch unreachable } };
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

        var environment = Environment.init(allocator);
        defer environment.deinit();

        var evaluator = Evaluator.init(allocator, &environment);
        var object = evaluator.eval(node);

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

        var environment = Environment.init(allocator);
        defer environment.deinit();

        var evaluator = Evaluator.init(allocator, &environment);
        var object = evaluator.eval(node);

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

        var environment = Environment.init(allocator);
        defer environment.deinit();

        var evaluator = Evaluator.init(allocator, &environment);
        var object = evaluator.eval(node);

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

        var environment = Environment.init(allocator);
        defer environment.deinit();

        var evaluator = Evaluator.init(allocator, &environment);
        var object = evaluator.eval(node);

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

        var environment = Environment.init(allocator);
        defer environment.deinit();

        var evaluator = Evaluator.init(allocator, &environment);
        var object = evaluator.eval(node);

        switch (object) {
            .Literal => |literal| switch (literal) {
                .Integer => |integer| try std.testing.expectEqual(test_input.expected, integer.value),
                else => unreachable,
            },
            else => unreachable,
        }
    }
}

test "Eval Error Handling" {
    const Input = struct { input: []const u8, expected: []const u8 };
    const input = [_]Input{
        .{ .input = "5 + true;", .expected = "Right operand type mismatch. Invalid operation: 'Integer' + 'Boolean'" },
        .{ .input = "5 + true; 5;", .expected = "Right operand type mismatch. Invalid operation: 'Integer' + 'Boolean'" },
        .{ .input = "-true", .expected = "Operand type mismatch. Invalid operation: -'Boolean'" },
        .{ .input = "true + false;", .expected = "Invalid operation: 'Boolean' + 'Boolean'" },
        .{ .input = "5; true + false; 5", .expected = "Invalid operation: 'Boolean' + 'Boolean'" },
        .{ .input = "if (10 > 1) { true + false; }", .expected = "Invalid operation: 'Boolean' + 'Boolean'" },
        .{
            .input =
            \\if (10 > 1) {
            \\    if (10 > 1) {
            \\        return true + false;
            \\    } return 1;
            \\}
            ,
            .expected = "Invalid operation: 'Boolean' + 'Boolean'",
        },
        .{ .input = "foobar", .expected = "Unknown identifier: 'foobar'" },
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

        var environment = Environment.init(allocator);
        defer environment.deinit();

        var evaluator = Evaluator.init(allocator, &environment);
        var object = evaluator.eval(node);

        switch (object) {
            .Error => |err| try std.testing.expectEqualStrings(test_input.expected, err.value),
            else => unreachable,
        }
    }
}

test "Eval Let Statement" {
    const Input = struct { input: []const u8, expected: i64 };
    const input = [_]Input{
        .{ .input = "let a = 5; a;", .expected = 5 },
        .{ .input = "let a = 5 * 5; a;", .expected = 25 },
        .{ .input = "let a = 5; let b = a; b;", .expected = 5 },
        .{ .input = "let a = 5; let b = a; let c = a + b + 5; c;", .expected = 15 },
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

        var environment = Environment.init(allocator);
        defer environment.deinit();

        var evaluator = Evaluator.init(allocator, &environment);
        var object = evaluator.eval(node);

        switch (object) {
            .Literal => |literal| switch (literal) {
                .Integer => |integer| try std.testing.expectEqual(test_input.expected, integer.value),
                else => unreachable,
            },
            else => unreachable,
        }
    }
}
