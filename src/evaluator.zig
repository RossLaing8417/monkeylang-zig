const std = @import("std");

const Evaluator = @This();

const Parser = @import("parser.zig");
const Lexer = @import("lexer.zig");
const Ast = @import("ast.zig");
const Object = @import("object.zig");
const Environment = @import("environment.zig");
const Builtin = @import("builtin.zig");

const Container = Object.Container;
const Error = std.mem.Allocator.Error || std.io.AnyWriter.Error;

const NULL = Container{ .Value = .{ .Null = .{} } };

allocator: std.mem.Allocator,

pub var indent: usize = 0;

fn debugEval(node: Ast.Node) !void {
    var buffer = try std.BoundedArray(u8, 4096).init(0);
    const writer = buffer.writer();
    try writer.writeByteNTimes(' ', indent * 2);
    try writer.print("EVAL {s}: ", .{@tagName(node)});
    try node.write(writer.any(), .none);
    std.mem.replaceScalar(u8, buffer.slice(), '\n', ' ');
    try writer.writeByte('\n');
    std.debug.print("{s}", .{buffer.slice()});
}

fn debugResult(result: Container, messsage: []const u8) !void {
    var buffer = try std.BoundedArray(u8, 4096).init(0);
    const writer = buffer.writer();
    try writer.writeByteNTimes(' ', indent * 2);
    try writer.print("- {s} result: ", .{messsage});
    try result.inspect(writer.any());
    std.mem.replaceScalar(u8, buffer.slice(), '\n', ' ');
    try writer.writeByte('\n');
    std.debug.print("{s}", .{buffer.slice()});
}

pub fn init(allocator: std.mem.Allocator) !*Evaluator {
    const evaluator = try allocator.create(Evaluator);
    evaluator.* = .{
        .allocator = allocator,
    };
    return evaluator;
}

pub fn deinit(self: *Evaluator) void {
    self.allocator.destroy(self);
}

pub fn evalAst(self: *Evaluator, ast: *Ast, environment: *Environment) !Container {
    return unwrapReturn(try self.evalNodes(ast.nodes, environment));
}

pub fn evalNodes(self: *Evaluator, nodes: []const Ast.Node, environment: *Environment) !Container {
    var result = NULL;
    for (nodes) |node| {
        result.deinit(self.allocator);
        result = try self.eval(node, environment);
        if (result == .ReturnValue or result == .Error) {
            break;
        }
    }
    return result;
}

pub fn eval(self: *Evaluator, node: Ast.Node, environment: *Environment) Error!Container {
    // try debugEval(node);
    // indent += 1;
    // defer indent -= 1;
    var result: Container = switch (node) {
        // Statements

        .LetStatement => |let_statement| try self.evalLetStatement(let_statement, environment),
        .ReturnStatement => |return_statement| try self.evalReturnStatement(return_statement, environment),
        .ExpressionStatement => |expr_statement| try self.eval(expr_statement.expression, environment),

        .BlockStatement => |block_statement| blk: {
            var result = try self.evalNodes(block_statement.statements, environment);
            switch (result) {
                .Value,
                .ReturnValue,
                => |value| if (value.getNode()) |blk_node| {
                    switch (blk_node) {
                        .Identifier => break :blk try result.copy(self.allocator),
                        else => {},
                    }
                },
                else => {},
            }
            break :blk result;
        },

        // Expressions

        .Identifier => |identifier| try self.evalIdentifier(identifier, environment),
        .Integer => |integer| .{ .Value = .{ .Integer = .{ .value = integer.value } } },
        .Boolean => |boolean| .{ .Value = .{ .Boolean = .{ .value = boolean.value } } },
        .String => |string| .{ .Value = .{ .String = .{ .value = string.value, .owned = false } } },

        .FunctionLiteral => |function_literal| blk: {
            environment.incRef();
            break :blk .{ .Value = .{ .Function = .{
                .parameters = function_literal.parameters,
                .body = function_literal.body,
                .environment = environment,
            } } };
        },
        .ArrayLiteral => |array_literal| try self.evalArrayLiteral(array_literal.elements, environment),

        .PrefixExpression => |prefix_expr| try self.evalPrefixExpression(
            prefix_expr.token,
            try self.eval(prefix_expr.operand, environment),
        ),
        .InfixExpression => |infix_expr| blk: {
            var left_operand = try self.eval(infix_expr.left_operand, environment);
            var right_operand = try self.eval(infix_expr.right_operand, environment);
            defer {
                if (infix_expr.left_operand != .Identifier) left_operand.deinit(self.allocator);
                if (infix_expr.right_operand != .Identifier) right_operand.deinit(self.allocator);
            }
            break :blk try self.evalInfixExpression(
                infix_expr.token,
                left_operand,
                right_operand,
            );
        },
        .GroupedExpression => |grouped_expr| try self.eval(grouped_expr.expression, environment),
        .IfExpression => |if_expr| try self.evalIfExpression(if_expr, environment),
        .CallExpression => |call_expr| try self.evalCallExpression(call_expr, environment),
        .IndexExpression => |index_expr| try self.evalIndexExpression(index_expr, environment),
    };
    switch (node) {
        .Identifier => {
            switch (result) {
                .Value,
                .ReturnValue,
                => |*value| value.setNode(node),
                else => {},
            }
        },
        else => {},
    }
    return result;
}

fn evalLetStatement(self: *Evaluator, let_statement: *const Ast.LetStatement, environment: *Environment) !Container {
    const result: Container = blk: {
        var result = try self.eval(let_statement.value, environment);
        if (result == .Error) {
            break :blk result;
        }
        std.debug.assert(result == .Value);
        var value = switch (let_statement.value) {
            .Integer,
            .Boolean,
            .String,
            .FunctionLiteral,
            => result,
            else => val_blk: {
                defer result.deinit(self.allocator);
                break :val_blk try result.copy(self.allocator);
            },
        };
        try environment.set(let_statement.name.value, value.Value);
        break :blk try value.copy(self.allocator);
    };
    // try debugResult(result, "LetStatement");
    return result;
}

fn evalReturnStatement(self: *Evaluator, return_statement: *const Ast.ReturnStatement, environment: *Environment) !Container {
    const result: Container = blk: {
        const result = try self.eval(return_statement.return_value, environment);
        switch (result) {
            .Value => |literal| break :blk .{ .ReturnValue = literal },
            .Error => break :blk result,
            else => unreachable,
        }
    };
    // try debugResult(result, "ReturnStatement");
    return result;
}

fn evalIdentifier(self: *Evaluator, identifier: *const Ast.Identifier, environment: *Environment) !Container {
    const result: Container = blk: {
        if (environment.get(identifier.value)) |object| {
            break :blk .{ .Value = object.* };
        }

        if (Builtin.FunctionMap.get(identifier.value)) |builtin| {
            break :blk .{ .BuiltinFunction = builtin };
        }

        break :blk try self.evalError("Unknown identifier: '{s}'", .{identifier.value});
    };
    // try debugResult(result, "Identifier");
    return result;
}

fn evalArrayLiteral(self: *Evaluator, elements: []const Ast.Node, environment: *Environment) !Container {
    var results = try std.ArrayList(Object.Value).initCapacity(self.allocator, elements.len);
    defer {
        for (results.items) |*result| {
            result.deinit(self.allocator);
        }
        results.deinit();
    }

    for (elements) |element| {
        const result = try self.eval(element, environment);
        if (result == .Error) {
            return result;
        }
        std.debug.assert(result == .Value);
        try results.append(result.Value);
    }

    const result: Container = .{ .Value = .{ .Array = .{ .values = try results.toOwnedSlice() } } };
    // try debugResult(result, "ArrayLiteral");
    return result;
}

fn evalPrefixExpression(self: *Evaluator, operator: Lexer.Token, operand: Container) !Container {
    const result = switch (operator.type) {
        .Bang => try self.evalBangOperator(operand),
        .Minus => try self.evalNegativeOperator(operand),
        else => try self.evalError("Invalid prefix operator: {s}", .{@tagName(operator.type)}),
    };
    // try debugResult(result, "PrefixExpression");
    return result;
}

fn evalBangOperator(self: *Evaluator, operand: Container) !Container {
    const result: Container = switch (operand) {
        .Value => |literal| switch (literal) {
            .Integer => |integer| .{ .Value = .{ .Boolean = .{ .value = integer.value == 0 } } },
            .Boolean => |boolean| .{ .Value = .{ .Boolean = .{ .value = !boolean.value } } },
            .Null => NULL,
            else => try self.evalError("Invalid literal '{s}' for operand", .{@tagName(literal)}),
        },
        .Error => operand,
        else => try self.evalError("Invalid type. Expected 'Value' but found '{s}'", .{@tagName(operand)}),
    };
    return result;
}

fn evalNegativeOperator(self: *Evaluator, operand: Container) !Container {
    const result: Container = switch (operand) {
        .Value => |literal| switch (literal) {
            .Integer => |integer| .{ .Value = .{ .Integer = .{ .value = -integer.value } } },
            .Null => NULL,
            else => try self.evalError("Operand type mismatch. Invalid operation: -'{s}'", .{@tagName(literal)}),
        },
        .Error => operand,
        else => try self.evalError("Invalid type. Expected 'Value' but found '{s}'", .{@tagName(operand)}),
    };
    return result;
}

fn evalInfixExpression(self: *Evaluator, operator: Lexer.Token, left_operand: Container, right_operand: Container) !Container {
    const result: Container = switch (left_operand) {
        .Value => |left_literal| switch (right_operand) {
            .Value => |right_literal| switch (left_literal) {
                .Integer => |left_integer| switch (right_literal) {
                    .Integer => |right_integer| try self.evalIntegerInfixExpression(
                        operator,
                        left_integer.value,
                        right_integer.value,
                    ),
                    .Null => NULL,
                    else => try self.evalError(
                        "Right operand type mismatch. Invalid operation: 'Integer' {s} '{s}'",
                        .{ operator.literal, @tagName(right_literal) },
                    ),
                },
                .Boolean => |left_boolean| switch (right_literal) {
                    .Boolean => |right_boolean| try self.evalBooleanInfixExpression(
                        operator,
                        left_boolean.value,
                        right_boolean.value,
                    ),
                    .Null => return NULL,
                    else => return try self.evalError(
                        "Right operand type mismatch. Invalid operation: 'Boolean' {s} '{s}'",
                        .{ operator.literal, @tagName(right_literal) },
                    ),
                },
                .String => |left_string| switch (right_literal) {
                    .String => |right_string| try self.evalStringInfixExpression(
                        operator,
                        left_string.value,
                        right_string.value,
                    ),
                    else => try self.evalError(
                        "Right operand type mismatch. Invalid operation: 'String' {s} '{s}'",
                        .{ operator.literal, @tagName(right_literal) },
                    ),
                },
                .Null => NULL,
                else => try self.evalError(
                    "Invalid literal '{s}' for left operand",
                    .{@tagName(left_literal)},
                ),
            },
            .Error => right_operand,
            else => try self.evalError(
                "Right operand type mismatch. Expected 'Value' but found '{s}'",
                .{@tagName(right_operand)},
            ),
        },
        .Error => left_operand,
        else => try self.evalError(
            "Right operand type mismatch. Expected 'Value' but found '{s}'",
            .{@tagName(left_operand)},
        ),
    };
    // try debugResult(result, "InfixExpression");
    return result;
}

fn evalIntegerInfixExpression(self: *Evaluator, operator: Lexer.Token, left_operand: i64, right_operand: i64) !Container {
    const result: Container = switch (operator.type) {
        .Plus => .{ .Value = .{ .Integer = .{ .value = left_operand + right_operand } } },
        .Minus => .{ .Value = .{ .Integer = .{ .value = left_operand - right_operand } } },
        .Asterisk => .{ .Value = .{ .Integer = .{ .value = left_operand * right_operand } } },
        .Slash => .{ .Value = .{ .Integer = .{ .value = @divExact(left_operand, right_operand) } } },
        .Equal => .{ .Value = .{ .Boolean = .{ .value = left_operand == right_operand } } },
        .NotEqual => .{ .Value = .{ .Boolean = .{ .value = left_operand != right_operand } } },
        .LessThan => .{ .Value = .{ .Boolean = .{ .value = left_operand < right_operand } } },
        .GreaterThan => .{ .Value = .{ .Boolean = .{ .value = left_operand > right_operand } } },
        else => try self.evalError("Invalid operation: 'Integer' {s} 'Integer'", .{operator.literal}),
    };
    return result;
}

fn evalBooleanInfixExpression(self: *Evaluator, operator: Lexer.Token, left_operand: bool, right_operand: bool) !Container {
    const result: Container = switch (operator.type) {
        .Equal => .{ .Value = .{ .Boolean = .{ .value = left_operand == right_operand } } },
        .NotEqual => .{ .Value = .{ .Boolean = .{ .value = left_operand != right_operand } } },
        else => try self.evalError("Invalid operation: 'Boolean' {s} 'Boolean'", .{operator.literal}),
    };
    return result;
}

fn evalStringInfixExpression(self: *Evaluator, operator: Lexer.Token, left_operand: []const u8, right_operand: []const u8) !Container {
    const result: Container = switch (operator.type) {
        .Plus => .{ .Value = .{ .String = try Object.String.initMerge(self.allocator, left_operand, right_operand) } },
        .Equal => .{ .Value = .{ .Boolean = .{ .value = std.mem.eql(u8, left_operand, right_operand) } } },
        .NotEqual => .{ .Value = .{ .Boolean = .{ .value = !std.mem.eql(u8, left_operand, right_operand) } } },
        else => try self.evalError("Invalid operation: 'String' {s} 'String'", .{operator.literal}),
    };
    return result;
}

fn evalIfExpression(self: *Evaluator, if_expr: *const Ast.IfExpression, environment: *Environment) !Container {
    const result: Container = blk: {
        const condition = try self.eval(if_expr.condition, environment);
        if (condition == .Error) {
            break :blk condition;
        }

        if (isTruthy(condition)) {
            break :blk try self.eval(if_expr.consequence, environment);
        } else if (if_expr.alternative) |alternative| {
            break :blk try self.eval(alternative, environment);
        }

        break :blk NULL;
    };
    // try debugResult(result, "IfExpression");
    return result;
}

fn evalCallExpression(self: *Evaluator, call_expr: *const Ast.CallExpression, environment: *Environment) !Container {
    var result: Container = blk: {
        var function = fn_blk: {
            var result = try self.eval(call_expr.function, environment);
            if (result == .Error) {
                break :blk result;
            }

            if (result == .BuiltinFunction) {
                break :blk try self.evalBuiltinFunction(result.BuiltinFunction, call_expr.arguments, environment);
            }

            std.debug.assert(result != .ReturnValue);

            if (result.Value != .Function) {
                defer result.deinit(self.allocator);
                break :blk try self.evalError(
                    "Call expression type mismatch. Expected 'Function' but found '{s}'",
                    .{@tagName(result.Value)},
                );
            }

            break :fn_blk result.Value.Function;
        };

        defer if (call_expr.function != .Identifier) {
            function.deinit(self.allocator);
        };

        if (call_expr.arguments.len != function.parameters.len) {
            break :blk try self.evalError(
                "Call expression signature mismatch. Expected {d} arguments but found {d}",
                .{ function.parameters.len, call_expr.arguments.len },
            );
        }

        var enclosed_env = try Environment.initEnclosed(self.allocator, function.environment);
        defer enclosed_env.decRef();

        for (function.parameters, call_expr.arguments) |param, arg| {
            const result = try self.eval(arg, environment);
            if (result == .Error) {
                break :blk result;
            }
            std.debug.assert(result == .Value);
            try enclosed_env.set(param.value, switch (result) {
                .Value => |value| if (value.getNode()) |node| switch (node) {
                    .Identifier => try result.Value.copy(self.allocator),
                    else => result.Value,
                } else result.Value,
                else => result.Value,
            });
        }

        break :blk unwrapReturn(try self.eval(.{ .BlockStatement = function.body }, enclosed_env));
    };
    defer result.deinit(self.allocator);
    // try debugResult(result, "CallExpression");
    return result.copy(self.allocator);
}

fn evalIndexExpression(self: *Evaluator, index_expr: *const Ast.IndexExpression, environment: *Environment) !Container {
    const result: Container = blk: {
        var array = arr_blk: {
            var result = try self.eval(index_expr.expression, environment);
            if (result == .Error) {
                break :blk result;
            }

            std.debug.assert(result != .ReturnValue);

            if (result.Value != .Array) {
                defer result.deinit(self.allocator);
                break :blk try self.evalError(
                    "Index expression type mismatch. Expected 'Array' but found '{s}'",
                    .{@tagName(result.Value)},
                );
            }

            break :arr_blk result.Value.Array;
        };

        defer if (index_expr.expression != .Identifier) {
            array.deinit(self.allocator);
        };

        const index = idx_blk: {
            var result = try self.eval(index_expr.index, environment);
            if (result == .Error) {
                break :blk result;
            }

            std.debug.assert(result == .Value);

            if (result.Value != .Integer) {
                defer result.deinit(self.allocator);
                break :blk try self.evalError(
                    "Index expression type mismatch. Expected 'Integer' but found '{s}'",
                    .{@tagName(result)},
                );
            }

            if (result.Value.Integer.value < 0 or result.Value.Integer.value >= array.values.len) {
                break :blk try self.evalError(
                    "Index {d} out of bounds for array of length {d}",
                    .{ result.Value.Integer.value, array.values.len },
                );
            }

            break :idx_blk result.Value.Integer;
        };

        break :blk .{ .Value = try array.values[@intCast(index.value)].copy(self.allocator) };
    };
    // try debugResult(result, "IndexExpression");
    return result;
}

fn evalBuiltinFunction(self: *Evaluator, builtin: Object.BuiltinFunction, arguments: []const Ast.Node, environment: *Environment) !Container {
    const result: Container = blk: {
        const args = try self.allocator.alloc(Container, arguments.len);
        defer {
            for (args, arguments) |*arg, argument| {
                if (argument != .Identifier) {
                    arg.deinit(self.allocator);
                }
            }
            self.allocator.free(args);
        }

        for (args, arguments) |*arg, argument| {
            const result = try self.eval(argument, environment);
            if (result == .Error) {
                break :blk result;
            }
            arg.* = result;
        }

        break :blk try builtin.func(self, args);
    };
    // try debugResult(result, "BuiltinFunction");
    return result;
}

fn isTruthy(object: Container) bool {
    switch (object) {
        .Value => |literal| switch (literal) {
            .Integer => |integer| return integer.value != 0,
            .Boolean => |boolean| return boolean.value,
            else => return false,
        },
        else => return false,
    }
}

pub fn evalError(self: *Evaluator, comptime message: []const u8, args: anytype) !Container {
    return .{ .Error = .{ .value = try std.fmt.allocPrint(self.allocator, message, args) } };
}

fn unwrapReturn(value: Container) Container {
    return switch (value) {
        .ReturnValue => |return_value| .{ .Value = return_value },
        else => value,
    };
}

const TestInput = struct {
    input: []const u8,
    expected: Container,
};

test "Eval Integer Expression" {
    const input = [_]TestInput{
        .{ .input = "5", .expected = .{ .Value = .{ .Integer = .{ .value = 5 } } } },
        .{ .input = "10", .expected = .{ .Value = .{ .Integer = .{ .value = 10 } } } },
        .{ .input = "-5", .expected = .{ .Value = .{ .Integer = .{ .value = -5 } } } },
        .{ .input = "-10", .expected = .{ .Value = .{ .Integer = .{ .value = -10 } } } },
        .{ .input = "5 + 5 + 5 + 5 - 10", .expected = .{ .Value = .{ .Integer = .{ .value = 10 } } } },
        .{ .input = "2 * 2 * 2 * 2 * 2", .expected = .{ .Value = .{ .Integer = .{ .value = 32 } } } },
        .{ .input = "-50 + 100 + -50", .expected = .{ .Value = .{ .Integer = .{ .value = 0 } } } },
        .{ .input = "5 * 2 + 10", .expected = .{ .Value = .{ .Integer = .{ .value = 20 } } } },
        .{ .input = "5 + 2 * 10", .expected = .{ .Value = .{ .Integer = .{ .value = 25 } } } },
        .{ .input = "20 + 2 * -10", .expected = .{ .Value = .{ .Integer = .{ .value = 0 } } } },
        .{ .input = "50 / 2 * 2 + 10", .expected = .{ .Value = .{ .Integer = .{ .value = 60 } } } },
        .{ .input = "2 * (5 + 10)", .expected = .{ .Value = .{ .Integer = .{ .value = 30 } } } },
        .{ .input = "3 * 3 * 3 + 10", .expected = .{ .Value = .{ .Integer = .{ .value = 37 } } } },
        .{ .input = "3 * (3 * 3) + 10", .expected = .{ .Value = .{ .Integer = .{ .value = 37 } } } },
        .{ .input = "(5 + 10 * 2 + 15 / 3) * 2 + -10", .expected = .{ .Value = .{ .Integer = .{ .value = 50 } } } },
    };

    const allocator = std.testing.allocator;
    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    for (input) |test_input| {
        var ast = try Ast.parse(allocator, test_input.input);
        defer ast.deinit(allocator);

        try testAst(&ast);

        var environment = try Environment.init(allocator);
        defer environment.decRef();

        var result = try evaluator.evalAst(&ast, environment);
        defer result.deinit(allocator);
        try expectEqualObjects(test_input.expected, result);
    }
}

test "Eval Boolean Expression" {
    const input = [_]TestInput{
        .{ .input = "true", .expected = .{ .Value = .{ .Boolean = .{ .value = true } } } },
        .{ .input = "false", .expected = .{ .Value = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "true", .expected = .{ .Value = .{ .Boolean = .{ .value = true } } } },
        .{ .input = "false", .expected = .{ .Value = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "1 < 2", .expected = .{ .Value = .{ .Boolean = .{ .value = true } } } },
        .{ .input = "1 > 2", .expected = .{ .Value = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "1 < 1", .expected = .{ .Value = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "1 > 1", .expected = .{ .Value = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "1 == 1", .expected = .{ .Value = .{ .Boolean = .{ .value = true } } } },
        .{ .input = "1 != 1", .expected = .{ .Value = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "1 == 2", .expected = .{ .Value = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "1 != 2", .expected = .{ .Value = .{ .Boolean = .{ .value = true } } } },
        .{ .input = "true == true", .expected = .{ .Value = .{ .Boolean = .{ .value = true } } } },
        .{ .input = "false == false", .expected = .{ .Value = .{ .Boolean = .{ .value = true } } } },
        .{ .input = "true == false", .expected = .{ .Value = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "true != false", .expected = .{ .Value = .{ .Boolean = .{ .value = true } } } },
        .{ .input = "false != true", .expected = .{ .Value = .{ .Boolean = .{ .value = true } } } },
        .{ .input = "(1 < 2) == true", .expected = .{ .Value = .{ .Boolean = .{ .value = true } } } },
        .{ .input = "(1 < 2) == false", .expected = .{ .Value = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "(1 > 2) == true", .expected = .{ .Value = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "(1 > 2) == false", .expected = .{ .Value = .{ .Boolean = .{ .value = true } } } },
    };

    const allocator = std.testing.allocator;
    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    for (input) |test_input| {
        var ast = try Ast.parse(allocator, test_input.input);
        defer ast.deinit(allocator);

        try testAst(&ast);

        var environment = try Environment.init(allocator);
        defer environment.decRef();

        var result = try evaluator.evalAst(&ast, environment);
        defer result.deinit(allocator);
        try expectEqualObjects(test_input.expected, result);
    }
}

test "Eval Bang Operator" {
    const input = [_]TestInput{
        .{ .input = "!true", .expected = .{ .Value = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "!false", .expected = .{ .Value = .{ .Boolean = .{ .value = true } } } },
        .{ .input = "!5", .expected = .{ .Value = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "!!true", .expected = .{ .Value = .{ .Boolean = .{ .value = true } } } },
        .{ .input = "!!false", .expected = .{ .Value = .{ .Boolean = .{ .value = false } } } },
        .{ .input = "!!5", .expected = .{ .Value = .{ .Boolean = .{ .value = true } } } },
    };

    const allocator = std.testing.allocator;
    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    for (input) |test_input| {
        var ast = try Ast.parse(allocator, test_input.input);
        defer ast.deinit(allocator);

        try testAst(&ast);

        var environment = try Environment.init(allocator);
        defer environment.decRef();

        var result = try evaluator.evalAst(&ast, environment);
        defer result.deinit(allocator);
        try expectEqualObjects(test_input.expected, result);
    }
}

test "Eval Strings" {
    const input = [_]TestInput{
        .{
            .input =
            \\"Hello World!"
            ,
            .expected = .{ .Value = .{ .String = .{ .value = "Hello World!", .owned = false } } },
        },
        .{
            .input =
            \\"Hello" + " " + "World!"
            ,
            .expected = .{ .Value = .{ .String = .{ .value = "Hello World!", .owned = false } } },
        },
        .{
            .input =
            \\"Hello" != "World!"
            ,
            .expected = .{ .Value = .{ .Boolean = .{ .value = true } } },
        },
    };

    const allocator = std.testing.allocator;
    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    for (input) |test_input| {
        var ast = try Ast.parse(allocator, test_input.input);
        defer ast.deinit(allocator);

        try testAst(&ast);

        var environment = try Environment.init(allocator);
        defer environment.decRef();

        var result = try evaluator.evalAst(&ast, environment);
        defer result.deinit(allocator);
        try expectEqualObjects(test_input.expected, result);
    }
}

test "Eval If Else Expression" {
    const input = [_]TestInput{
        .{ .input = "if (true) { 10 }", .expected = .{ .Value = .{ .Integer = .{ .value = 10 } } } },
        .{ .input = "if (false) { 10 }", .expected = .{ .Value = .{ .Null = .{} } } },
        .{ .input = "if (1) { 10 }", .expected = .{ .Value = .{ .Integer = .{ .value = 10 } } } },
        .{ .input = "if (1 < 2) { 10 }", .expected = .{ .Value = .{ .Integer = .{ .value = 10 } } } },
        .{ .input = "if (1 > 2) { 10 }", .expected = .{ .Value = .{ .Null = .{} } } },
        .{ .input = "if (1 > 2) { 10 } else { 20 }", .expected = .{ .Value = .{ .Integer = .{ .value = 20 } } } },
        .{ .input = "if (1 < 2) { 10 } else { 20 }", .expected = .{ .Value = .{ .Integer = .{ .value = 10 } } } },
    };

    const allocator = std.testing.allocator;
    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    for (input) |test_input| {
        var ast = try Ast.parse(allocator, test_input.input);
        defer ast.deinit(allocator);

        try testAst(&ast);

        var environment = try Environment.init(allocator);
        defer environment.decRef();

        var result = try evaluator.evalAst(&ast, environment);
        defer result.deinit(allocator);
        try expectEqualObjects(test_input.expected, result);
    }
}

test "Eval Return Statement" {
    const input = [_]TestInput{
        .{ .input = "return 10;", .expected = .{ .Value = .{ .Integer = .{ .value = 10 } } } },
        .{ .input = "return 10; 9;", .expected = .{ .Value = .{ .Integer = .{ .value = 10 } } } },
        .{ .input = "return 2 * 5; 9;", .expected = .{ .Value = .{ .Integer = .{ .value = 10 } } } },
        .{ .input = "9; return 2 * 5; 9;", .expected = .{ .Value = .{ .Integer = .{ .value = 10 } } } },
        .{
            .input =
            \\if (10 > 1) {
            \\    if (10 > 1) {
            \\        return 10;
            \\    }
            \\    return 1;
            \\}
            ,
            .expected = .{ .Value = .{ .Integer = .{ .value = 10 } } },
        },
    };

    const allocator = std.testing.allocator;
    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    for (input) |test_input| {
        var ast = try Ast.parse(allocator, test_input.input);
        defer ast.deinit(allocator);

        try testAst(&ast);

        var environment = try Environment.init(allocator);
        defer environment.decRef();

        var result = try evaluator.evalAst(&ast, environment);
        defer result.deinit(allocator);
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
        .{
            .input =
            \\"Hello" - "World"
            ,
            .expected = .{ .Error = .{ .value = "Invalid operation: 'String' - 'String'" } },
        },
        .{ .input = "[1, 2, 3][3]", .expected = .{ .Error = .{ .value = "Index 3 out of bounds for array of length 3" } } },
        .{ .input = "[1, 2, 3][-1]", .expected = .{ .Error = .{ .value = "Index -1 out of bounds for array of length 3" } } },
    };

    const allocator = std.testing.allocator;
    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    for (input) |test_input| {
        var ast = try Ast.parse(allocator, test_input.input);
        defer ast.deinit(allocator);

        try testAst(&ast);

        var environment = try Environment.init(allocator);
        defer environment.decRef();

        var result = try evaluator.evalAst(&ast, environment);
        defer result.deinit(allocator);
        try expectEqualObjects(test_input.expected, result);
    }
}

test "Eval Let Statement" {
    const input = [_]TestInput{
        .{ .input = "let a = 5; a;", .expected = .{ .Value = .{ .Integer = .{ .value = 5 } } } },
        .{ .input = "let a = 5 * 5; a;", .expected = .{ .Value = .{ .Integer = .{ .value = 25 } } } },
        .{ .input = "let a = 5; let b = a; b;", .expected = .{ .Value = .{ .Integer = .{ .value = 5 } } } },
        .{ .input = "let a = 5; let b = a; let c = a + b + 5; c;", .expected = .{ .Value = .{ .Integer = .{ .value = 15 } } } },
    };

    const allocator = std.testing.allocator;
    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    for (input) |test_input| {
        var ast = try Ast.parse(allocator, test_input.input);
        defer ast.deinit(allocator);

        try testAst(&ast);

        var environment = try Environment.init(allocator);
        defer environment.decRef();

        var result = try evaluator.evalAst(&ast, environment);
        defer result.deinit(allocator);
        try expectEqualObjects(test_input.expected, result);
    }
}

test "Eval Function Value" {
    const input = "fn(x) { x + 2; }";

    const allocator = std.testing.allocator;
    var ast = try Ast.parse(allocator, input);
    defer ast.deinit(allocator);

    try testAst(&ast);

    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    var environment = try Environment.init(allocator);
    defer environment.decRef();

    const function = ast.nodes[0].ExpressionStatement.expression.FunctionLiteral;
    const expected = Container{ .Value = .{ .Function = .{
        .parameters = function.parameters,
        .body = function.body,
        .environment = environment,
    } } };

    var result = try evaluator.evalAst(&ast, environment);
    defer result.deinit(allocator);
    try expectEqualObjects(expected, result);
}

test "Eval Call Expression" {
    const input = [_]TestInput{
        .{ .input = "let identity = fn(x) { x; }; identity(5);", .expected = .{ .Value = .{ .Integer = .{ .value = 5 } } } },
        .{ .input = "let identity = fn(x) { return x; }; identity(5);", .expected = .{ .Value = .{ .Integer = .{ .value = 5 } } } },
        .{ .input = "let double = fn(x) { x * 2; }; double(5);", .expected = .{ .Value = .{ .Integer = .{ .value = 10 } } } },
        .{ .input = "let add = fn(x, y) { x + y; }; add(5, 5);", .expected = .{ .Value = .{ .Integer = .{ .value = 10 } } } },
        .{ .input = "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", .expected = .{ .Value = .{ .Integer = .{ .value = 20 } } } },
        .{ .input = "fn(x) { x; }(5)", .expected = .{ .Value = .{ .Integer = .{ .value = 5 } } } },
    };

    const allocator = std.testing.allocator;
    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    for (input) |test_input| {
        var ast = try Ast.parse(allocator, test_input.input);
        defer ast.deinit(allocator);

        try testAst(&ast);

        var environment = try Environment.init(allocator);
        defer environment.decRef();

        var result = try evaluator.evalAst(&ast, environment);
        defer result.deinit(allocator);
        try expectEqualObjects(test_input.expected, result);
    }
}

test "Eval Builtins" {
    const input = [_]TestInput{
        .{
            .input =
            \\len("")
            ,
            .expected = .{ .Value = .{ .Integer = .{ .value = 0 } } },
        },
        .{
            .input =
            \\len("four")
            ,
            .expected = .{ .Value = .{ .Integer = .{ .value = 4 } } },
        },
        .{
            .input =
            \\len("Hello World")
            ,
            .expected = .{ .Value = .{ .Integer = .{ .value = 11 } } },
        },
        .{
            .input =
            \\len(1)
            ,
            .expected = .{ .Error = .{ .value = "Builtin signature mismatch. Expected 'String' or 'Array' but found 'Integer'" } },
        },
        .{
            .input =
            \\len("one", "two")
            ,
            .expected = .{ .Error = .{ .value = "Builtin signature mismatch. Expected 1 argument but found 2" } },
        },
        .{
            .input =
            \\len([1, 2, 3])
            ,
            .expected = .{ .Value = .{ .Integer = .{ .value = 3 } } },
        },
    };

    const allocator = std.testing.allocator;
    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    for (input) |test_input| {
        var ast = try Ast.parse(allocator, test_input.input);
        defer ast.deinit(allocator);

        try testAst(&ast);

        var environment = try Environment.init(allocator);
        defer environment.decRef();

        var result = try evaluator.evalAst(&ast, environment);
        defer result.deinit(allocator);
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

    const allocator = std.testing.allocator;
    var ast = try Ast.parse(allocator, input);
    defer ast.deinit(allocator);

    try testAst(&ast);

    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    var environment = try Environment.init(allocator);
    defer environment.decRef();

    const expected = Container{ .Value = .{ .Integer = .{ .value = 4 } } };

    var result = try evaluator.evalAst(&ast, environment);
    defer result.deinit(allocator);
    try expectEqualObjects(expected, result);
}

test "Eval Arrays" {
    const input = [_]TestInput{
        // .{ .input = "[1, 2 * 2, 3 + 3]", .expected = .{
        //     .Value = .{
        //         .Array = .{ .values = @constCast(&[_]Object.Value{
        //             .{ .Integer = .{ .value = 1 } },
        //             .{ .Integer = .{ .value = 4 } },
        //             .{ .Integer = .{ .value = 6 } },
        //         }) },
        //     },
        // } },
        .{
            .input =
            \\let map = fn(arr, f) {
            \\    let itr = fn(arr, accumulated) {
            \\        if (len(arr) == 0) {
            \\            accumulated;
            \\         } else {
            \\            itr(rest(arr), push(accumulated, f(first(arr))));
            \\        }
            \\    };
            \\    itr(arr, []);
            \\};
            \\let a = [1, 2, 3, 4];
            \\let double = fn(x) { x * 2; };
            \\map(a, double);
            ,
            .expected = .{
                .Value = .{
                    .Array = .{
                        .values = @constCast(&[_]Object.Value{
                            .{ .Integer = .{ .value = 2 } },
                            .{ .Integer = .{ .value = 4 } },
                            .{ .Integer = .{ .value = 6 } },
                            .{ .Integer = .{ .value = 8 } },
                        }),
                    },
                },
            },
        },
    };

    const allocator = std.testing.allocator;
    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    for (input) |test_input| {
        var ast = try Ast.parse(allocator, test_input.input);
        defer ast.deinit(allocator);

        try testAst(&ast);

        var environment = try Environment.init(allocator);
        defer environment.decRef();

        var result = try evaluator.evalAst(&ast, environment);
        defer result.deinit(allocator);
        try expectEqualObjects(test_input.expected, result);
    }
}

test "Eval Index Expression" {
    const input = [_]TestInput{
        .{ .input = "[1, 2, 3][0]", .expected = .{ .Value = .{ .Integer = .{ .value = 1 } } } },
        .{ .input = "[1, 2, 3][1]", .expected = .{ .Value = .{ .Integer = .{ .value = 2 } } } },
        .{ .input = "[1, 2, 3][2]", .expected = .{ .Value = .{ .Integer = .{ .value = 3 } } } },
        .{ .input = "let i = 0; [1][i];", .expected = .{ .Value = .{ .Integer = .{ .value = 1 } } } },
        .{ .input = "[1, 2, 3][1 + 1];", .expected = .{ .Value = .{ .Integer = .{ .value = 3 } } } },
        .{ .input = "let myArray = [1, 2, 3]; myArray[2];", .expected = .{ .Value = .{ .Integer = .{ .value = 3 } } } },
        .{ .input = "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];", .expected = .{ .Value = .{ .Integer = .{ .value = 6 } } } },
        .{ .input = "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]", .expected = .{ .Value = .{ .Integer = .{ .value = 2 } } } },
    };

    const allocator = std.testing.allocator;
    var evaluator = try Evaluator.init(allocator);
    defer evaluator.deinit();

    for (input) |test_input| {
        var ast = try Ast.parse(allocator, test_input.input);
        defer ast.deinit(allocator);

        try testAst(&ast);

        var environment = try Environment.init(allocator);
        defer environment.decRef();

        var result = try evaluator.evalAst(&ast, environment);
        defer result.deinit(allocator);
        try expectEqualObjects(test_input.expected, result);
    }
}

const TestError = error{ TestExpectedEqual, TestUnexpectedResult };

fn testAst(ast: *Ast) !void {
    if (ast.errors.len > 0) {
        std.debug.print("Parse failed with {d} errors:\n", .{ast.errors.len});
        for (ast.errors) |err| {
            std.debug.print("- {s}\n", .{err});
        }
        try std.testing.expect(false);
    }
}

fn expectEqualObjects(expected: Container, actual: Container) TestError!void {
    std.testing.expectEqualStrings(@tagName(expected), @tagName(actual)) catch |err| {
        if (actual == .Error and expected != .Error) {
            std.debug.print("Error value: {s}\n", .{actual.Error.value});
        }
        return err;
    };
    switch (expected) {
        .Value => |value| try expectEqualValues(value, actual.Value),
        .ReturnValue => |value| try expectEqualValues(value, actual.ReturnValue),
        .BuiltinFunction => try std.testing.expect(false),
        .Error => |err| try std.testing.expectEqualStrings(err.value, actual.Error.value),
    }
}

fn expectEqualValues(expected: Object.Value, actual: Object.Value) TestError!void {
    try std.testing.expectEqualStrings(@tagName(expected), @tagName(actual));
    switch (expected) {
        .Null => {},
        .Integer => |integer| try std.testing.expectEqual(integer.value, actual.Integer.value),
        .Boolean => |boolean| try std.testing.expectEqual(boolean.value, actual.Boolean.value),
        .String => |string| try std.testing.expectEqualStrings(string.value, actual.String.value),
        .Array => |array| try expectEqualArrays(array.values, actual.Array.values),
        .Function => |function| try expectEqualFunctions(function, actual.Function),
    }
}

fn expectEqualArrays(expected_values: []const Object.Value, actual_values: []const Object.Value) TestError!void {
    try std.testing.expectEqual(expected_values.len, actual_values.len);
    for (expected_values, actual_values) |expected, actual| {
        try expectEqualValues(expected, actual);
    }
}

fn expectEqualFunctions(expected: Object.Function, actual: Object.Function) TestError!void {
    try std.testing.expectEqual(expected.parameters.len, actual.parameters.len);
    for (expected.parameters, actual.parameters) |expected_param, actual_param| {
        try std.testing.expectEqualStrings(expected_param.value, actual_param.value);
    }
    try std.testing.expectEqual(expected.body, actual.body);
    try std.testing.expectEqual(expected.environment, actual.environment);
}
