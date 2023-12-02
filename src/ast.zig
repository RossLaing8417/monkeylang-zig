const std = @import("std");

const Lexer = @import("lexer.zig");
const Token = Lexer.Token;
const WriteError = std.fs.File.WriteError;

pub const Node = union(enum) {
    Statement: *Statement,
    Expression: *Expression,

    pub fn tokenLiteral(self: Node) []const u8 {
        switch (self) {
            inline else => |node| return node.tokenLiteral(),
        }
    }

    pub fn write(self: Node, writer: anytype) void {
        switch (self) {
            inline else => |node| node.write(writer),
        }
    }
};

pub const Statement = union(enum) {
    Program: *Program,

    LetStatement: *LetStatement,
    ReturnStatement: *ReturnStatement,
    ExpressionStatement: *ExpressionStatement,

    pub fn tokenLiteral(self: Statement) []const u8 {
        switch (self) {
            inline else => |statement| return statement.tokenLiteral(),
        }
    }

    pub fn write(self: Statement, writer: anytype) void {
        switch (self) {
            inline else => |statement| statement.write(writer),
        }
    }
};

pub const Expression = union(enum) {
    Identifier: *Identifier,
    Integer: *Integer,
    Boolean: *Boolean,

    PrefixExpression: *PrefixExpression,
    InfixExpression: *InfixExpression,
    IfExpression: *IfExpression,
    FunctionLiteral: *FunctionLiteral,
    CallExpression: *CallExpression,

    pub fn tokenLiteral(self: Expression, writer: anytype) void {
        switch (self) {
            inline else => |expression| return expression.tokenLiteral(writer),
        }
    }

    pub fn write(self: Expression, writer: anytype) void {
        switch (self) {
            inline else => |expression| expression.write(writer),
        }
    }
};

pub const Program = struct {
    statements: std.ArrayList(*Statement),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !*Program {
        var program = try allocator.create(Program);
        errdefer program.deinit();

        program.* = Program{
            .statements = std.ArrayList(*Statement).init(allocator),
            .allocator = allocator,
        };

        return program;
    }

    pub fn deinit(self: *Program) void {
        self.statements.deinit();
        self.allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const Program) []const u8 {
        if (self.statements.items.len > 0) {
            return self.statements.items[0].tokenLiteral();
        }
        return "";
    }

    pub fn write(self: *const Program, writer: anytype) void {
        for (self.statements.items) |statement| {
            statement.write(writer);
            writer.writeAll("\n") catch unreachable;
        }
    }
};

pub const LetStatement = struct {
    token: Token,
    name: *Identifier = undefined,
    value: *Expression = undefined,

    pub fn tokenLiteral(self: *const LetStatement) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const LetStatement, writer: anytype) void {
        writer.writeAll(self.tokenLiteral()) catch unreachable;
        writer.writeAll(" ") catch unreachable;
        self.name.write(writer);
        writer.writeAll(" = ") catch unreachable;
        self.value.write(writer);
        writer.writeAll(";") catch unreachable;
    }
};

pub const ReturnStatement = struct {
    token: Token,
    return_value: *Expression = undefined,

    pub fn tokenLiteral(self: *const ReturnStatement) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const ReturnStatement, writer: anytype) void {
        writer.writeAll(self.tokenLiteral()) catch unreachable;
        writer.writeAll(" ") catch unreachable;
        writer.writeAll("<...>") catch unreachable;
        self.return_value.write(writer);
        writer.writeAll(";") catch unreachable;
    }
};

pub const ExpressionStatement = struct {
    token: Token,
    expression: *Expression = undefined,

    pub fn tokenLiteral(self: *const ExpressionStatement) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const ExpressionStatement, writer: anytype) void {
        self.expression.write(writer);
        writer.writeAll(";") catch unreachable;
    }
};

pub const Identifier = struct {
    token: Token,
    value: []const u8,

    pub fn tokenLiteral(self: *const Identifier) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const Identifier, writer: anytype) void {
        writer.writeAll(self.value) catch unreachable;
    }
};

pub const Integer = struct {
    token: Token,
    value: []const u8, // i64?

    pub fn tokenLiteral(self: *const Integer) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const Integer, writer: anytype) void {
        writer.writeAll(self.value) catch unreachable;
    }
};

pub const Boolean = struct {
    token: Token,
    value: []const u8, // bool?

    pub fn tokenLiteral(self: *const Boolean) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const Boolean, writer: anytype) void {
        writer.writeAll(self.value) catch unreachable;
    }
};

pub const PrefixExpression = struct {
    token: Token,
    operator: []const u8,
    operand: *Expression = undefined,

    pub fn tokenLiteral(self: *const PrefixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const PrefixExpression, writer: anytype) void {
        writer.writeAll("(") catch unreachable;
        writer.writeAll(self.operator) catch unreachable;
        self.operand.write(writer);
        writer.writeAll(")") catch unreachable;
    }
};

pub const InfixExpression = struct {
    token: Token,
    operator: []const u8,
    left_operand: *Expression = undefined,
    right_operand: *Expression = undefined,

    pub fn tokenLiteral(self: *const InfixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const InfixExpression, writer: anytype) void {
        writer.writeAll("(") catch unreachable;
        self.left_operand.write(writer);
        writer.writeAll(" ") catch unreachable;
        writer.writeAll(self.operator) catch unreachable;
        writer.writeAll(" ") catch unreachable;
        self.right_operand.write(writer);
        writer.writeAll(")") catch unreachable;
    }
};

pub const IfExpression = struct {
    token: Token,
    condition: *Expression,
    consequence: *BlockStatement,
    alternative: ?*BlockStatement,

    pub fn tokenLiteral(self: *const IfExpression) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const IfExpression, writer: anytype) void {
        writer.writeAll("if ") catch unreachable;
        self.condition.write(writer);
        writer.writeAll(" ") catch unreachable;
        self.consequence.write(writer);
        if (self.alternative) |alternative| {
            writer.writeAll("else ") catch unreachable;
            alternative.write(writer);
        }
    }
};

pub const BlockStatement = struct {
    token: Token,
    statements: std.ArrayList(*Statement),
    allocator: std.mem.Allocator,

    pub fn init(token: Token, allocator: std.mem.Allocator) !*BlockStatement {
        var block_statement = try allocator.create(BlockStatement);
        errdefer block_statement.deinit();

        block_statement.* = BlockStatement{
            .token = token,
            .statements = std.ArrayList(*Statement).init(allocator),
            .allocator = allocator,
        };

        return block_statement;
    }

    pub fn deinit(self: *BlockStatement) void {
        self.statements.deinit();
        self.allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const BlockStatement) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const BlockStatement, writer: anytype) void {
        writer.writeAll("{\n") catch unreachable;
        for (self.statements.items) |statement| {
            statement.write(writer);
            writer.writeAll("\n") catch unreachable;
        }
        writer.writeAll("}\n") catch unreachable;
    }
};

pub const FunctionLiteral = struct {
    token: Token,
    parameters: std.ArrayList(*Identifier),
    body: *BlockStatement,
    allocator: std.mem.Allocator,

    pub fn init(token: Token, allocator: std.mem.Allocator) !*FunctionLiteral {
        var function_literal = try allocator.create(FunctionLiteral);
        errdefer function_literal.deinit();

        function_literal.* = FunctionLiteral{
            .token = token,
            .parameters = std.ArrayList(*Identifier).init(allocator),
            .body = undefined,
            .allocator = allocator,
        };

        return function_literal;
    }

    pub fn deinit(self: *FunctionLiteral) void {
        self.parameters.deinit();
        self.allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const FunctionLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const FunctionLiteral, writer: anytype) void {
        writer.writeAll("fn ") catch unreachable;
        writer.writeAll("(") catch unreachable;
        for (self.parameters.items, 0..) |parameter, i| {
            if (i > 0) writer.writeAll(", ") catch unreachable;
            parameter.write(writer);
        }
        writer.writeAll(") ") catch unreachable;
        self.body.write(writer);
    }
};

pub const CallExpression = struct {
    token: Token,
    function: *Expression,
    arguments: std.ArrayList(*Expression),
    allocator: std.mem.Allocator,

    pub fn init(token: Token, allocator: std.mem.Allocator) !*CallExpression {
        var call_expression = try allocator.create(CallExpression);
        errdefer call_expression.deinit();

        call_expression.* = CallExpression{
            .token = token,
            .function = undefined,
            .arguments = std.ArrayList(*Expression).init(allocator),
            .allocator = allocator,
        };

        return call_expression;
    }

    pub fn deinit(self: *CallExpression) void {
        self.arguments.deinit();
        self.allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const CallExpression) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const CallExpression, writer: anytype) void {
        self.function.write(writer);
        writer.writeAll("(") catch unreachable;
        for (self.arguments.items, 0..) |argument, i| {
            if (i > 0) writer.writeAll(", ") catch unreachable;
            argument.write(writer);
        }
        writer.writeAll(")") catch unreachable;
    }
};
