const std = @import("std");

const Ast = @This();

const Parser = @import("parser.zig");
const Lexer = @import("lexer.zig");
const Token = Lexer.Token;

source: []const u8,
tokens: []const Token,
nodes: []const Node,

pub fn parse(allocator: std.mem.Allocator, source: []const u8) !Ast {
    var tokens = std.ArrayList(Token).init(allocator);
    defer tokens.deinit();

    var lexer = Lexer.init(source);
    while (true) {
        const token = lexer.nextToken();
        tokens.append(token);
        if (token.type == .Eof) {
            break;
        }
    }

    var parser = Parser{
        .allocator = allocator,
        .tokens = tokens.items,
        .tok_i = 0,
        .nodes = std.ArrayList([]const Node).init(allocator),
        .errors = std.ArrayList([]const u8).init(allocator),
    };
    defer parser.nodes.deinit();
    defer parser.errors.deinit();

    try parser.parseProgram();

    return .{
        .source = source,
        .tokens = tokens.toOwnedSlice(),
        .nodes = parser.nodes.toOwnedSlice(),
        .errors = parser.errors.toOwnedSlice(),
    };
}

pub fn deinit(self: *Ast, allocator: std.mem.Allocator) void {
    for (self.tokens) |token| {
        token.deinit(allocator);
    }
    for (self.nodes) |node| {
        node.deinit(allocator);
    }
    for (self.errors) |err| {
        err.deinit(allocator);
    }
    allocator.free(self.tokens);
    allocator.free(self.nodes);
    allocator.free(self.errors);
}

pub const Node = union(enum) {
    // Statements
    LetStatement: *LetStatement,
    ReturnStatement: *ReturnStatement,
    ExpressionStatement: *ExpressionStatement,

    BlockStatement: *BlockStatement,

    // Expressions
    Identifier: *Identifier,
    Integer: *Integer,
    Boolean: *Boolean,

    PrefixExpression: *PrefixExpression,
    InfixExpression: *InfixExpression,
    GroupedExpression: *GroupedExpression,
    IfExpression: *IfExpression,
    FunctionLiteral: *FunctionLiteral,
    CallExpression: *CallExpression,

    pub fn deinit(self: *const Node, allocator: std.mem.Allocator) void {
        switch (self.*) {
            inline else => |node| node.deinit(allocator),
        }
    }

    pub fn token(self: *const Node) Token {
        switch (self.*) {
            inline else => |node| return node.token,
        }
    }

    pub fn tokenLiteral(self: *const Node) []const u8 {
        switch (self.*) {
            inline else => |node| return node.tokenLiteral(),
        }
    }

    pub fn write(self: *const Node, writer: anytype) !void {
        switch (self.*) {
            inline else => |node| node.write(writer),
        }
    }
};

pub const LetStatement = struct {
    token: Token,
    name: *Identifier = undefined,
    value: Node = undefined,

    pub fn deinit(self: *LetStatement, allocator: std.mem.Allocator) void {
        self.name.deinit(allocator);
        self.value.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const LetStatement) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const LetStatement, writer: anytype) !void {
        try writer.writeAll(self.tokenLiteral());
        try writer.writeAll(" ");
        try self.name.write(writer);
        try writer.writeAll(" = ");
        try self.value.write(writer);
        try writer.writeAll(";");
    }
};

pub const ReturnStatement = struct {
    token: Token,
    return_value: Node = undefined,

    pub fn deinit(self: *ReturnStatement, allocator: std.mem.Allocator) void {
        self.return_value.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const ReturnStatement) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const ReturnStatement, writer: anytype) !void {
        try writer.writeAll(self.tokenLiteral());
        try writer.writeAll(" ");
        try writer.writeAll("<...>");
        try self.return_value.write(writer);
        try writer.writeAll(";");
    }
};

pub const ExpressionStatement = struct {
    token: Token,
    expression: Node = undefined,

    pub fn deinit(self: *ExpressionStatement, allocator: std.mem.Allocator) void {
        self.expression.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const ExpressionStatement) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const ExpressionStatement, writer: anytype) !void {
        try self.expression.write(writer);
        try writer.writeAll(";");
    }
};

pub const Identifier = struct {
    token: Token,
    value: []const u8,

    pub fn deinit(_: *Identifier, _: std.mem.Allocator) void {}

    pub fn tokenLiteral(self: *const Identifier) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const Identifier, writer: anytype) !void {
        try writer.writeAll(self.value);
    }
};

pub const Integer = struct {
    token: Token,
    value: i64,

    pub fn deinit(_: *Integer, _: std.mem.Allocator) void {}

    pub fn tokenLiteral(self: *const Integer) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const Integer, writer: anytype) !void {
        writer.printf("{d}", .{self.value});
    }
};

pub const Boolean = struct {
    token: Token,
    value: bool,

    pub fn deinit(_: *Boolean, _: std.mem.Allocator) void {}

    pub fn tokenLiteral(self: *const Boolean) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const Boolean, writer: anytype) !void {
        try writer.printf("{}", .{self.value});
    }
};

pub const PrefixExpression = struct {
    token: Token,
    operator: []const u8,
    operand: Node = undefined,

    pub fn deinit(self: *PrefixExpression, allocator: std.mem.Allocator) void {
        self.operand.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const PrefixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const PrefixExpression, writer: anytype) !void {
        try writer.writeAll(self.operator);
        try self.operand.write(writer);
    }
};

pub const InfixExpression = struct {
    token: Token,
    operator: []const u8,
    left_operand: Node = undefined,
    right_operand: Node = undefined,

    pub fn deinit(self: *InfixExpression, allocator: std.mem.Allocator) void {
        self.left_operand.deinit(allocator);
        self.right_operand.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const InfixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const InfixExpression, writer: anytype) !void {
        try self.left_operand.write(writer);
        try writer.writeAll(" ");
        try writer.writeAll(self.operator);
        try writer.writeAll(" ");
        try self.right_operand.write(writer);
    }
};

pub const GroupedExpression = struct {
    token: Token,
    expression: Node,

    pub fn deinit(self: *GroupedExpression, allocator: std.mem.Allocator) void {
        self.expression.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const GroupedExpression) []const u8 {
        self.token.literal;
    }

    pub fn write(self: *const GroupedExpression, writer: anytype) !void {
        try writer.writeAll("(");
        try self.expression.write(writer);
        try writer.writeAll(")");
    }
};

pub const IfExpression = struct {
    token: Token,
    condition: Node,
    consequence: *BlockStatement,
    alternative: ?*BlockStatement,

    pub fn deinit(self: *IfExpression, allocator: std.mem.Allocator) void {
        self.condition.deinit(allocator);
        self.consequence.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const IfExpression) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const IfExpression, writer: anytype) !void {
        try writer.writeAll("if ");
        try self.condition.write(writer);
        try writer.writeAll(" ");
        try self.consequence.write(writer);
        if (self.alternative) |alternative| {
            try writer.writeAll("else ");
            try alternative.write(writer);
        }
    }
};

pub const BlockStatement = struct {
    token: Token,
    statements: []const Node,

    pub fn deinit(self: *BlockStatement, allocator: std.mem.Allocator) void {
        for (self.statements) |*statement| {
            statement.deinit(allocator);
        }
        allocator.free(self.statements);
        allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const BlockStatement) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const BlockStatement, writer: anytype) !void {
        try writer.writeAll("{\n");
        for (self.statements.items) |statement| {
            try statement.write(writer);
            try writer.writeAll("\n");
        }
        try writer.writeAll("}\n");
    }
};

pub const FunctionLiteral = struct {
    token: Token,
    parameters: []const *Identifier,
    body: *BlockStatement,

    pub fn deinit(self: *FunctionLiteral, allocator: std.mem.Allocator) void {
        for (self.parameters) |*parameter| {
            parameter.deinit(allocator);
        }
        allocator.free(self.parameters);
        self.body.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const FunctionLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const FunctionLiteral, writer: anytype) !void {
        try writer.printf("{s} ", .{self.tokenLiteral()});
        try writer.writeAll("(");
        for (self.parameters.items, 0..) |parameter, i| {
            if (i > 0) try writer.writeAll(", ");
            try parameter.write(writer);
        }
        try writer.writeAll(") ");
        try self.body.write(writer);
    }
};

pub const CallExpression = struct {
    token: Token,
    function: Node,
    arguments: []const Node,

    pub fn deinit(self: *CallExpression, allocator: std.mem.Allocator) void {
        for (self.arguments) |argument| {
            argument.deinit(allocator);
        }
        allocator.free(self.arguments);
        allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const CallExpression) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const CallExpression, writer: anytype) !void {
        try self.function.write(writer);
        try writer.writeAll("(");
        for (self.arguments.items, 0..) |argument, i| {
            if (i > 0) try writer.writeAll(", ");
            try argument.write(writer);
        }
        try writer.writeAll(")");
    }
};
