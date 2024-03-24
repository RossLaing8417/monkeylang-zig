const std = @import("std");

const Ast = @This();

const Parser = @import("parser.zig");
const Lexer = @import("lexer.zig");
const Token = Lexer.Token;

source: []const u8,
tokens: []const Token,
nodes: []const Node,
errors: [][]const u8,

pub fn parse(allocator: std.mem.Allocator, source: []const u8) !Ast {
    var tokens = std.ArrayList(Token).init(allocator);
    defer tokens.deinit();

    var lexer = Lexer.init(source);
    while (true) {
        const token = lexer.nextToken();
        try tokens.append(token);
        if (token.type == .Eof) {
            break;
        }
    }

    var parser = Parser{
        .allocator = allocator,
        .tokens = tokens.items,
        .tok_i = 0,
        .nodes = std.ArrayList(Node).init(allocator),
        .errors = std.ArrayList([]const u8).init(allocator),
    };
    defer parser.nodes.deinit();
    defer parser.errors.deinit();

    try parser.parseProgram();

    return .{
        .source = source,
        .tokens = try tokens.toOwnedSlice(),
        .nodes = try parser.nodes.toOwnedSlice(),
        .errors = try parser.errors.toOwnedSlice(),
    };
}

pub fn deinit(self: *Ast, allocator: std.mem.Allocator) void {
    for (self.nodes) |node| {
        node.deinit(allocator);
    }
    for (self.errors) |err| {
        allocator.free(err);
    }
    allocator.free(self.tokens);
    allocator.free(self.nodes);
    allocator.free(self.errors);
}

pub fn write(self: *Ast, allocator: std.mem.Allocator) ![]const u8 {
    var buffer = try std.ArrayList(u8).initCapacity(allocator, self.source.len);
    defer buffer.deinit();
    var writer = buffer.writer();
    for (self.nodes) |node| {
        try node.write(&buffer, .none);
        try writer.writeByte('\n');
    }
    return try buffer.toOwnedSlice();
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
    String: *String,

    FunctionLiteral: *FunctionLiteral,
    ArrayLiteral: *ArrayLiteral,

    PrefixExpression: *PrefixExpression,
    InfixExpression: *InfixExpression,
    GroupedExpression: *GroupedExpression,
    IfExpression: *IfExpression,
    CallExpression: *CallExpression,
    IndexExpression: *IndexExpression,

    pub const WriteOption = enum {
        none,
        debug_precedence,
    };

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

    pub fn write(self: *const Node, buffer: *std.ArrayList(u8), option: Node.WriteOption) error{OutOfMemory}!void {
        switch (self.*) {
            inline else => |node| try node.write(buffer, option),
        }
    }
};

pub const LetStatement = struct {
    token: Token,
    name: *Identifier,
    value: Node = undefined,

    pub fn deinit(self: *LetStatement, allocator: std.mem.Allocator) void {
        self.name.deinit(allocator);
        self.value.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const LetStatement) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const LetStatement, buffer: *std.ArrayList(u8), option: Node.WriteOption) !void {
        var writer = buffer.writer();
        try writer.writeAll(self.tokenLiteral());
        try writer.writeAll(" ");
        try self.name.write(buffer, option);
        try writer.writeAll(" = ");
        try self.value.write(buffer, option);
        try writer.writeAll(";");
    }
};

pub const ReturnStatement = struct {
    token: Token,
    return_value: Node,

    pub fn deinit(self: *ReturnStatement, allocator: std.mem.Allocator) void {
        self.return_value.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const ReturnStatement) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const ReturnStatement, buffer: *std.ArrayList(u8), option: Node.WriteOption) !void {
        var writer = buffer.writer();
        try writer.writeAll(self.tokenLiteral());
        try writer.writeAll(" ");
        try writer.writeAll("<...>");
        try self.return_value.write(buffer, option);
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

    pub fn write(self: *const ExpressionStatement, buffer: *std.ArrayList(u8), option: Node.WriteOption) !void {
        var writer = buffer.writer();
        try self.expression.write(buffer, option);
        try writer.writeAll(";");
    }
};

pub const BlockStatement = struct {
    token: Token,
    statements: []const Node,

    pub fn deinit(self: *BlockStatement, allocator: std.mem.Allocator) void {
        for (self.statements) |statement| {
            statement.deinit(allocator);
        }
        allocator.free(self.statements);
        allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const BlockStatement) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const BlockStatement, buffer: *std.ArrayList(u8), option: Node.WriteOption) !void {
        var writer = buffer.writer();
        try writer.writeAll("{\n");
        for (self.statements) |statement| {
            try statement.write(buffer, option);
            try writer.writeByte('\n');
        }
        try writer.writeAll("}\n");
    }
};

pub const Identifier = struct {
    token: Token,
    value: []const u8,

    pub fn deinit(self: *Identifier, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const Identifier) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const Identifier, buffer: *std.ArrayList(u8), _: Node.WriteOption) !void {
        var writer = buffer.writer();
        try writer.writeAll(self.value);
    }
};

pub const Integer = struct {
    token: Token,
    value: i64,

    pub fn deinit(self: *Integer, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const Integer) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const Integer, buffer: *std.ArrayList(u8), _: Node.WriteOption) !void {
        var writer = buffer.writer();
        try writer.print("{d}", .{self.value});
    }
};

pub const Boolean = struct {
    token: Token,
    value: bool,

    pub fn deinit(self: *Boolean, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const Boolean) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const Boolean, buffer: *std.ArrayList(u8), _: Node.WriteOption) !void {
        var writer = buffer.writer();
        try writer.print("{}", .{self.value});
    }
};

pub const String = struct {
    token: Token,
    value: []const u8,

    pub fn deinit(self: *String, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const String) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const String, buffer: *std.ArrayList(u8), _: Node.WriteOption) !void {
        var writer = buffer.writer();
        try writer.print("\"{s}\"", .{self.value});
    }
};

pub const FunctionLiteral = struct {
    token: Token,
    parameters: []const *Identifier,
    body: *BlockStatement,

    pub fn deinit(self: *FunctionLiteral, allocator: std.mem.Allocator) void {
        for (self.parameters) |parameter| {
            parameter.deinit(allocator);
        }
        allocator.free(self.parameters);
        self.body.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const FunctionLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const FunctionLiteral, buffer: *std.ArrayList(u8), option: Node.WriteOption) !void {
        var writer = buffer.writer();
        try writer.print("{s} ", .{self.tokenLiteral()});
        try writer.writeAll("(");
        for (self.parameters, 0..) |parameter, i| {
            if (i > 0) try writer.writeAll(", ");
            try parameter.write(buffer, option);
        }
        try writer.writeAll(") ");
        try self.body.write(buffer, option);
    }
};

pub const ArrayLiteral = struct {
    token: Token,
    elements: []const Node,

    pub fn deinit(self: *ArrayLiteral, allocator: std.mem.Allocator) void {
        for (self.elements) |element| {
            element.deinit(allocator);
        }
        allocator.free(self.elements);
        allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const ArrayLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const ArrayLiteral, buffer: *std.ArrayList(u8), option: Node.WriteOption) !void {
        var writer = buffer.writer();
        try writer.writeAll("[");
        for (self.elements, 0..) |element, i| {
            if (i > 0) try writer.writeAll(", ");
            try element.write(buffer, option);
        }
        try writer.writeAll("]");
    }
};

pub const PrefixExpression = struct {
    token: Token,
    operator: []const u8,
    operand: Node,

    pub fn deinit(self: *PrefixExpression, allocator: std.mem.Allocator) void {
        self.operand.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const PrefixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const PrefixExpression, buffer: *std.ArrayList(u8), option: Node.WriteOption) !void {
        var writer = buffer.writer();
        if (option == .debug_precedence) {
            try writer.writeAll("(");
        }
        try writer.writeAll(self.operator);
        try self.operand.write(buffer, option);
        if (option == .debug_precedence) {
            try writer.writeAll(")");
        }
    }
};

pub const InfixExpression = struct {
    token: Token,
    operator: []const u8,
    left_operand: Node,
    right_operand: Node,

    pub fn deinit(self: *InfixExpression, allocator: std.mem.Allocator) void {
        self.left_operand.deinit(allocator);
        self.right_operand.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const InfixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const InfixExpression, buffer: *std.ArrayList(u8), option: Node.WriteOption) !void {
        var writer = buffer.writer();
        if (option == .debug_precedence) {
            try writer.writeAll("(");
        }
        try self.left_operand.write(buffer, option);
        try writer.writeAll(" ");
        try writer.writeAll(self.operator);
        try writer.writeAll(" ");
        try self.right_operand.write(buffer, option);
        if (option == .debug_precedence) {
            try writer.writeAll(")");
        }
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
        return self.token.literal;
    }

    pub fn write(self: *const GroupedExpression, buffer: *std.ArrayList(u8), option: Node.WriteOption) !void {
        var writer = buffer.writer();
        if (option != .debug_precedence) {
            try writer.writeAll("(");
        }
        try self.expression.write(buffer, option);
        if (option != .debug_precedence) {
            try writer.writeAll(")");
        }
    }
};

pub const IfExpression = struct {
    token: Token,
    condition: Node,
    consequence: Node,
    alternative: ?Node,

    pub fn deinit(self: *IfExpression, allocator: std.mem.Allocator) void {
        self.condition.deinit(allocator);
        self.consequence.deinit(allocator);
        if (self.alternative) |alternative| {
            alternative.deinit(allocator);
        }
        allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const IfExpression) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const IfExpression, buffer: *std.ArrayList(u8), option: Node.WriteOption) !void {
        var writer = buffer.writer();
        try writer.writeAll("if ");
        try self.condition.write(buffer, option);
        try writer.writeAll(" ");
        try self.consequence.write(buffer, option);
        if (self.alternative) |alternative| {
            try writer.writeAll("else ");
            try alternative.write(buffer, option);
        }
    }
};

pub const CallExpression = struct {
    token: Token,
    function: Node,
    arguments: []const Node,

    pub fn deinit(self: *CallExpression, allocator: std.mem.Allocator) void {
        self.function.deinit(allocator);
        for (self.arguments) |argument| {
            argument.deinit(allocator);
        }
        allocator.free(self.arguments);
        allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const CallExpression) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const CallExpression, buffer: *std.ArrayList(u8), option: Node.WriteOption) !void {
        var writer = buffer.writer();
        try self.function.write(buffer, option);
        try writer.writeAll("(");
        for (self.arguments, 0..) |argument, i| {
            if (i > 0) try writer.writeAll(", ");
            try argument.write(buffer, option);
        }
        try writer.writeAll(")");
    }
};

pub const IndexExpression = struct {
    token: Token,
    expression: Node,
    index: Node,

    pub fn deinit(self: *IndexExpression, allocator: std.mem.Allocator) void {
        self.expression.deinit(allocator);
        self.index.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const IndexExpression) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const IndexExpression, buffer: *std.ArrayList(u8), option: Node.WriteOption) !void {
        var writer = buffer.writer();
        if (option == .debug_precedence) {
            try writer.writeAll("(");
        }
        try self.expression.write(buffer, option);
        try writer.writeAll("[");
        try self.index.write(buffer, option);
        try writer.writeAll("]");
        if (option == .debug_precedence) {
            try writer.writeAll(")");
        }
    }
};
