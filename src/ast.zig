const std = @import("std");

const Lexer = @import("lexer.zig");
const Token = Lexer.Token;
const WriteError = std.fs.File.WriteError;

const INIT_PROG_CAPACITY = 32;

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

    PrefixExpression: *PrefixExpression,

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
            .statements = try std.ArrayList(*Statement).initCapacity(allocator, INIT_PROG_CAPACITY),
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
