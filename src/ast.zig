const std = @import("std");

const Lexer = @import("lexer.zig");
const Token = Lexer.Token;
const WriteError = std.fs.File.WriteError;

const INIT_PROG_CAPACITY = 32;

pub const Node = union(enum) {
    Statement: Statement,
    Expression: Expression,

    pub fn tokenLiteral(self: Node) []const u8 {
        switch (self) {
            inline else => |node| return node.tokenLiteral(),
        }
    }

    pub fn write(self: Node, writer: std.fs.File.Writer) WriteError!usize {
        switch (self) {
            inline else => |node| return try node.write(writer),
        }
    }
};

pub const Statement = union(enum) {
    Program: Program,

    LetStatement: LetStatement,
    ReturnStatement: ReturnStatement,
    ExpressionStatement: ExpressionStatement,

    pub fn tokenLiteral(self: Statement) []const u8 {
        switch (self) {
            inline else => |statement| return statement.tokenLiteral(),
        }
    }

    pub fn write(self: Statement, writer: std.fs.File.Writer) WriteError!usize {
        switch (self) {
            inline else => |statement| return try statement.write(writer),
        }
    }
};

pub const Expression = union(enum) {
    Identifier: Identifier,

    pub fn tokenLiteral(self: Expression, writer: std.fs.File.Writer) WriteError!usize {
        switch (self) {
            inline else => |expression| return expression.tokenLiteral(writer),
        }
    }

    pub fn write(self: Expression, writer: std.fs.File.Writer) WriteError!usize {
        switch (self) {
            inline else => |expression| return try expression.write(writer),
        }
    }
};

pub const Program = struct {
    statements: std.ArrayList(Statement),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !*Program {
        var program = try allocator.create(Program);
        errdefer program.deinit();

        program.* = Program{
            .statements = try std.ArrayList(Statement).initCapacity(allocator, INIT_PROG_CAPACITY),
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

    pub fn write(self: *const Program, writer: std.fs.File.Writer) WriteError!usize {
        var bytes: usize = 0;
        for (self.statements.items) |statement| {
            bytes += try statement.write(writer);
            bytes += try writer.write("\n");
        }
        return bytes;
    }
};

pub const LetStatement = struct {
    token: Token,
    name: Identifier = undefined,
    value: Expression = undefined,

    pub fn tokenLiteral(self: *const LetStatement) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const LetStatement, writer: std.fs.File.Writer) WriteError!usize {
        var bytes: usize = 0;
        bytes += try writer.write(self.tokenLiteral());
        bytes += try writer.write(" ");
        bytes += try self.name.write(writer);
        bytes += try writer.write(" = ");
        bytes += try writer.write("<...>");
        bytes += try writer.write(";");
        return bytes;
    }
};

pub const ReturnStatement = struct {
    token: Token,
    return_value: Expression = undefined,

    pub fn tokenLiteral(self: *const ReturnStatement) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const ReturnStatement, writer: std.fs.File.Writer) WriteError!usize {
        var bytes: usize = 0;
        bytes += try writer.write(self.tokenLiteral());
        bytes += try writer.write(" ");
        bytes += try writer.write("<...>");
        bytes += try writer.write(";");
        return bytes;
    }
};

pub const ExpressionStatement = struct {
    token: Token,
    expression: Expression = undefined,

    pub fn tokenLiteral(self: *const ExpressionStatement) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const ExpressionStatement, writer: std.fs.File.Writer) WriteError!usize {
        return try self.expression.write(writer);
    }
};

pub const Identifier = struct {
    token: Token,
    value: []const u8 = undefined,

    pub fn tokenLiteral(self: *const Identifier) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const Identifier, writer: std.fs.File.Writer) WriteError!usize {
        return try writer.write(self.value);
    }
};
