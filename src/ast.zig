const std = @import("std");

const Lexer = @import("lexer.zig");
const Token = Lexer.Token;

const INIT_PROG_CAPACITY = 32;

pub const Node = union(enum) {
    Statement: Statement,
    Expression: Expression,

    pub fn tokenLiteral(self: Node) []const u8 {
        switch (self) {
            inline else => |node| return node.tokenLiteral(),
        }
    }
};

pub const Statement = union(enum) {
    Program: Program,
    LetStatement: LetStatement,

    pub fn tokenLiteral(self: Statement) []const u8 {
        switch (self) {
            inline else => |node| return node.tokenLiteral(),
        }
    }
};

pub const Expression = struct {
    Identifier: Identifier,

    pub fn tokenLiteral(self: *const Expression) []const u8 {
        switch (self) {
            inline else => |node| return node.tokenLiteral(),
        }
    }
};

pub const Program = struct {
    statements: std.ArrayList(Statement),

    pub fn init(allocator: std.mem.Allocator) !*Program {
        var program = try allocator.create(Program);

        program.* = Program{
            .statements = try std.ArrayList(Statement).initCapacity(allocator, INIT_PROG_CAPACITY),
        };

        return program;
    }

    pub fn deinit(self: *Program, allocator: std.mem.Allocator) void {
        self.statements.deinit();
        allocator.destroy(self);
    }

    pub fn tokenLiteral(self: *const Program) []const u8 {
        if (self.statements.items.len > 0) {
            return self.statements.items[0].tokenLiteral();
        }
        return "";
    }
};

pub const LetStatement = struct {
    token: Token,
    name: Identifier = undefined,
    value: Expression = undefined,

    pub fn tokenLiteral(self: *const LetStatement) []const u8 {
        return self.token.literal;
    }
};

pub const Identifier = struct {
    token: Token,
    value: []const u8 = undefined,

    pub fn tokenLiteral(self: *const Identifier) []const u8 {
        return self.token.literal;
    }
};
