const std = @import("std");

const Parser = @import("parser.zig");
const Lexer = @import("lexer.zig");
const Ast = @import("ast.zig");

pub const Object = union(enum) {
    Integer: Integer,
    Boolean: Boolean,
    Null: Null,

    pub fn inspect(self: *const Object, writer: anytype) !void {
        switch (self.*) {
            inline else => |object| try object.inspect(writer),
        }
    }
};

pub const Integer = struct {
    value: i64,

    pub fn inspect(self: *const Integer, writer: anytype) !void {
        try writer.print("{d}", .{self.value});
    }
};

pub const Boolean = struct {
    value: bool,

    pub fn inspect(self: *const Boolean, writer: anytype) !void {
        try writer.print("{}", .{self.value});
    }
};

pub const Null = struct {
    pub fn inspect(_: *const Null, writer: anytype) !void {
        try writer.writeAll("null");
    }
};

pub fn eval(node: Ast.Node) Object {
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
                else => unreachable,
            }
        },
    }

    return .{ .Null = .{} };
}

fn evalStatements(statements: []*Ast.Statement) Object {
    var result: Object = undefined;

    for (statements) |statement| {
        result = eval(.{ .Statement = statement });
    }

    return result;
}

test "Eval Integer Expression" {
    const Input = struct { input: []const u8, expected: i64 };
    const input = [_]Input{
        .{ .input = "5", .expected = 5 },
        .{ .input = "10", .expected = 10 },
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
