const Repl = @This();

const std = @import("std");

const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Ast = @import("Ast.zig");
const Evaluator = @import("Evaluator.zig");
const Environment = @import("Environment.zig");

const PROMPT = ">> ";

allocator: std.mem.Allocator,
in_file: std.fs.File,
out_file: std.fs.File,

pub fn init(allocator: std.mem.Allocator, in_file: std.fs.File, out_file: std.fs.File) Repl {
    return Repl{
        .allocator = allocator,
        .in_file = in_file,
        .out_file = out_file,
    };
}

pub fn loop(self: *Repl) !void {
    var in_stream = self.in_file.reader();
    var out_stream = self.out_file.writer();

    var environment = try Environment.init(self.allocator);
    defer environment.decRef();

    var evaluator = try Evaluator.init(self.allocator);
    defer evaluator.deinit();

    var ast_store = std.ArrayList(Ast).init(self.allocator);
    defer {
        for (ast_store.items) |*ast| {
            self.allocator.free(ast.source);
            ast.deinit(self.allocator);
        }
        ast_store.deinit();
    }

    var out_buffer = std.ArrayList(u8).init(self.allocator);
    defer out_buffer.deinit();

    var in_buffer = std.ArrayList(u8).init(self.allocator);
    defer in_buffer.deinit();

    const out_writer = out_buffer.writer().any();
    const in_writer = in_buffer.writer();

    while (true) {
        _ = try out_stream.write(PROMPT);

        in_stream.streamUntilDelimiter(in_writer, '\n', null) catch |err| switch (err) {
            error.EndOfStream => break,
            else => |e| return e,
        };

        if (in_buffer.items.len == 0) {
            continue;
        }

        var ast = try Ast.parse(self.allocator, try in_buffer.toOwnedSlice());

        if (ast.errors.len > 0) {
            defer {
                self.allocator.free(ast.source);
                ast.deinit(self.allocator);
            }
            try out_stream.writeAll("Errors:");
            for (ast.errors) |err| {
                try out_stream.writeAll("\n\t");
                try out_stream.writeAll(err);
            }
            try out_stream.writeAll("\n");
            continue;
        }

        try ast_store.append(ast);

        var result = try evaluator.evalAst(&ast, environment);
        defer result.deinit(self.allocator);

        if (result != .Value or result == .Value and result.Value != .Function) {
            out_buffer.shrinkRetainingCapacity(0);
            try result.inspect(out_writer);
            try out_stream.writeAll(out_buffer.items);
            try out_stream.writeByte('\n');
        }
    }
}

test {
    std.testing.refAllDeclsRecursive(Ast);
}
