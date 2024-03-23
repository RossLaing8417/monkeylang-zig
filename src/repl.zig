const Repl = @This();

const std = @import("std");

const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const Ast = @import("ast.zig");
const Evaluator = @import("evaluator.zig");
const Environment = @import("environment.zig");

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
    defer environment.deinit(self.allocator);

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

    while (true) {
        _ = try out_stream.write(PROMPT);

        var buffer = std.ArrayList(u8).init(self.allocator);
        defer buffer.deinit();

        var writer = buffer.writer();

        while (true) {
            in_stream.streamUntilDelimiter(writer, '\n', null) catch |err| switch (err) {
                error.EndOfStream => break,
                else => |e| return e,
            };
            try writer.writeByte('\n');
        }

        if (buffer.items.len == 0) {
            continue;
        }

        var ast = try Ast.parse(self.allocator, try buffer.toOwnedSlice());

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

        const result = try evaluator.evalAst(&ast, environment);
        _ = result;
        // if (result != .Literal or result == .Literal and result.Literal != .Function) {
        //     try result.inspect(out_stream);
        //     try out_stream.writeAll("\n");
        // }
    }
}

test {
    std.testing.refAllDeclsRecursive(Ast);
}
