const Repl = @This();

const std = @import("std");

const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const Ast = @import("ast.zig");
const Evaluator = @import("evaluator.zig");
const Environment = @import("environment.zig");
const Object = @import("object.zig");

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
    defer environment.deinit();

    // Repl scope is entire execution run
    var arena = std.heap.ArenaAllocator.init(self.allocator);

    var buf_allocator = arena.allocator();

    while (true) {
        _ = try out_stream.write(PROMPT);

        // Not cleaning up, we want the memory to persist while the repl is active
        var buffer = std.ArrayList([]const u8).init(buf_allocator);
        var writer = buffer.writer();

        while (true) {
            in_stream.streamUntilDelimiter(writer, '\n', null) catch |err| switch (err) {
                error.EndOfStream => break,
                else => |e| return e,
            };
            writer.writeByte('\n');
        }

        if (buffer.items.len == 0) {
            continue;
        }

        var lexer = Lexer.init(buffer.items);
        var parser = try Parser.init(&lexer, self.allocator);
        defer parser.deinit();

        var program = try parser.parseProgram(self.allocator);

        if (parser.errors.items.len > 0) {
            try out_stream.writeAll("Errors:");
            for (parser.errors.items) |err| {
                try out_stream.writeAll("\n\t");
                try out_stream.writeAll(err);
            }
            try out_stream.writeAll("\n");
            continue;
        }

        var evaluator = Evaluator.init(self.allocator);

        var statement = Ast.Statement{ .Program = program };
        var result = evaluator.eval(.{ .Statement = &statement }, environment);

        if (result != .Literal or result == .Literal and result.Literal != .Function) {
            try result.inspect(out_stream);
            try out_stream.writeAll("\n");
        }
    }
}
