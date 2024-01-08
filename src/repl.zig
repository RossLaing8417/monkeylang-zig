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

    var arena = std.heap.ArenaAllocator.init(self.allocator);
    defer arena.deinit();

    var allocator = arena.allocator();

    var environment = try Environment.init(allocator);
    defer environment.deinit();

    while (true) {
        _ = try out_stream.write(PROMPT);

        // Casually allocating memory...
        var buffer = std.ArrayList(u8).init(allocator);

        in_stream.streamUntilDelimiter(buffer.writer(), '\n', null) catch |err| switch (err) {
            error.EndOfStream => break,
            else => |e| return e,
        };

        if (buffer.items.len == 0) {
            continue;
        }

        var line = buffer.items;

        var lexer = Lexer.init(line);
        var parser = try Parser.init(&lexer, allocator);
        defer parser.deinit();

        var program = try parser.parseProgram(allocator);

        if (parser.errors.items.len > 0) {
            try out_stream.writeAll("Errors:");
            for (parser.errors.items) |err| {
                try out_stream.writeAll("\n\t");
                try out_stream.writeAll(err);
            }
            try out_stream.writeAll("\n");
            continue;
        }

        var evaluator = Evaluator.init(allocator);

        var statement = Ast.Statement{ .Program = program };
        var result = evaluator.eval(.{ .Statement = &statement }, environment);

        if (result != .Literal or result == .Literal and result.Literal != .Function) {
            try result.inspect(out_stream);
            try out_stream.writeAll("\n");
        }
    }
}
