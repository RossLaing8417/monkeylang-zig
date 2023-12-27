const Repl = @This();

const std = @import("std");

const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const Ast = @import("ast.zig");
const Object = @import("object.zig");

const PROMPT = ">> ";
const MAX_LENGTH = 256;

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

    var buffer: [MAX_LENGTH]u8 = undefined;

    while (true) {
        _ = try out_stream.write(PROMPT);

        // TODO: Try streamUntilDelimiter

        while (try in_stream.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
            var lexer = Lexer.init(line);
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
                break;
            }

            var statement = Ast.Statement{ .Program = program };
            var result = Object.eval(.{ .Statement = &statement });

            try result.inspect(out_stream);
            try out_stream.writeAll("\n");

            break;
        }

        // I think something is getting stuck in a buffer somewhere...
        // I'm getting to this point but if I don't write something
        // (using the out stream or std.debug) then I'm left stuck
        // waiting for input but without the prompt. If i hit <Ctrl-D>
        // then I get to the promt.
        // Some day I figure this out I hope...
        try out_stream.print("", .{});
    }
}
