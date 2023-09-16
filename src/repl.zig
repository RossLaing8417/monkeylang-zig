const Repl = @This();

const std = @import("std");

const Lexer = @import("lexer.zig");

const PROMPT = ">> ";
const MAX_LENGTH = 256;

in_file: std.fs.File,
out_file: std.fs.File,

pub fn init(in_file: std.fs.File, out_file: std.fs.File) Repl {
    return Repl{
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

            while (true) {
                const token = lexer.nextToken();
                if (token.type == .Eof) break;

                try out_stream.print("{s} = '{s}'\n", .{ @tagName(token.type), token.literal });
            }

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
