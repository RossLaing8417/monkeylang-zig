const std = @import("std");

const Repl = @import("repl.zig");

pub fn main() !void {
    const stdin = std.io.getStdIn();
    const stdout = std.io.getStdOut();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const result = gpa.deinit();
        if (result == .leak) {
            std.debug.print("Leak!\n", .{});
        }
    }

    var repl = Repl.init(gpa.allocator(), stdin, stdout);

    try repl.loop();
}

test {
    _ = @import("lexer.zig");
    _ = @import("parser.zig");
    _ = @import("evaluator.zig");
}
