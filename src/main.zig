const std = @import("std");

const Repl = @import("repl.zig");

pub fn main() !void {
    const stdin = std.io.getStdIn();
    const stdout = std.io.getStdOut();

    var repl = Repl.init(stdin, stdout);

    try repl.loop();
}

test {
    _ = @import("lexer.zig");
    _ = @import("repl.zig");
}
