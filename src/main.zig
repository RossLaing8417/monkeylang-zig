const std = @import("std");

const Repl = @import("repl.zig");

pub fn main() !void {
    const stdin = std.io.getStdIn();
    const stdout = std.io.getStdOut();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);

    var repl = Repl.init(gpa.allocator(), stdin, stdout);

    try repl.loop();
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
