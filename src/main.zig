const std = @import("std");

const Repl = @import("Repl.zig");

pub fn main() !void {
    const stdin = std.io.getStdIn();
    const stdout = std.io.getStdOut();

    var gpa: std.heap.DebugAllocator(.{}) = .init;
    defer std.debug.assert(gpa.deinit() == .ok);

    var repl = Repl.init(gpa.allocator(), stdin, stdout);

    try repl.loop();
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
