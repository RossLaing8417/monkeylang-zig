const std = @import("std");

const Evaluator = @import("evaluator.zig");
const ObjectType = @import("object.zig");

const BuiltinFunction = ObjectType.BuiltinFunction;
const Object = ObjectType.Object;

const Error = std.mem.Allocator.Error;

pub const Function = (*const fn (*Evaluator, []const Object) Error!Object);

pub const map = std.ComptimeStringMap(BuiltinFunction, .{
    .{ "len", BuiltinFunction{ .name = "len", .func = &len } },
});

fn len(evaluator: *Evaluator, objects: []const Object) Error!Object {
    if (objects.len != 1) {
        return try evaluator.evalError(
            "Builtin signature mismatch. Expected 1 argument but found {d}",
            .{objects.len},
        );
    }

    std.debug.assert(objects[0] == .Literal);

    const arg = objects[0].Literal;

    if (arg == .String) {
        const string = arg.String.value;
        var result: i64 = 0;
        for (0..string.len) |i| {
            if (string[i] == '\\' and string[i + 1] == '"') {
                continue;
            }
            result += 1;
        }
        return .{ .Literal = .{ .Integer = .{ .value = result } } };
    }

    return try evaluator.evalError(
        "Builtin signature mismatch. Expected 'String' but found '{s}'",
        .{@tagName(arg)},
    );
}
