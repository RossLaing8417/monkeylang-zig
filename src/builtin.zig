const std = @import("std");

const Evaluator = @import("evaluator.zig");
const Object = @import("object.zig");

const BuiltinFunction = Object.BuiltinFunction;
const Container = Object.Container;

const Error = std.mem.Allocator.Error;

pub const Function = (*const fn (*Evaluator, []const Container) Error!Container);

const NULL = Container{ .Value = .{ .Null = .{} } };

pub const FunctionMap = std.StaticStringMap(BuiltinFunction).initComptime(.{
    .{ "len", BuiltinFunction{ .name = "len", .func = &len } },
    .{ "first", BuiltinFunction{ .name = "first", .func = &first } },
    .{ "last", BuiltinFunction{ .name = "last", .func = &last } },
    .{ "rest", BuiltinFunction{ .name = "rest", .func = &rest } },
    .{ "push", BuiltinFunction{ .name = "push", .func = &push } },
});

fn len(evaluator: *Evaluator, objects: []const Container) Error!Container {
    if (objects.len != 1) {
        return try evaluator.evalError(
            "Builtin signature mismatch. Expected 1 argument but found {d}",
            .{objects.len},
        );
    }

    std.debug.assert(objects[0] == .Value);

    const arg = objects[0].Value;

    switch (arg) {
        .String => |string| {
            var result: i64 = 0;
            for (0..string.value.len) |i| {
                if (string.value[i] == '\\' and string.value[i + 1] == '"') {
                    continue;
                }
                result += 1;
            }
            return .{ .Value = .{ .Integer = .{ .value = result } } };
        },
        .Array => |array| return .{ .Value = .{ .Integer = .{ .value = @intCast(array.values.len) } } },
        else => return try evaluator.evalError(
            "Builtin signature mismatch. Expected 'String' or 'Array' but found '{s}'",
            .{@tagName(arg)},
        ),
    }
}

fn first(evaluator: *Evaluator, objects: []const Container) Error!Container {
    if (objects.len != 1) {
        return try evaluator.evalError(
            "Builtin signature mismatch. Expected 1 argument but found {d}",
            .{objects.len},
        );
    }

    std.debug.assert(objects[0] == .Value);

    const arg = objects[0].Value;

    switch (arg) {
        .Array => |array| switch (array.values.len) {
            0 => return NULL,
            else => return .{ .Value = try array.values[0].copy(evaluator.allocator) },
        },
        else => return try evaluator.evalError(
            "Builtin signature mismatch. Expected 'String' or 'Array' but found '{s}'",
            .{@tagName(arg)},
        ),
    }
}

fn last(evaluator: *Evaluator, objects: []const Container) Error!Container {
    if (objects.len != 1) {
        return try evaluator.evalError(
            "Builtin signature mismatch. Expected 1 argument but found {d}",
            .{objects.len},
        );
    }

    std.debug.assert(objects[0] == .Value);

    const arg = objects[0].Value;

    switch (arg) {
        .Array => |array| switch (array.values.len) {
            0 => return NULL,
            else => return .{ .Value = try array.values[array.values.len - 1].copy(evaluator.allocator) },
        },
        else => return try evaluator.evalError(
            "Builtin signature mismatch. Expected 'String' or 'Array' but found '{s}'",
            .{@tagName(arg)},
        ),
    }
}

fn rest(evaluator: *Evaluator, objects: []const Container) Error!Container {
    if (objects.len != 1) {
        return try evaluator.evalError(
            "Builtin signature mismatch. Expected 1 argument but found {d}",
            .{objects.len},
        );
    }

    std.debug.assert(objects[0] == .Value);

    const arg = objects[0].Value;

    switch (arg) {
        .Array => |array| switch (array.values.len) {
            0 => return NULL,
            else => {
                var temp = Object.Value{ .Array = .{ .values = array.values[1..] } };
                return .{ .Value = try temp.copy(evaluator.allocator) };
            },
        },
        else => return try evaluator.evalError(
            "Builtin signature mismatch. Expected 'String' or 'Array' but found '{s}'",
            .{@tagName(arg)},
        ),
    }
}

fn push(evaluator: *Evaluator, objects: []const Container) Error!Container {
    if (objects.len != 2) {
        return try evaluator.evalError(
            "Builtin signature mismatch. Expected 2 argument but found {d}",
            .{objects.len},
        );
    }

    std.debug.assert(objects[0] == .Value);
    std.debug.assert(objects[1] == .Value);

    const arg1 = objects[0].Value;

    switch (arg1) {
        .Array => |array| {
            return .{ .Value = .{ .Array = try Object.Array.initAppend(evaluator.allocator, array.values, objects[1].Value) } };
        },
        else => return try evaluator.evalError(
            "Builtin signature mismatch. Expected 'Array' as first argument but found '{s}'",
            .{@tagName(arg1)},
        ),
    }
}
