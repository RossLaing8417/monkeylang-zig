const std = @import("std");

const Ast = @import("ast.zig");
const Environment = @import("environment.zig");
const Evaluator = @import("evaluator.zig");
const Builtin = @import("builtin.zig");

pub const Container = union(enum) {
    Value: Value,
    ReturnValue: Value,
    BuiltinFunction: BuiltinFunction,
    Error: Error,

    pub fn deinit(self: *Container, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .BuiltinFunction => {},
            inline else => |*container| container.deinit(allocator),
        }
    }

    pub fn copy(self: *Container, allocator: std.mem.Allocator) !Container {
        return switch (self.*) {
            .Value => |value| .{ .Value = try value.copy(allocator) },
            .ReturnValue => |literal| .{ .ReturnValue = try literal.copy(allocator) },
            .BuiltinFunction => unreachable,
            .Error => |err| .{ .Error = try err.copy(allocator) },
        };
    }

    pub fn inspect(self: *const Container, buffer: *std.ArrayList(u8)) !void {
        switch (self.*) {
            inline else => |container| try container.inspect(buffer),
        }
    }
};

pub const Error = struct {
    value: []const u8,

    pub fn deinit(self: *Error, allocator: std.mem.Allocator) void {
        allocator.free(self.value);
    }

    pub fn copy(self: *const Error, allocator: std.mem.Allocator) !Error {
        return .{ .value = try allocator.dupe(u8, self.value) };
    }

    pub fn inspect(self: *const Error, buffer: *std.ArrayList(u8)) !void {
        try buffer.writer().print("ERROR {s}", .{self.value});
    }
};

pub const BuiltinFunction = struct {
    name: []const u8,
    func: Builtin.Function,

    pub fn inspect(self: *const BuiltinFunction, buffer: *std.ArrayList(u8)) !void {
        try buffer.writer().print("@{s}(...args)", .{self.name});
    }
};

pub const Value = union(enum) {
    Null: Null,

    Integer: Integer,
    Boolean: Boolean,
    String: String,

    Function: Function,
    Array: Array,

    pub fn deinit(self: *Value, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Null, .Integer, .Boolean => {},
            inline else => |*value| value.deinit(allocator),
        }
    }

    pub fn copy(self: *const Value, allocator: std.mem.Allocator) !Value {
        return switch (self.*) {
            .String => |string| .{ .String = try string.copy(allocator) },
            .Array => |array| .{ .Array = try array.copy(allocator) },
            .Function => |function| .{ .Function = try function.copy(allocator) },
            else => self.*,
        };
    }

    pub fn inspect(self: *const Value, buffer: *std.ArrayList(u8)) !void {
        switch (self.*) {
            inline else => |literal| try literal.inspect(buffer),
        }
    }
};

pub const Null = struct {
    pub fn inspect(_: *const Null, buffer: *std.ArrayList(u8)) !void {
        try buffer.writer().writeAll("null");
    }
};

pub const Integer = struct {
    value: i64,

    pub fn inspect(self: *const Integer, buffer: *std.ArrayList(u8)) !void {
        try buffer.writer().print("{}", .{self.value});
    }
};

pub const Boolean = struct {
    value: bool,

    pub fn inspect(self: *const Boolean, buffer: *std.ArrayList(u8)) !void {
        try buffer.writer().print("{}", .{self.value});
    }
};

pub const String = struct {
    value: []const u8,
    owned: bool,

    pub fn initMerge(allocator: std.mem.Allocator, first: []const u8, second: []const u8) !String {
        var buffer = try allocator.alloc(u8, first.len + second.len);
        std.mem.copyForwards(u8, buffer, first);
        std.mem.copyForwards(u8, buffer[first.len..], second);
        return .{ .value = buffer, .owned = true };
    }

    pub fn deinit(self: *String, allocator: std.mem.Allocator) void {
        if (self.owned) {
            allocator.free(self.value);
        }
    }

    pub fn copy(self: *const String, allocator: std.mem.Allocator) !String {
        return .{ .value = try allocator.dupe(u8, self.value), .owned = true };
    }

    pub fn inspect(self: *const String, buffer: *std.ArrayList(u8)) !void {
        var writer = buffer.writer();
        for (0..self.value.len) |i| {
            // Skipping the \ if it escapce a "
            if (self.value[i] == '\\' and self.value[i + 1] == '"') {
                continue;
            }
            try writer.writeByte(self.value[i]);
        }
    }
};

pub const Function = struct {
    parameters: []const *Ast.Identifier,
    body: *Ast.BlockStatement,
    environment: *Environment,

    pub fn deinit(self: *Function, _: std.mem.Allocator) void {
        self.environment.decRef();
    }

    pub fn copy(self: *const Function, _: std.mem.Allocator) !Function {
        self.environment.incRef();
        return self.*;
    }

    pub fn inspect(self: *const Function, buffer: *std.ArrayList(u8)) !void {
        var writer = buffer.writer();
        try writer.writeAll("fn (");

        for (self.parameters, 0..) |identifier, i| {
            if (i > 0) {
                try writer.writeAll(", ");
            }

            try identifier.write(buffer, .none);
        }

        try writer.writeAll(")");

        try self.body.write(buffer, .none);
    }
};

pub const Array = struct {
    values: []Value,

    pub fn initAppend(allocator: std.mem.Allocator, from: []const Value, value: Value) !Array {
        var values = try allocator.alloc(Value, from.len + 1);
        for (0..from.len) |i| {
            values[i] = try from[i].copy(allocator);
        }
        values[from.len] = try value.copy(allocator);
        return .{ .values = values };
    }

    pub fn initCopy(allocator: std.mem.Allocator, from: []const Value) !Array {
        const values = try allocator.alloc(Value, from.len);
        for (values, from) |value, val| {
            value.* = try val.copy(allocator);
        }
        return .{ .values = values };
    }

    pub fn deinit(self: *Array, allocator: std.mem.Allocator) void {
        for (self.values) |*value| {
            switch (value.*) {
                .String => |*string| string.deinit(allocator),
                .Array => |*array| array.deinit(allocator),
                .Function => |*function| function.deinit(allocator),
                else => {},
            }
        }
        allocator.free(self.values);
    }

    pub fn copy(self: *const Array, allocator: std.mem.Allocator) std.mem.Allocator.Error!Array {
        const values = try allocator.alloc(Value, self.values.len);
        for (values, self.values) |*to, from| {
            to.* = try from.copy(allocator);
        }
        return .{ .values = values };
    }

    pub fn inspect(self: *const Array, buffer: *std.ArrayList(u8)) std.mem.Allocator.Error!void {
        var writer = buffer.writer();
        try writer.writeByte('[');
        for (self.values, 0..) |value, i| {
            if (i > 0) {
                try writer.writeAll(", ");
            }
            try value.inspect(buffer);
        }
        try writer.writeByte(']');
    }
};
