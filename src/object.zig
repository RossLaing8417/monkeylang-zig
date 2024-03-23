const std = @import("std");

const Ast = @import("ast.zig");
const Environment = @import("environment.zig");
const Evaluator = @import("evaluator.zig");

pub const Object = union(enum) {
    Literal: Literal,
    ReturnValue: Literal,
    Error: Error,

    pub fn inspect(self: *const Object, buffer: *std.ArrayList(u8)) !void {
        switch (self.*) {
            inline else => |object| try object.inspect(buffer),
        }
    }
};

pub const Literal = union(enum) {
    Integer: Integer,
    Boolean: Boolean,
    String: String,
    Function: Function,
    Null: Null,

    pub fn inspect(self: *const Literal, buffer: *std.ArrayList(u8)) !void {
        switch (self.*) {
            inline else => |literal| try literal.inspect(buffer),
        }
    }
};

pub const Error = struct {
    value: []const u8,

    pub fn inspect(self: *const Error, buffer: *std.ArrayList(u8)) !void {
        try buffer.writer().print("ERROR {s}", .{self.value});
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

pub const BuiltInFunction = struct {
    name: []const u8,
    func: fn (*Evaluator, []const Object) Object,

    pub fn inspect(self: *const BuiltInFunction, buffer: *std.ArrayList(u8)) !void {
        var writer = buffer.writer();
        try writer.print("@{s}(...args)", .{self.name});
    }
};

pub const Null = struct {
    pub fn inspect(_: *const Null, buffer: *std.ArrayList(u8)) !void {
        try buffer.writer().writeAll("null");
    }
};
