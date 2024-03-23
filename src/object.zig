const std = @import("std");

const Ast = @import("ast.zig");
const Environment = @import("environment.zig");

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

pub const Null = struct {
    pub fn inspect(_: *const Null, buffer: *std.ArrayList(u8)) !void {
        try buffer.writer().writeAll("null");
    }
};
