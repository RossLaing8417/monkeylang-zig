pub const Object = union(enum) {
    Literal: Literal,
    ReturnValue: Literal,

    pub fn inspect(self: *const Object, writer: anytype) !void {
        switch (self.*) {
            inline else => |object| try object.inspect(writer),
        }
    }
};

pub const Literal = union(enum) {
    Integer: Integer,
    Boolean: Boolean,
    Null: Null,

    pub fn inspect(self: *const Literal, writer: anytype) !void {
        switch (self.*) {
            inline else => |literal| try literal.inspect(writer),
        }
    }
};

pub const Integer = struct {
    value: i64,

    pub fn inspect(self: *Integer, writer: anytype) !void {
        try writer.print("{}", .{self.value});
    }
};

pub const Boolean = struct {
    value: bool,

    pub fn inspect(self: *Boolean, writer: anytype) !void {
        try writer.print("{}", .{self.value});
    }
};

pub const Null = struct {
    pub fn inspect(_: *const Null, writer: anytype) !void {
        try writer.writeAll("null");
    }
};
