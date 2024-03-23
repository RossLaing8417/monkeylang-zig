const std = @import("std");

const Environment = @This();

const Object = @import("object.zig").Object;

const Map = std.StringHashMap(Object);

store: Map,
outer: ?*Environment,

pub fn init(allocator: std.mem.Allocator) !*Environment {
    var environment = try allocator.create(Environment);
    environment.* = .{
        .store = Map.init(allocator),
        .outer = null,
    };

    return environment;
}

pub fn initEnclosed(allocator: std.mem.Allocator, outer: *Environment) !*Environment {
    var environment = try init(allocator);
    environment.outer = outer;
    return environment;
}

pub fn deinit(self: *Environment, allocator: std.mem.Allocator) void {
    self.store.deinit();
    allocator.destroy(self);
}

pub fn get(self: *Environment, name: []const u8) ?*Object {
    if (self.store.getPtr(name)) |object| {
        return object;
    }
    if (self.outer) |outer| {
        return outer.get(name);
    }
    return null;
}

pub fn set(self: *Environment, name: []const u8, value: Object) !void {
    try self.store.put(name, value);
}
