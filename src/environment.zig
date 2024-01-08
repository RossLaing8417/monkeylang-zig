const std = @import("std");

const Environment = @This();

const Object = @import("object.zig");

const Map = std.StringHashMap(Object.Object);

arena: std.mem.Allocator,
store: Map,
outer: ?*Environment,

pub fn init(arena: std.mem.Allocator) !*Environment {
    var environment = try arena.create(Environment);
    environment.* = .{
        .arena = arena,
        .store = Map.init(arena),
        .outer = null,
    };

    return environment;
}

pub fn initEnclosed(arena: std.mem.Allocator, outer: *Environment) !*Environment {
    var environment = try arena.create(Environment);
    environment.* = .{
        .arena = arena,
        .store = Map.init(arena),
        .outer = outer,
    };

    return environment;
}

pub fn deinit(self: *Environment) void {
    self.store.deinit();
    self.arena.destroy(self);
}

pub fn get(self: *Environment, name: []const u8) ?*Object.Object {
    std.log.info("Get name? {s} @ {*}", .{ name, self });

    if (self.store.getPtr(name)) |object| {
        return object;
    }

    if (self.outer) |outer| {
        return outer.get(name);
    }

    return null;
}

pub fn set(self: *Environment, name: []const u8, value: Object.Object) !void {
    std.log.info("Set name? {s} @ {*}", .{ name, self });

    var _name = name;

    if (!self.store.contains(name)) {
        _name = try self.arena.dupe(u8, name);
    }

    try self.store.put(_name, value);
}
