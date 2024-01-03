const std = @import("std");

const Environment = @This();

const Object = @import("object.zig");

const Map = std.StringHashMapUnmanaged(Object.Object);

arena: std.mem.Allocator,
store: Map,

pub fn init(arena: std.mem.Allocator) Environment {
    return .{
        .arena = arena,
        .store = Map{},
    };
}

pub fn deinit(self: *Environment) void {
    self.store.deinit(self.arena);
}

pub fn get(self: *Environment, name: []const u8) ?*Object.Object {
    return self.store.getPtr(name);
}

pub fn set(self: *Environment, name: []const u8, value: Object.Object) !*Object.Object {
    var _name = name;
    if (!self.store.contains(name)) {
        _name = try self.arena.dupe(u8, name);
    }

    try self.store.put(self.arena, _name, value);
    return self.store.getPtr(_name).?;
}
