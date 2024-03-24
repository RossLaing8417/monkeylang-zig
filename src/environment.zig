const std = @import("std");

const Environment = @This();

const Object = @import("object.zig");

const Value = Object.Value;
const Map = std.StringHashMap(Value);

allocator: std.mem.Allocator,
store: Map,
outer: ?*Environment,
ref_count: usize,
local_functions: usize,

pub fn init(allocator: std.mem.Allocator) !*Environment {
    var environment = try allocator.create(Environment);
    environment.* = .{
        .allocator = allocator,
        .store = Map.init(allocator),
        .outer = null,
        .ref_count = 0,
        .local_functions = 0,
    };

    environment.incRef();

    return environment;
}

pub fn initEnclosed(allocator: std.mem.Allocator, outer: *Environment) !*Environment {
    var environment = try init(allocator);
    environment.outer = outer;
    return environment;
}

pub fn deinit(self: *Environment) void {
    std.debug.assert(self.ref_count == 0);
    var itr = self.store.valueIterator();
    while (itr.next()) |value| {
        value.deinit(self.allocator);
    }
    self.store.deinit();
    self.allocator.destroy(self);
}

pub fn incRef(self: *Environment) void {
    self.ref_count += 1;
    // std.debug.print("REF: {*} -> {d} ({d})\n", .{ self, self.ref_count, self.local_functions });
    // std.debug.dumpCurrentStackTrace(null);
}

pub fn decRef(self: *Environment) void {
    if (self.ref_count == 0 and self.local_functions > 0) {
        return;
    }
    self.ref_count -= 1;
    // std.debug.print("REF: {*} -> {d} ({d})\n", .{ self, self.ref_count, self.local_functions });
    // std.debug.dumpCurrentStackTrace(null);
    if (self.ref_count == 0) {
        self.deinit();
    } else if (self.ref_count == self.local_functions) {
        self.ref_count = 0;
        self.deinit();
    }
}

pub fn get(self: *Environment, name: []const u8) ?*Value {
    if (self.store.getPtr(name)) |value| {
        return value;
    }
    if (self.outer) |outer| {
        return outer.get(name);
    }
    return null;
}

pub fn set(self: *Environment, name: []const u8, value: Value) !void {
    if (self.store.getPtr(name)) |existing| {
        if (existing.* == .Function and existing.Function.environment == self) {
            self.local_functions -= 1;
        }
        existing.deinit(self.allocator);
    }
    if (value == .Function and value.Function.environment == self) {
        self.local_functions += 1;
    }
    try self.store.put(name, value);
}
