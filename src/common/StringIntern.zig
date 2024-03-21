const std = @import("std");

const StringIntern = @This();

arena_state: std.heap.ArenaAllocator.State = .{},
set: std.StringHashMapUnmanaged(void) = .{},

pub fn deinit(string_intern: *StringIntern, gpa: std.mem.Allocator) void {
    string_intern.set.deinit(gpa);
    string_intern.arena_state.promote(gpa).deinit();
}

pub fn intern(
    self: *StringIntern,
    gpa: std.mem.Allocator,
    s: []const u8,
) error{OutOfMemory}![]const u8 {
    const key_ptr = blk: {
        const gop = try self.set.getOrPut(gpa, s);
        if (gop.found_existing) return gop.key_ptr.*;
        break :blk gop.key_ptr;
    };
    errdefer self.set.removeByPtr(key_ptr);
    return try self.store(gpa, s, key_ptr);
}

fn store(
    self: *StringIntern,
    gpa: std.mem.Allocator,
    s: []const u8,
    ptr: *[]const u8,
) error{OutOfMemory}![]const u8 {
    @setCold(true);

    var arena_state = self.arena_state.promote(gpa);
    defer self.arena_state = arena_state.state;
    const arena = arena_state.allocator();

    const alloced_s = try arena.dupe(u8, s);
    ptr.* = alloced_s;
    return alloced_s;
}
