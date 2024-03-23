const std = @import("std");
const testing = std.testing;

const Collector = @This();

const Metric = @import("Metric.zig");

ptr: *anyopaque,
vtb: *const VTable,

pub const VTable = struct {
    /// This function must be safe to call from another thread.
    collect: *const fn (ptr: *anyopaque, arena: std.mem.Allocator) error{OutOfMemory}![]Metric.Family,
};

/// This function is safe to call from another thread.
pub inline fn collect(self: Collector, arena: std.mem.Allocator) error{OutOfMemory}![]Metric.Family {
    return self.vtb.collect(self.ptr, arena);
}

pub const TestCollector = struct {
    mutex: std.Thread.Mutex = .{},

    metrics: []const Metric.Family,

    pub fn collector(c: *TestCollector) Collector {
        return .{ .ptr = c, .vtb = &.{
            .collect = TestCollector.collect,
        } };
    }

    fn collect(ptr: *anyopaque, arena: std.mem.Allocator) error{OutOfMemory}![]Metric.Family {
        const c: *TestCollector = @alignCast(@ptrCast(ptr));
        c.mutex.lock();
        defer c.mutex.unlock();

        const metrics = try arena.alloc(Metric.Family, c.metrics.len);
        for (metrics, c.metrics) |*m_out, m_in| m_out.* = try m_in.dupe(arena);
        return metrics;
    }
};

test TestCollector {
    var c: TestCollector = .{
        .metrics = &.{
            .{
                .name = "observed_packets",
                .help = "The number of packets",
                .type = .counter,
                .data = &.{
                    .{
                        .value = .{ .counter = 1.0 },
                        .labels = &.{ .{ .n = "kind", .v = "tcp" }, .{ .n = "direction", .v = "rx" } },
                    },
                    .{
                        .value = .{ .counter = 110.0 },
                        .labels = &.{ .{ .n = "kind", .v = "udp" }, .{ .n = "direction", .v = "tx" } },
                    },
                    .{
                        .value = .{ .counter = 0.0 },
                        .labels = &.{ .{ .n = "kind", .v = "tcp" }, .{ .n = "direction", .v = "tx" } },
                    },
                },
            },
        },
    };

    const collector = c.collector();
    _ = collector;
}
