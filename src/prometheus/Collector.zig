const std = @import("std");
const testing = std.testing;

const Collector = @This();

ptr: *anyopaque,
vtb: *const VTable,

pub const VTable = struct {
    /// This function must be safe to call from another thread.
    collect: *const fn (ptr: *anyopaque, arena: std.mem.Allocator) error{OutOfMemory}![]MetricFamily,
};

/// This function is safe to call from another thread.
pub inline fn collect(self: Collector, arena: std.mem.Allocator) error{OutOfMemory}![]MetricFamily {
    return self.vtb.collect(self.ptr, arena);
}

pub const MetricFamily = struct {
    name: []const u8,
    help: []const u8,
    type: MetricType,
    data: []const Metric,
};

pub const MetricType = enum { counter, gauge };

pub const Metric = struct {
    value: MetricValue,
    labels: []const struct { []const u8, []const u8 },
};

pub const MetricValue = union(MetricType) {
    counter: f64,
    gauge: f64,
};

pub const TestCollector = struct {
    mutex: std.Thread.Mutex = .{},

    metrics: []const MetricFamily,

    pub fn collector(c: *TestCollector) Collector {
        return .{ .ptr = c, .vtb = &.{
            .collect = TestCollector.collect,
        } };
    }

    fn collect(ptr: *anyopaque, arena: std.mem.Allocator) error{OutOfMemory}![]MetricFamily {
        const c: *TestCollector = @alignCast(@ptrCast(ptr));
        c.mutex.lock();
        defer c.mutex.unlock();

        const metrics = try arena.alloc(MetricFamily, c.metrics.len);

        for (metrics, c.metrics) |*m_out, m_in| {
            const data = try arena.alloc(Metric, m_in.data.len);

            for (data, m_in.data) |*d_out, d_in| {
                const labels = try arena.alloc(struct { []const u8, []const u8 }, d_in.labels.len);
                for (labels, d_in.labels) |*l_out, l_in| {
                    l_out.* = .{ try arena.dupe(u8, l_in[0]), try arena.dupe(u8, l_in[1]) };
                }

                d_out.* = .{
                    .value = d_in.value,
                    .labels = labels,
                };
            }

            m_out.* = .{
                .name = try arena.dupe(u8, m_in.name),
                .help = try arena.dupe(u8, m_in.help),
                .type = m_in.type,
                .data = data,
            };
        }

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
                        .labels = &.{ .{ "kind", "tcp" }, .{ "direction", "rx" } },
                    },
                    .{
                        .value = .{ .counter = 110.0 },
                        .labels = &.{ .{ "kind", "udp" }, .{ "direction", "tx" } },
                    },
                    .{
                        .value = .{ .counter = 0.0 },
                        .labels = &.{ .{ "kind", "tcp" }, .{ "direction", "tx" } },
                    },
                },
            },
        },
    };

    const collector = c.collector();
    _ = collector;
}
