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
    /// The metric name must match the prometheus naming rules https://prometheus.io/docs/concepts/data_model/#metric-names-and-labels.
    name: []const u8,
    help: []const u8,
    type: MetricType,
    data: []const Metric,

    /// Frees the name, help and data arrays, which must have been allocated with the provided allocator.
    /// Does not free the underlying metrics in data.
    pub fn deinit(metric_family: MetricFamily, gpa: std.mem.Allocator) void {
        gpa.free(metric_family.data);
        gpa.free(metric_family.help);
        gpa.free(metric_family.name);
    }

    /// Dupes the metric family, including the metrics in data (i.e. performs a deep copy).
    pub fn dupe(metric_family: MetricFamily, gpa: std.mem.Allocator) error{OutOfMemory}!MetricFamily {
        const name = try gpa.dupe(u8, metric_family.name);
        errdefer gpa.free(name);

        const help = try gpa.dupe(u8, metric_family.help);
        errdefer gpa.free(help);

        var data = try std.ArrayListUnmanaged(Metric).initCapacity(gpa, metric_family.data.len);
        errdefer {
            for (0..data.items.len) |i| data.items[data.items.len - 1 - i].deinit(gpa);
            data.deinit(gpa);
        }

        for (metric_family.data) |m| data.appendAssumeCapacity(try m.dupe(gpa));

        return .{
            .name = name,
            .help = help,
            .type = metric_family.type,
            .data = data.toOwnedSlice(gpa) catch unreachable,
        };
    }
};

pub const MetricType = enum { counter, gauge };

pub const Metric = struct {
    value: MetricValue,
    labels: []const MetricLabel,

    /// Frees the label array, which must have been allocated with the provided allocator. Does not free the underlying labels.
    pub fn deinit(metric: Metric, gpa: std.mem.Allocator) void {
        gpa.free(metric.labels);
    }

    /// Dupes the metric, including the labels and their contents (i.e. performs a deep copy).
    pub fn dupe(metric: Metric, gpa: std.mem.Allocator) error{OutOfMemory}!Metric {
        var labels = try std.ArrayListUnmanaged(MetricLabel).initCapacity(gpa, metric.labels.len);
        errdefer {
            for (0..labels.items.len) |i| labels.items[labels.items.len - 1 - i].deinit(gpa);
            labels.deinit(gpa);
        }

        for (metric.labels) |l| labels.appendAssumeCapacity(try l.dupe(gpa));

        return .{
            .value = metric.value,
            .labels = labels.toOwnedSlice(gpa) catch unreachable,
        };
    }
};

pub const MetricLabel = struct {
    /// The label name (must match the prometheus naming rules https://prometheus.io/docs/concepts/data_model/#metric-names-and-labels).
    n: []const u8,

    /// The label value (may be empty).
    v: []const u8 = "",

    /// Frees the name and value, both of which must have been allocated with the provided allocator.
    pub fn deinit(label: MetricLabel, gpa: std.mem.Allocator) void {
        gpa.free(label.v);
        gpa.free(label.n);
    }

    /// Dupes the label, allocating the new name and value using the provided allocator.
    pub fn dupe(label: MetricLabel, gpa: std.mem.Allocator) error{OutOfMemory}!MetricLabel {
        const n = try gpa.dupe(u8, label.n);
        errdefer gpa.free(n);

        const v = try gpa.dupe(u8, label.v);
        errdefer gpa.free(v);

        return .{ .n = n, .v = v };
    }
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
