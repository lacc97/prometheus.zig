const std = @import("std");

const Exposer = @This();

const Collector = @import("Collector.zig");

mutex: std.Thread.Mutex = .{},

gpa: std.mem.Allocator,
collectables: std.ArrayListUnmanaged(Collector) = .{},

pub fn deinit(exposer: *Exposer) void {
    exposer.collectables.deinit(exposer.gpa);
}

/// The collectable must live for at least as long as the exposer.
pub fn add(exposer: *Exposer, collectable: Collector) error{OutOfMemory}!void {
    exposer.mutex.lock();
    defer exposer.mutex.unlock();

    try exposer.collectables.append(exposer.gpa, collectable);
}

pub fn collectAndWriteAll(exposer: *Exposer, w: anytype) !void {
    exposer.mutex.lock();
    defer exposer.mutex.unlock();

    var arena_state = std.heap.ArenaAllocator.init(exposer.gpa);
    defer arena_state.deinit();

    try writeAll(w, try collectAll(arena_state.allocator(), exposer.collectables.items));
}

fn collectAll(arena: std.mem.Allocator, collectors: []const Collector) !std.StringArrayHashMapUnmanaged(MetricFamily) {
    var families_collected_count: u64 = 0;
    var families_skipped_invalid_name_count: u64 = 0;
    var families_skipped_mismatched_help_count: u64 = 0;
    var families_skipped_mismatched_type_count: u64 = 0;
    var metrics_collected_count: u64 = 0;
    var metrics_skipped_count: u64 = 0;
    var metrics_skipped_mismatched_type_count: u64 = 0;
    var metrics_skipped_invalid_labels_count: u64 = 0;
    var metrics_skipped_duplicate_count: u64 = 0;

    var families: std.StringArrayHashMapUnmanaged(MetricFamily) = .{};

    for (collectors) |c| {
        const families_collected = try c.collect(arena);

        try families.ensureUnusedCapacity(arena, families_collected.len);
        for (families_collected) |f_collected| {
            families_collected_count += 1;
            metrics_collected_count += f_collected.data.len;

            if (f_collected.data.len == 0) {
                continue;
            }

            if (!isValidMetricName(f_collected.name)) {
                families_skipped_invalid_name_count += 1;
                metrics_skipped_count += f_collected.data.len;
                continue;
            }

            const f = blk_f: {
                const gop = families.getOrPutAssumeCapacity(f_collected.name);
                if (!gop.found_existing) gop.value_ptr.* = .{
                    .name = f_collected.name,
                    .help = f_collected.help,
                    .type = f_collected.type,
                    .data = .{},
                };
                break :blk_f gop.value_ptr;
            };

            if (f_collected.type != f.type) {
                families_skipped_mismatched_type_count += 1;
                metrics_skipped_count += f_collected.data.len;
                continue;
            }

            if (!std.mem.eql(u8, f_collected.help, f.help)) {
                families_skipped_mismatched_help_count += 1;
                metrics_skipped_count += f_collected.data.len;
                continue;
            }

            try f.data.ensureUnusedCapacity(arena, f_collected.data.len);
            for (f_collected.data) |m_collected| {
                if (std.meta.activeTag(m_collected.value) != f.type) {
                    metrics_skipped_mismatched_type_count += 1;
                    continue;
                }

                if (!isValidLabels(m_collected.labels)) {
                    metrics_skipped_invalid_labels_count += 1;
                    continue;
                }

                // Collect all non-empty values.
                const labels = blk_labels: {
                    var labels = try std.ArrayListUnmanaged(Label).initCapacity(arena, m_collected.labels.len);
                    for (m_collected.labels) |l_collected| if (l_collected[1].len > 0) labels.appendAssumeCapacity(l_collected);
                    break :blk_labels labels.toOwnedSlice(arena) catch unreachable;
                };

                std.sort.pdq(
                    Label,
                    labels,
                    {},
                    struct {
                        fn lessThan(_: void, a: Label, b: Label) bool {
                            return std.mem.order(u8, a[0], b[0]) == .lt;
                        }
                    }.lessThan,
                );

                const gop = f.data.getOrPutAssumeCapacity(labels);
                if (gop.found_existing) {
                    metrics_skipped_duplicate_count += 1;
                    continue;
                }
                gop.value_ptr.* = m_collected.value;
            }
        }
    }

    return families;
}

fn writeAll(w: anytype, families: std.StringArrayHashMapUnmanaged(MetricFamily)) !void {
    for (families.values()) |family| {
        if (family.help.len > 0) {
            try w.print("# HELP {s} {s}\n", .{ family.name, family.help });
        }
        try w.print("# TYPE {s} {s}\n", .{ family.name, @tagName(family.type) });
        for (family.data.values(), family.data.keys()) |val, lab| {
            switch (val) {
                .counter, .gauge => |v| try writeFloatMetric(w, family.name, lab, v),
            }
        }
    }
}

fn writeFloatMetric(
    w: anytype,
    name: []const u8,
    labels: []const Label,
    value: f64,
) !void {
    try w.writeAll(name);
    if (labels.len > 0) {
        try w.writeByte('{');
        for (labels) |l| {
            try w.print("{s}=\"", .{l[0]});
            for (l[1]) |ch| {
                const ch0: u8 = switch (ch) {
                    '\n' => blk: {
                        try w.writeByte('\\');
                        break :blk 'n';
                    },
                    '\\' => blk: {
                        try w.writeByte('\\');
                        break :blk '\\';
                    },
                    else => ch,
                };
                try w.writeByte(ch0);
            }
            try w.writeAll("\",");
        }
        try w.writeByte('}');
    }

    try w.print(" {}\n", .{value});
}

const MetricFamily = struct {
    name: []const u8,
    help: []const u8,
    type: MetricType,
    data: std.ArrayHashMapUnmanaged([]const Label, MetricValue, HashMapContext, true),

    const HashMapContext = struct {
        pub fn hash(_: HashMapContext, x: []const Label) u32 {
            var h = std.hash.Wyhash.init(0);
            h.update(std.mem.asBytes(&x.len));
            for (x) |y| {
                h.update(std.mem.asBytes(&y[0].len));
                h.update(y[0]);
                h.update(std.mem.asBytes(&y[1].len));
                h.update(y[1]);
            }
            return @truncate(h.final());
        }

        pub fn eql(_: HashMapContext, a: []const Label, b: []const Label, _: usize) bool {
            if (a.len != b.len) return false;
            for (a, b) |aa, bb| {
                if (!std.mem.eql(u8, aa[0], bb[0]) or !std.mem.eql(u8, aa[1], aa[1])) {
                    return false;
                }
            }
            return true;
        }
    };
};

const MetricType = Collector.MetricType;
const Metric = Collector.Metric;
const MetricValue = Collector.MetricValue;
const Label = struct { []const u8, []const u8 };

fn isValidMetricName(name: []const u8) bool {
    _ = name; // autofix
    // TODO:
    return true;
}

fn isValidLabels(labels: []const Label) bool {
    _ = labels; // autofix
    // TODO:
    return true;
}

test collectAndWriteAll {
    const testing = std.testing;

    var c: Collector.TestCollector = .{ .metrics = &.{
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
                    .labels = &.{.{ "kind", "udp" }},
                },
                .{
                    .value = .{ .counter = 0.0 },
                    .labels = &.{ .{ "kind", "" }, .{ "direction", "tx" } },
                },
            },
        },
    } };

    var exposer: Exposer = .{ .gpa = testing.allocator };
    defer exposer.deinit();

    try exposer.add(c.collector());

    const stderr_unbuffered = std.io.getStdErr().writer();
    var buffer = std.io.bufferedWriter(stderr_unbuffered);
    try exposer.collectAndWriteAll(buffer.writer());
    try buffer.flush();
}
