const std = @import("std");

const Exposer = @This();

const Collector = @import("Collector.zig");

const Metric = @import("Metric.zig");

mutex: std.Thread.Mutex = .{},

gpa: std.mem.Allocator,
collectables: std.ArrayListUnmanaged(Collector) = .{},

stats: Statistics = .{},

exhibitionism: ?InitOptions.Exhibitionism = null,

pub fn init(exp: *Exposer, gpa: std.mem.Allocator, opts: InitOptions) error{OutOfMemory}!void {
    exp.* = .{ .gpa = gpa };

    if (opts.exhibitionism) |exh| {
        const prefix = try gpa.dupe(u8, exh.prefix);
        errdefer gpa.free(prefix);

        var labels = try std.ArrayListUnmanaged(Metric.Label).initCapacity(gpa, exh.labels.len);
        errdefer {
            for (0..labels.items.len) |i| labels.items[labels.items.len - 1 - i].deinit(gpa);
            labels.deinit(gpa);
        }

        for (exh.labels) |l| labels.appendAssumeCapacity(try l.dupe(gpa));

        exp.exhibitionism = .{
            .prefix = prefix,
            .labels = labels.toOwnedSlice(gpa) catch unreachable,
        };
    }
}

pub const InitOptions = struct {
    /// Configuration of how the exposer should expose it's own scraping metrics.
    /// If null the exposer will not expose itself.
    exhibitionism: ?Exhibitionism = null,

    pub const Exhibitionism = struct {
        /// A string to be prefixed to every exposer metric.
        prefix: []const u8 = "",

        /// A list of labels to be added to every exposer metric.
        labels: []const Metric.Label = &.{},
    };
};

pub fn deinit(exp: *Exposer) void {
    if (exp.exhibitionism) |exh| {
        for (exh.labels) |l| l.deinit(exp.gpa);
        exp.gpa.free(exh.labels);
        exp.gpa.free(exh.prefix);
    }
    exp.collectables.deinit(exp.gpa);
}

/// The collectable must live for at least as long as the exposer.
pub fn add(exp: *Exposer, collectable: Collector) error{OutOfMemory}!void {
    exp.mutex.lock();
    defer exp.mutex.unlock();

    try exp.collectables.append(exp.gpa, collectable);
}

pub fn collectAndWriteAll(exp: *Exposer, w: anytype) !void {
    exp.mutex.lock();
    defer exp.mutex.unlock();

    var arena_state = std.heap.ArenaAllocator.init(exp.gpa);
    defer arena_state.deinit();

    try writeAll(w, try exp.collectAll(arena_state.allocator()));
}

fn collectAll(exp: *Exposer, arena: std.mem.Allocator) !std.StringArrayHashMapUnmanaged(MetricFamily) {
    const expose_self = exp.exhibitionism != null;

    var families: std.StringArrayHashMapUnmanaged(MetricFamily) = .{};
    for (exp.collectables.items) |c| try exp.collectSingle(arena, &families, c);
    if (expose_self) try exp.collectSingle(arena, &families, .{ .ptr = exp, .vtb = &.{ .collect = collectSelf } });
    return families;
}

fn collectSingle(
    exp: *Exposer,
    arena: std.mem.Allocator,
    families: *std.StringArrayHashMapUnmanaged(MetricFamily),
    c: Collector,
) !void {
    const families_collected = try c.collect(arena);

    try families.ensureUnusedCapacity(arena, families_collected.len);
    loop_family: for (families_collected) |f_collected| {
        exp.stats.families_collected_count_total += 1;
        exp.stats.metrics_collected_count_total += f_collected.data.len;

        if (f_collected.data.len == 0) {
            continue :loop_family;
        }

        if (!f_collected.isValidName()) {
            exp.stats.families_failed_count_total += 1;
            exp.stats.metrics_failed_count_total += f_collected.data.len;
            continue :loop_family;
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
            exp.stats.families_failed_count_total += 1;
            exp.stats.metrics_failed_count_total += f_collected.data.len;
            continue :loop_family;
        }

        if (!std.mem.eql(u8, f_collected.help, f.help)) {
            exp.stats.families_failed_count_total += 1;
            exp.stats.metrics_failed_count_total += f_collected.data.len;
            continue :loop_family;
        }

        try f.data.ensureUnusedCapacity(arena, f_collected.data.len);
        loop_metric: for (f_collected.data) |m_collected| {
            if (std.meta.activeTag(m_collected.value) != f.type) {
                exp.stats.metrics_failed_count_total += 1;
                continue :loop_metric;
            }

            for (m_collected.labels) |l_collected| {
                if (!l_collected.isValid()) {
                    exp.stats.metrics_failed_count_total += 1;
                    continue :loop_metric;
                }
            }

            // Collect all non-empty values.
            const labels = blk_labels: {
                var labels = try std.ArrayListUnmanaged(Metric.Label).initCapacity(arena, m_collected.labels.len);
                for (m_collected.labels) |l_collected| if (l_collected.v.len > 0) labels.appendAssumeCapacity(l_collected);
                break :blk_labels labels.toOwnedSlice(arena) catch unreachable;
            };

            std.sort.pdq(
                Metric.Label,
                labels,
                {},
                struct {
                    fn lessThan(_: void, a: Metric.Label, b: Metric.Label) bool {
                        return std.mem.order(u8, a.n, b.n) == .lt;
                    }
                }.lessThan,
            );

            const gop = f.data.getOrPutAssumeCapacity(labels);
            if (gop.found_existing) {
                exp.stats.metrics_failed_count_total += 1;
                continue :loop_metric;
            }
            gop.value_ptr.* = m_collected.value;
        }
    }
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
    labels: []const Metric.Label,
    value: f64,
) !void {
    try w.writeAll(name);
    if (labels.len > 0) {
        try w.writeByte('{');
        for (labels) |l| {
            try w.print("{s}=\"", .{l.n});
            for (l.v) |ch| {
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

const Statistics = struct {
    families_collected_count_total: u64 = 0,
    families_failed_count_total: u64 = 0,
    metrics_collected_count_total: u64 = 0,
    metrics_failed_count_total: u64 = 0,

    const help = struct {
        pub const families_collected_count_total: []const u8 = "The metric families collected";
        pub const families_failed_count_total: []const u8 = "The collected metric families that failed to be exposed";
        pub const metrics_collected_count_total: []const u8 = "The metrics collected";
        pub const metrics_failed_count_total: []const u8 = "The collected metrics that failed to be exposed";
    };
};

fn collectSelf(ptr: *anyopaque, arena: std.mem.Allocator) error{OutOfMemory}![]Metric.Family {
    const exp: *Exposer = @alignCast(@ptrCast(ptr));
    const exh = exp.exhibitionism.?;

    // We do not lock because we can only be called from within ourselves while already holding the lock.

    const stats_info = @typeInfo(Statistics).Struct;

    const families = try arena.alloc(Metric.Family, stats_info.fields.len);

    inline for (stats_info.fields, 0..) |f, i| {
        const h = @field(Statistics.help, f.name);

        families[i] = .{
            .name = try std.mem.concat(arena, u8, &.{ exh.prefix, f.name }),
            .help = h,
            .type = .counter,
            .data = blk_data: {
                const metric: Metric = .{
                    .value = .{ .counter = @floatFromInt(@field(exp.stats, f.name)) },
                    .labels = exh.labels,
                };
                break :blk_data try arena.dupe(Metric, &.{try metric.dupe(arena)});
            },
        };
    }

    return families;
}

const MetricFamily = struct {
    name: []const u8,
    help: []const u8,
    type: Metric.Type,
    data: std.ArrayHashMapUnmanaged([]const Metric.Label, Metric.Value, HashMapContext, true),

    const HashMapContext = struct {
        pub fn hash(_: HashMapContext, x: []const Metric.Label) u32 {
            var h = std.hash.Wyhash.init(0);
            h.update(std.mem.asBytes(&x.len));
            for (x) |y| {
                h.update(std.mem.asBytes(&y.n.len));
                h.update(y.n);
                h.update(std.mem.asBytes(&y.v.len));
                h.update(y.v);
            }
            return @truncate(h.final());
        }

        pub fn eql(_: HashMapContext, a: []const Metric.Label, b: []const Metric.Label, _: usize) bool {
            if (a.len != b.len) return false;
            for (a, b) |aa, bb| {
                if (!std.mem.eql(u8, aa.n, bb.n) or !std.mem.eql(u8, aa.v, aa.v)) {
                    return false;
                }
            }
            return true;
        }
    };
};

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
                    .labels = &.{ .{ .n = "kind", .v = "tcp" }, .{ .n = "direction", .v = "rx" } },
                },
                .{
                    .value = .{ .counter = 110.0 },
                    .labels = &.{.{ .n = "kind", .v = "udp" }},
                },
                .{
                    .value = .{ .counter = 0.0 },
                    .labels = &.{ .{ .n = "kind" }, .{ .n = "direction", .v = "tx" } },
                },
                .{
                    .value = .{ .counter = 1.0 },
                    .labels = &.{.{ .n = "__asd", .v = "udp" }},
                },
                .{
                    .value = .{ .counter = 2.0 },
                    .labels = &.{.{ .n = "kind%", .v = "udp" }},
                },
            },
        },
    } };

    var buf: [64]u8 = undefined;
    const pid_s = try std.fmt.bufPrint(&buf, "{}", .{1}); // TODO: uhh how do i get crossplatform pid

    var exposer: Exposer = undefined;
    try exposer.init(testing.allocator, .{
        .exhibitionism = .{
            .prefix = "pxx_",
            .labels = &.{
                .{ .n = "pid", .v = pid_s },
            },
        },
    });
    defer exposer.deinit();

    try exposer.add(c.collector());

    const stderr_unbuffered = std.io.getStdErr().writer();
    var buffer = std.io.bufferedWriter(stderr_unbuffered);
    try exposer.collectAndWriteAll(buffer.writer());
    try buffer.flush();

    try testing.expectEqual(@as(u64, 1 + 4), exposer.stats.families_collected_count_total);
    try testing.expectEqual(@as(u64, 0 + 0), exposer.stats.families_failed_count_total);
    try testing.expectEqual(@as(u64, 5 + 4), exposer.stats.metrics_collected_count_total);
    try testing.expectEqual(@as(u64, 2 + 0), exposer.stats.metrics_failed_count_total);
}
