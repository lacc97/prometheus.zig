const std = @import("std");
const testing = std.testing;

const Metric = @This();

value: Value,
labels: []const Label,

/// Frees the label array, which must have been allocated with the provided allocator. Does not free the underlying labels.
pub fn deinit(metric: Metric, gpa: std.mem.Allocator) void {
    gpa.free(metric.labels);
}

/// Dupes the metric, including the labels and their contents (i.e. performs a deep copy).
pub fn dupe(metric: Metric, gpa: std.mem.Allocator) error{OutOfMemory}!Metric {
    var labels = try std.ArrayListUnmanaged(Label).initCapacity(gpa, metric.labels.len);
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

pub const Type = enum {
    counter,
    gauge,
};

pub const Family = struct {
    /// The metric name must match the prometheus naming rules https://prometheus.io/docs/concepts/data_model/#metric-names-and-labels.
    name: []const u8,
    help: []const u8,
    type: Type,
    data: []const Metric,

    /// Frees the name, help and data arrays, which must have been allocated with the provided allocator.
    /// Does not free the underlying metrics in data.
    pub fn deinit(family: Family, gpa: std.mem.Allocator) void {
        gpa.free(family.data);
        gpa.free(family.help);
        gpa.free(family.name);
    }

    /// Dupes the metric family, including the metrics in data (i.e. performs a deep copy).
    pub fn dupe(family: Family, gpa: std.mem.Allocator) error{OutOfMemory}!Family {
        const name = try gpa.dupe(u8, family.name);
        errdefer gpa.free(name);

        const help = try gpa.dupe(u8, family.help);
        errdefer gpa.free(help);

        var data = try std.ArrayListUnmanaged(Metric).initCapacity(gpa, family.data.len);
        errdefer {
            for (0..data.items.len) |i| data.items[data.items.len - 1 - i].deinit(gpa);
            data.deinit(gpa);
        }

        for (family.data) |m| data.appendAssumeCapacity(try m.dupe(gpa));

        return .{
            .name = name,
            .help = help,
            .type = family.type,
            .data = data.toOwnedSlice(gpa) catch unreachable,
        };
    }
};

pub const Value = union(Type) {
    counter: f64,
    gauge: f64,
};

pub const Label = struct {
    /// The label name (must match the prometheus naming rules https://prometheus.io/docs/concepts/data_model/#metric-names-and-labels).
    n: []const u8,

    /// The label value (may be empty).
    v: []const u8 = "",

    /// Frees the name and value, both of which must have been allocated with the provided allocator.
    pub fn deinit(label: Label, gpa: std.mem.Allocator) void {
        gpa.free(label.v);
        gpa.free(label.n);
    }

    /// Dupes the label, allocating the new name and value using the provided allocator.
    pub fn dupe(label: Label, gpa: std.mem.Allocator) error{OutOfMemory}!Label {
        const n = try gpa.dupe(u8, label.n);
        errdefer gpa.free(n);

        const v = try gpa.dupe(u8, label.v);
        errdefer gpa.free(v);

        return .{ .n = n, .v = v };
    }
};
