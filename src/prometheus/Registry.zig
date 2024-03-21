const std = @import("std");
const assert = std.debug.assert;

const Registry = @This();

mutex: std.Thread.Mutex = .{},

gpa: std.mem.Allocator,

metrics: std.StringArrayHashMapUnmanaged(*Family) = .{},

const MetricType = @import("metrics.zig").MetricType;
const IntCounter = @import("metrics.zig").IntCounter;
const Counter = @import("metrics.zig").Counter;
const IntGauge = @import("metrics.zig").IntGauge;
const Gauge = @import("metrics.zig").Gauge;

pub fn init(gpa: std.mem.Allocator) Registry {
    return .{ .gpa = gpa };
}
pub fn deinit(reg: *Registry) void {
    const gpa = reg.gpa;

    for (reg.metrics.values()) |v| {
        v.deinit(gpa);
        gpa.destroy(v);
    }

    reg.metrics.deinit(gpa);
}

pub const FamilyOpts = struct {
    name: []const u8,
    help: []const u8 = &.{},
    label_names: []const []const u8 = &.{},
    type: MetricType,
};

pub fn family(reg: *Registry, opts: FamilyOpts) !FamilyRegistry {
    _ = reg; // autofix
    _ = opts; // autofix

}

pub const FamilyRegistry = struct {};

const Family = struct {
    name: []const u8,
    help: []const u8,
    label_names: []const []const u8,
    type: MetricType,

    data: std.ArrayHashMapUnmanaged([]const []const u8, MetricPtr, HashMapContext, true) = .{},

    fn init(
        gpa: std.mem.Allocator,
        name: []const u8,
        help: []const u8,
        label_names: []const []const u8,
    ) error{OutOfMemory}!Family {
        const name_alloced = try gpa.dupe(name);
        errdefer gpa.free(name_alloced);

        const help_alloced = try gpa.dupe(help);
        errdefer gpa.free(help_alloced);

        var label_names_alloced = try std.ArrayListUnmanaged([]const u8).initCapacity(gpa, label_names.len);
        errdefer {
            for (label_names_alloced.items) |l| gpa.free(l);
            label_names_alloced.deinit(gpa);
        }
        for (label_names) |l| label_names_alloced.appendAssumeCapacity(try gpa.dupe(l));

        std.sort.pdq([][]const u8, label_names_alloced.items, {}, stringOrder);

        return .{
            .name = name_alloced,
            .help = help_alloced,
            .label_names = label_names_alloced.toOwnedSlice(gpa) catch unreachable,
        };
    }

    fn deinit(fam: Family, gpa: std.mem.Allocator) void {
        for (fam.data.keys()) |k| {
            for (k) |l| gpa.free(l);
            gpa.free(k);
        }
        for (fam.label_names) |l| gpa.free(l);
        gpa.free(fam.label_names);
        gpa.free(fam.help);
        gpa.free(fam.name);
    }

    const MetricPtr = union(MetricType) {
        int_counter: *IntCounter,
        float_counter: *Counter,
        int_gauge: *IntGauge,
        float_gauge: *Gauge,
    };

    fn addWithLabelStrings(
        fam: *Family,
        gpa: std.mem.Allocator,
        ptr: MetricPtr,
        labels: []const struct { []const u8, []const u8 },
    ) error{ LabelShapeMismatch, LabelNameMismatch, DuplicateLabel, TypeMismatch, OutOfMemory }!void {
        if (std.meta.activeTag(ptr) != fam.type) return error.TypeMismatch;

        const labels_alloced = try dupeLabelValues(fam.label_names, labels);
        errdefer {
            for (labels_alloced) |l| gpa.free(l);
            gpa.free(labels_alloced);
        }

        const gop = try fam.data.getOrPut(gpa, labels_alloced);
        errdefer fam.data.swapRemoveAt(gop.index);

        gop.value_ptr.* = ptr;
    }

    fn addWithLabelStruct(
        fam: *Family,
        gpa: std.mem.Allocator,
        ptr: MetricPtr,
        labels: []const struct { []const u8, []const u8 },
    ) error{ LabelShapeMismatch, LabelNameMismatch, TypeMismatch, OutOfMemory }!void {
        _ = gpa; // autofix
        _ = labels; // autofix
        if (std.meta.activeTag(ptr) != fam.type) return error.TypeMismatch;
    }

    /// The label names must be sorted. The labels do not need to be sorted.
    fn dupeLabelValues(
        gpa: std.mem.Allocator,
        label_names: []const []const u8,
        labels: []const struct { []const u8, []const u8 },
    ) error{ LabelShapeMismatch, LabelNameMismatch, DuplicateLabel, OutOfMemory }![]const []const u8 {
        if (labels.len > label_names.len) return error.LabelShapeMismatch;

        // We want to check for duplicate labels without unnecessary allocations.
        const labels_alloced = try gpa.alloc([]const u8, label_names.len);
        errdefer gpa.free(labels_alloced);
        @memset(labels_alloced, &.{});

        // If we find a label index but the corresponding entry is already filled, we have a duplicate label name.
        for (labels) |lab| {
            const index = labelIndex(label_names, lab[0]) orelse return error.LabelNameMismatch;
            const lab_alloced = &labels_alloced[index];
            if (lab_alloced.ptr == label_names[index].ptr or lab_alloced.len == label_names[index].len) return error.DuplicateLabel;
            lab_alloced.* = label_names[index];
        }

        // Prepare the values for duping.
        @memset(labels_alloced, &.{});
        for (labels) |lab| labels_alloced[labelIndex(label_names, lab[0]) orelse unreachable] = lab[1];

        // Now we can actually dupe the label values.
        var i: usize = 0;
        errdefer for (0..i) |j| gpa.free(labels_alloced[j]);
        while (i < labels_alloced.len) : (i += 1) labels_alloced[i] = try gpa.dupe(u8, labels_alloced[i]);

        return labels_alloced;
    }

    test dupeLabelValues {
        const testing = std.testing;

        const expectedEqual = struct {
            fn expectedEqual(expected: []const []const u8, actual: []const []const u8) !void {
                try testing.expectEqual(expected.len, actual.len);
                for (expected, actual) |e, a| try testing.expectEqualSlices(u8, e, a);
            }
        }.expectedEqual;

        var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena_state.deinit();

        const arena = arena_state.allocator();

        {
            const label_names = [_][]const u8{ "axe", "baz", "zzz" };

            try expectedEqual(&.{ "", "", "" }, try dupeLabelValues(arena, &label_names, &.{}));
            try expectedEqual(&.{ "", "0", "2" }, try dupeLabelValues(arena, &label_names, &.{ .{ "baz", "0" }, .{ "zzz", "2" } }));
            try expectedEqual(&.{ "1", "0", "2" }, try dupeLabelValues(arena, &label_names, &.{ .{ "baz", "0" }, .{ "axe", "1" }, .{ "zzz", "2" } }));

            try testing.expectError(
                error.LabelShapeMismatch,
                dupeLabelValues(arena, &label_names, &.{ .{ "baz", "0" }, .{ "axe", "1" }, .{ "zzz", "2" }, .{ "foo", "3" } }),
            );
            try testing.expectError(
                error.LabelNameMismatch,
                dupeLabelValues(arena, &label_names, &.{ .{ "baz", "0" }, .{ "bar", "1" }, .{ "zzz", "2" } }),
            );
            try testing.expectError(
                error.DuplicateLabel,
                dupeLabelValues(arena, &label_names, &.{ .{ "baz", "0" }, .{ "axe", "1" }, .{ "baz", "2" } }),
            );
        }
    }

    /// The label names must be sorted.
    fn labelIndex(label_names: []const []const u8, label: []const u8) ?usize {
        return std.sort.binarySearch([]const u8, label, label_names, {}, stringOrder);
    }

    const HashMapContext = struct {
        pub fn hash(_: HashMapContext, k: []const []const u8) u32 {
            var h = std.hash.Wyhash.init(0);
            h.update(std.mem.asBytes(&k.len));
            for (k) |s| {
                h.update(std.mem.asBytes(&s.len));
                h.update(s);
            }
            return @truncate(h.final());
        }

        pub fn eql(_: HashMapContext, k1: []const []const u8, k2: []const []const u8, _: usize) bool {
            if (k1.len != k2.len) return false;
            for (k1, k2) |s1, s2| if (!std.mem.eql(u8, s1, s2)) return false;
            return true;
        }
    };

    fn stringOrder(_: void, a: []const u8, b: []const u8) std.math.Order {
        return std.mem.order(u8, a, b);
    }

    fn stringLessThan(_: void, a: []const u8, b: []const u8) bool {
        return stringOrder({}, a, b) == .lt;
    }
};

pub fn write(reg: *Registry, w: anytype) !void {
    for (reg.metrics.values()) |fam| {
        if (fam.help.len > 0) try w.print("# HELP {s} {s}\n", .{ fam.name, fam.help });
        try w.print("# TYPE {s} {s}\n", .{ fam.name, switch (std.meta.activeTag(fam.data)) {
            .int_counter, .float_counter => "counter",
            .int_gauge, .float_gauge => "gauge",
        } });
        switch (fam.data) {
            inline else => |data| {
                for (data.keys(), data.values()) |labels, metric| {
                    std.debug.assert(labels.len == fam.labels.len);

                    try w.writeAll(fam.name);
                    if (labels.len > 0) {
                        try w.writeByte('{');
                        for (fam.labels, labels) |name, value| {
                            if (value.len > 0) try w.print("{s}=\"{s}\",", .{ name, value });
                        }
                        try w.writeByte('}');
                    }
                    try w.print(" {}\n", .{metric.get()});
                }
            },
        }
        try w.writeAll("\n");
    }
}

pub fn register(
    reg: *Registry,
    metric: anytype,
    opts: struct { name: []const u8, help: []const u8, labels: []const []const u8 },
) error{ MismatchedType, MismatchedHelp, MismatchedDimensions, OutOfMemory }!void {
    const Metric, const metric_type = blk_metric: {
        const M0 = @TypeOf(metric);
        break :blk_metric switch (@typeInfo(M0)) {
            .Pointer => |info| .{ info.child, MetricType.value(info.child) },
            else => @compileError("metric must be a pointer to a metric type (got " ++ @typeName(M0) ++ " instead)"),
        };
    };

    reg.mutex.lock();
    defer reg.mutex.unlock();

    const gpa = reg.gpa;

    _ = gpa; // autofix
    _ = metric_type; // autofix

    _ = Metric; // autofix
    _ = opts; // autofix
}

fn labelNames(comptime Dimensions: type) []const []const u8 {
    const struct_info = switch (@typeInfo(Dimensions)) {
        .Struct => |info| blk_info: {
            if (info.is_tuple) @compileError("Dimensions type (`" ++ @typeName(Dimensions) ++ "') must not be a tuple");
            break :blk_info info;
        },
        else => @compileError("Dimensions type (`" ++ @typeName(Dimensions) ++ "') must be a struct"),
    };

    // TODO: check that the label names conform to Prometheus naming spec

    var labels: [struct_info.fields.len][]const u8 = undefined;
    for (&labels, struct_info.fields) |*l, f| {
        comptime _ = LabelFormattingType(f.type); // type check
        l.* = f.name;
    }
    return &labels;
}

const LabelDesc = struct {
    name: []const u8,
};

fn verifyLabels(comptime T: type) []LabelDesc {
    _ = T; // autofix

}

fn verifySingleLabel(comptime name: []const u8, comptime T: type) LabelDesc {
    _ = name; // autofix
    _ = T; // autofix
    const err = struct {
        fn err(comptime format: []const u8, args: anytype) []const u8 {
            _ = format; // autofix
            _ = args; // autofix
            return std.fmt.comptimePrint("");
        }
    }.err;
    _ = err; // autofix
}

/// Returns the type that will be used for formatting after coercion.
fn LabelFormattingType(comptime Field: type) type {
    const typeError = struct {
        fn typeError(comptime format: []const u8, args: anytype) []const u8 {
            return std.fmt.comptimePrint("unsupported type for label formatting `{s}': " ++ format, .{@typeName(Field)} ++ args);
        }
    }.typeError;

    return switch (@typeInfo(Field)) {
        .Optional => |info| ?LabelFormattingType(info.child),
        .Int => |info| blk: {
            if (info.bits > 64) @compileError(typeError("only up to 64 bits for now sorry (TODO: figure out static buffer size for larger ints)", .{}));
            break :blk switch (info.signedness) {
                .unsigned => u64,
                .signed => i64,
            };
        },
        .Float => |info| blk: {
            if (info.bits > 64) @compileError(typeError("only up to 64 bits for now sorry (TODO: figure out static buffer size for larger floats)", .{}));
            break :blk f64;
        },
        .Pointer => |info| blk: {
            if (info.size != .Slice or info.child != u8) @compileError(typeError("only u8 strings allowed"));
            break :blk []const u8;
        },
        .Enum => |info| blk: {
            if (!info.is_exhaustive) @compileError(typeError("non-exhaustive enums not allowed for now sorry (TODO: ?)", .{}));
            break :blk Field;
        },
        else => @compileError(typeError("not supported", .{})),
    };
}

test {
    _ = Family;
}
