const std = @import("std");
const assert = std.debug.assert;

pub const MetricType = enum {
    int_counter,
    float_counter,
    int_gauge,
    float_gauge,

    pub fn Type(comptime metric_type: MetricType) type {
        return switch (metric_type) {
            .int_counter => Counter(.int),
            .float_counter => Counter(.float),
            .int_gauge => Gauge(.int),
            .float_gauge => Gauge(.float),
        };
    }
};

pub fn Counter(comptime itype: enum { int, float }) type {
    const T = switch (itype) {
        .int => i64,
        .float => f64,
    };

    return struct {
        const C = @This();

        value: T = 0,

        pub const metric_type: MetricType = switch (itype) {
            .int => .int_counter,
            .float => .float_counter,
        };

        pub inline fn get(c: *const C) T {
            return @atomicLoad(T, &c.value, .monotonic);
        }

        pub inline fn inc(c: *C, x: T) void {
            _ = @atomicRmw(T, &c.value, .Add, x, .monotonic);
        }
    };
}

pub fn Gauge(comptime itype: enum { int, float }) type {
    const T = switch (itype) {
        .int => i64,
        .float => f64,
    };

    return struct {
        const G = @This();

        value: T = 0,

        pub const metric_type: MetricType = switch (itype) {
            .int => .int_gauge,
            .float => .float_gauge,
        };

        pub inline fn get(g: *const G) T {
            return @atomicLoad(T, &g.value, .monotonic);
        }

        pub inline fn set(g: *G, x: T) T {
            @atomicStore(T, &g.value, x, .monotonic);
        }

        pub inline fn inc(g: *G, x: T) void {
            _ = @atomicRmw(T, &g.value, .Add, x, .monotonic);
        }

        pub inline fn dec(g: *G, x: T) void {
            _ = @atomicRmw(T, &g.value, .Sub, x, .monotonic);
        }
    };
}

pub fn Family(comptime metric_type: MetricType, comptime Dims: type) type {
    return struct {
        const Self = @This();

        reg: *Registry,
        ptr: *Registry.MetricFamily,

        pub const Metric = metric_type.Type();
        pub const Dimensions = Dims;

        pub fn add(fam: Self, m: *Metric, dimensions: Dimensions) !void {
            return try fam.reg.addMetric(Metric.metric_type, fam.ptr, m, Dimensions, dimensions);
        }
    };
}

pub const Registry = struct {
    mutex: std.Thread.Mutex = .{},

    gpa: std.mem.Allocator,

    strings: StringIntern = .{},

    metrics: std.StringArrayHashMapUnmanaged(*MetricFamily) = .{},

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
        reg.strings.deinit(gpa);
    }

    pub const FamilyOpts = struct {
        metric_type: MetricType,
        Dimensions: type,
    };

    pub fn family(
        reg: *Registry,
        name: []const u8,
        help: []const u8,
        comptime opts: FamilyOpts,
    ) error{ MismatchedType, OutOfMemory }!Family(opts.metric_type, opts.Dimensions) {
        reg.mutex.lock();
        defer reg.mutex.unlock();

        const gpa = reg.gpa;

        const index, const key_ptr, const value_ptr = blk_ptrs: {
            const gop = try reg.metrics.getOrPut(gpa, name);
            errdefer if (!gop.found_existing) reg.metrics.swapRemoveAt(gop.index);

            if (gop.found_existing) return .{ .reg = reg, .ptr = gop.value_ptr.* };
            break :blk_ptrs .{ gop.index, gop.key_ptr, gop.value_ptr };
        };
        errdefer reg.metrics.swapRemoveAt(index);

        const label_names: []const []const u8 = comptime labelNames(opts.Dimensions);

        const name_interned = try reg.strings.intern(gpa, name);
        key_ptr.* = name_interned;
        value_ptr.* = try gpa.create(MetricFamily);
        errdefer gpa.destroy(value_ptr.*);

        value_ptr.*.* = .{
            .name = name_interned,
            .help = try reg.strings.intern(gpa, help),
            .labels = label_names,
            .data = @unionInit(std.meta.FieldType(MetricFamily, .data), @tagName(opts.metric_type), .{}),
        };

        return .{ .reg = reg, .ptr = value_ptr.* };
    }

    fn addMetric(
        reg: *Registry,
        comptime metric_type: MetricType,
        metric_family: *MetricFamily,
        metric: *metric_type.Type(),
        comptime Dimensions: type,
        dims: Dimensions,
    ) error{ FoundExisting, IncompatibleDimensions, OutOfMemory }!void {
        reg.mutex.lock();
        defer reg.mutex.unlock();

        const gpa = reg.gpa;

        const label_names = comptime labelNames(Dimensions);
        {
            if (label_names.len != metric_family.labels.len) return error.IncompatibleDimensions;
            for (label_names, metric_family.labels) |name_new, name| if (!std.mem.eql(u8, name_new, name)) return error.IncompatibleDimensions;
        }

        const labels = try gpa.alloc([]const u8, label_names.len);
        errdefer gpa.free(labels);

        // It's fine to format the labels here even before checking the map. If the labels are duplicate,
        // they were already interned. Otherwise, we were going to intern them anyway.
        inline for (labels, @typeInfo(Dimensions).Struct.fields) |*l, f| {
            l.* = try formatLabel(gpa, &reg.strings, f.type, @field(dims, f.name));
        }

        const gop = try @field(metric_family.data, @tagName(metric_type)).getOrPut(gpa, labels);
        if (gop.found_existing) return error.FoundExisting;
        gop.value_ptr.* = metric;
    }

    const MetricFamily = struct {
        name: []const u8,
        help: []const u8,
        labels: []const []const u8,
        data: union(MetricType) {
            int_counter: std.ArrayHashMapUnmanaged([]const []const u8, *MetricType.int_counter.Type(), LabelsHashMapContext, true),
            float_counter: std.ArrayHashMapUnmanaged([]const []const u8, *MetricType.float_counter.Type(), LabelsHashMapContext, true),
            int_gauge: std.ArrayHashMapUnmanaged([]const []const u8, *MetricType.int_gauge.Type(), LabelsHashMapContext, true),
            float_gauge: std.ArrayHashMapUnmanaged([]const []const u8, *MetricType.float_gauge.Type(), LabelsHashMapContext, true),
        },

        pub fn deinit(self: *MetricFamily, gpa: std.mem.Allocator) void {
            switch (self.data) {
                inline else => |*data| {
                    for (data.keys()) |k| gpa.free(k);
                    data.deinit(gpa);
                },
            }
        }
    };

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

    fn formatLabel(gpa: std.mem.Allocator, strings: *StringIntern, comptime Field: type, f: Field) error{OutOfMemory}![]const u8 {
        if (@typeInfo(Field) == .Optional) {
            if (f) |f_child| return formatLabel(gpa, strings, @typeInfo(Field).Optional.child, f_child);
            return "";
        }

        const FormattingType = LabelFormattingType(Field);

        const f_format: FormattingType = f;

        return switch (FormattingType) {
            u64, i64, f64 => blk: {
                var buf: [64]u8 = undefined;
                break :blk try strings.intern(gpa, std.fmt.bufPrint(&buf, "{}", .{f_format}) catch unreachable);
            },
            []const u8 => try strings.intern(gpa, f_format),
            else => if (FormattingType == Field) blk: {
                assert(@typeInfo(Field) == .Enum);
                break :blk @tagName(f_format);
            } else unreachable,
        };
    }
};

const StringIntern = struct {
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
};

const LabelsHashMapContext = struct {
    pub fn hash(_: LabelsHashMapContext, k: []const []const u8) u32 {
        var h = std.hash.Wyhash.init(0);
        h.update(std.mem.asBytes(&k.len));
        for (k) |s| {
            h.update(std.mem.asBytes(&s.len));
            h.update(s);
        }
        return @truncate(h.final());
    }

    pub fn eql(_: LabelsHashMapContext, k1: []const []const u8, k2: []const []const u8, _: usize) bool {
        if (k1.len != k2.len) return false;
        for (k1, k2) |s1, s2| if (!std.mem.eql(u8, s1, s2)) return false;
        return true;
    }
};
