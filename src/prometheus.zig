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
            @atomicLoad(T, &c.value, .monotonic);
        }

        pub inline fn inc(c: *C, x: T) void {
            @atomicRmw(T, &c.value, .Add, x, .monotonic);
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
            @atomicLoad(T, &g.value, .monotonic);
        }

        pub inline fn set(g: *G, x: T) T {
            @atomicStore(T, &g.value, x, .monotonic);
        }

        pub inline fn inc(g: *G, x: T) void {
            @atomicRmw(T, &g.value, .Add, x, .monotonic);
        }

        pub inline fn dec(g: *G, x: T) void {
            @atomicRmw(T, &g.value, .Sub, x, .monotonic);
        }
    };
}

pub fn Family(comptime MetricFamily: type, comptime Dims: type) type {
    return struct {
        const Self = @This();

        reg: *Registry,
        ptr: *MetricFamily,

        pub const Metric = MetricFamily.Metric;
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

    metrics: std.StringHashMapUnmanaged(MetricType) = .{},
    metrics_data: struct {
        int_counter: std.StringArrayHashMapUnmanaged(*MetricFamily(.int_counter)) = .{},
        float_counter: std.StringArrayHashMapUnmanaged(*MetricFamily(.float_counter)) = .{},
        int_gauge: std.StringArrayHashMapUnmanaged(*MetricFamily(.int_gauge)) = .{},
        float_gauge: std.StringArrayHashMapUnmanaged(*MetricFamily(.float_gauge)) = .{},
    } = .{},

    pub fn init(gpa: std.mem.Allocator) Registry {
        return .{ .gpa = gpa };
    }
    pub fn deinit(reg: *Registry) void {
        const gpa = reg.gpa;

        inline for (@typeInfo(MetricType).Enum.fields) |f| {
            const data = &@field(reg.metrics_data, f.name);

            var it = data.iterator();
            while (it.next()) |entry| {
                entry.value_ptr.*.deinit(gpa);
                gpa.destroy(entry.value_ptr.*);
            }

            data.deinit(gpa);
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
    ) error{OutOfMemory}!Family(MetricFamily(opts.metric_type), opts.Dimensions) {
        reg.mutex.lock();
        defer reg.mutex.unlock();

        const gpa = reg.gpa;

        const metric_type_key_ptr, const metric_type_ptr, const data_index, const data_key_ptr, const data_ptr = blk_ptrs: {
            // Lookup the family name in the appropriate maps.

            const metrics_gop = try reg.metrics.getOrPut(gpa, name);
            errdefer if (!metrics_gop.found_existing) reg.metrics.removeByPtr(metrics_gop.key_ptr);

            const data = &@field(reg.metrics_data, @tagName(opts.metric_type));
            const data_gop = try data.getOrPut(gpa, name);
            errdefer if (!data_gop.found_existing) data.removeByPtr(data_gop.key_ptr);

            // Either we find a match in both hashmaps or in none.
            assert(@intFromBool(metrics_gop.found_existing) ^ @intFromBool(data_gop.found_existing) == 0);

            if (metrics_gop.found_existing) return .{ .reg = reg, .ptr = data_gop.value_ptr.* };
            break :blk_ptrs .{ metrics_gop.key_ptr, metrics_gop.value_ptr, data_gop.index, data_gop.key_ptr, data_gop.value_ptr };
        };
        // The family is new and needs to be initialised.
        errdefer {
            @field(reg.metrics_data, @tagName(opts.metric_type)).swapRemoveAt(data_index);
            reg.metrics.removeByPtr(metric_type_key_ptr);
        }

        const label_names: []const []const u8 = comptime labelNames(opts.Dimensions);

        const name_interned = try reg.strings.intern(gpa, name);
        metric_type_key_ptr.* = name_interned;
        data_key_ptr.* = name_interned;

        metric_type_ptr.* = opts.metric_type;

        data_ptr.* = try gpa.create(MetricFamily(opts.metric_type));
        errdefer gpa.destroy(data_ptr.*);

        data_ptr.*.* = .{
            .name = name_interned,
            .help = try reg.strings.intern(gpa, help),
            .labels = label_names,
        };

        return .{ .reg = reg, .ptr = data_ptr.* };
    }

    fn addMetric(
        reg: *Registry,
        comptime metric_type: MetricType,
        mf: *MetricFamily(metric_type),
        m: *MetricFamily(metric_type).Metric,
        comptime Dimensions: type,
        dims: Dimensions,
    ) error{ FoundExisting, IncompatibleDimensions, OutOfMemory }!void {
        reg.mutex.lock();
        defer reg.mutex.unlock();

        const gpa = reg.gpa;

        const label_names = comptime labelNames(Dimensions);
        {
            if (label_names.len != mf.labels.len) return error.IncompatibleDimensions;
            for (label_names, mf.labels) |name_new, name| if (!std.mem.eql(u8, name_new, name)) return error.IncompatibleDimensions;
        }

        const labels = try gpa.alloc([]const u8, label_names.len);
        errdefer gpa.free(labels);

        inline for (labels, @typeInfo(Dimensions).Struct.fields) |*l, f| {
            l.* = try formatLabel(&reg.strings, f.type, @field(dims, f.name));
        }

        const gop = try mf.data.getOrPut(gpa, labels);
        if (gop.found_existing) return error.FoundExisting;
        gop.value_ptr.* = m;
    }

    fn MetricFamily(comptime metric_type: MetricType) type {
        return struct {
            const Self = @This();

            name: []const u8,
            help: []const u8,
            labels: []const []const u8,
            data: std.ArrayHashMapUnmanaged([]const []const u8, *Metric, LabelsHashMapContext, true) = .{},

            pub const Metric = metric_type.Type();

            pub fn deinit(self: *Self, gpa: std.mem.Allocator) void {
                for (self.data.keys()) |k| gpa.free(k);
                self.data.deinit(gpa);
            }
        };
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

    fn formatLabel(strings: *StringIntern, comptime Field: type, f: Field) error{OutOfMemory}![]const u8 {
        if (@typeInfo(Field) == .Optional) {
            if (f) |f_child| return formatLabel(strings, @typeInfo(Field).Optional.child, f_child);
            return "";
        }

        const FormattingType = LabelFormattingType(Field);

        const f_format: FormattingType = f;

        return switch (FormattingType) {
            u64, i64, f64 => blk: {
                var buf: [64]u8 = undefined;
                break :blk try strings.intern(std.fmt.bufPrint(&buf, "{}", .{f_format}) catch unreachable);
            },
            []const u8 => try strings.intern(f_format),
            Field => blk: {
                assert(@typeInfo(Field) == .Enum);
                break :blk @tagName(f_format);
            },
            else => unreachable,
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
