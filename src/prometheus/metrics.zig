const std = @import("std");

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

    pub fn value(comptime T: type) MetricType {
        return switch (T) {
            IntCounter => .int_counter,
            Counter => .float_counter,
            IntGauge => .int_counter,
            Gauge => .float_counter,
            else => @compileError(@typeName(T) ++ " is not a metric type"),
        };
    }
};

pub const IntCounter = CounterImpl(i64);
pub const Counter = CounterImpl(f64);

pub const IntGauge = GaugeImpl(i64);
pub const Gauge = GaugeImpl(f64);

fn CounterImpl(comptime T: type) type {
    return struct {
        const C = @This();

        value: T = 0,

        pub inline fn get(c: *const C) T {
            return @atomicLoad(T, &c.value, .monotonic);
        }

        pub inline fn inc(c: *C, x: T) void {
            _ = @atomicRmw(T, &c.value, .Add, x, .monotonic);
        }
    };
}

fn GaugeImpl(comptime T: type) type {
    return struct {
        const G = @This();

        value: T = 0,

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
