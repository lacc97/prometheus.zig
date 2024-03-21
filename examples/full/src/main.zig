const std = @import("std");
const assert = std.debug.assert;

const prometheus = @import("prometheus");

var my_int_counter = prometheus.Counter(.int){};

pub fn main() !void {
    var gpa_state: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer _ = gpa_state.deinit();
    const gpa = gpa_state.allocator();

    var registry = prometheus.Registry.init(gpa);
    defer registry.deinit();

    const family = try registry.family("observed_packets", "Number of packets", .{
        .metric_type = .int_counter,
        .Dimensions = struct { protocol: enum { tcp, udp }, direction: enum { rx, tx } },
    });

    var tcp_rx_counter: prometheus.Counter(.int) = .{};
    try family.add(&tcp_rx_counter, .{ .protocol = .tcp, .direction = .rx });

    var tcp_tx_counter: prometheus.Counter(.int) = .{};
    try family.add(&tcp_tx_counter, .{ .protocol = .tcp, .direction = .tx });

    const family_2 = try registry.family("my_float", "Floaty number of float thingies", .{
        .metric_type = .float_gauge,
        .Dimensions = struct { opt_label: ?enum { a, b } = null, name: []const u8, id: u16 },
    });

    const floaties = try gpa.alloc(prometheus.Gauge(.float), 16);
    defer gpa.free(floaties);
    for (floaties, 0..) |*f, i| {
        f.* = .{};
        try family_2.add(f, .{ .name = "hello", .id = @intCast(i) });
    }

    var buffered = std.io.bufferedWriter(std.io.getStdOut().writer());
    try registry.write(buffered.writer());
    try buffered.flush();
}
