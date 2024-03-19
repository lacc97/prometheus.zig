const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const test_step = b.step("test", "Run unit tests");

    const mod = b.addModule("prometheus.zig", .{
        .root_source_file = .{ .path = "src/prometheus.zig" },
        .target = target,
        .optimize = optimize,
    });
    _ = mod; // autofix

    const mod_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/prometheus.zig" },
        .target = target,
        .optimize = optimize,
    });
    test_step.dependOn(&b.addRunArtifact(mod_tests).step);

    const prometheus_cmd = addPrometheusControlCommand(b);
    if (b.args) |args| prometheus_cmd.addArgs(args);
    b.step("prometheus", "Control test prometheus instance").dependOn(&prometheus_cmd.step);
}

fn addPrometheusControlCommand(b: *std.Build) *std.Build.Step.Run {
    const cmd = b.addSystemCommand(&.{"/bin/bash"});
    cmd.addFileArg(.{ .path = "test/prometheus/control.sh" });
    cmd.has_side_effects = true;
    return cmd;
}
