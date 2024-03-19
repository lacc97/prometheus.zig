const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const test_step = b.step("test", "Run unit tests");

    const mod = b.addModule("prometheus.zig", .{
        .root_source_file = "src/prometheus.zig",
        .target = target,
        .optimize = optimize,
    });
    _ = mod; // autofix

    const mod_tests = b.addTest(.{
        .root_source_file = "src/prometheus.zig",
        .target = target,
        .optimize = optimize,
    });
    test_step.dependOn(&b.addRunArtifact(mod_tests).step);
}
