const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const prometheus_dep = b.dependency("prometheus.zig", .{ .target = target, .optimize = optimize });

    const exe = b.addExecutable(.{
        .name = "example",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addImport("prometheus", prometheus_dep.module("prometheus.zig"));
    b.installArtifact(exe);

    const exe_run = b.addRunArtifact(exe);
    exe_run.has_side_effects = true;
    if (b.args) |args| exe_run.addArgs(args);
    b.step("run", "Run this example").dependOn(&exe_run.step);
}
