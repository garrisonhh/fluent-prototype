const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("fluent", "src/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.setOutputDir(".");
    exe.linkLibC();

    // linenoise (placeholder until static library linking gets fixed)
    exe.addCSourceFile("lib/linenoise/linenoise.c", &.{""});
    exe.addIncludePath("lib/linenoise");

    // packages
    exe.addPackagePath("kritzler", "lib/kritzler/kritzler.zig");
    exe.addPackagePath("util", "lib/util/util.zig");

    exe.install();
}
