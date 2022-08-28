const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("lisp", "src/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.setOutputDir(".");
    exe.linkLibC();

    // linenoise (placeholder until static library linking gets fixed)
    exe.addCSourceFile("lib/linenoise/linenoise.c", &.{""});
    exe.addIncludeDir("lib/linenoise");

    // kritzler
    exe.addPackagePath("kritzler", "lib/kritzler/kritzler.zig");

    exe.install();
}
