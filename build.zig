const std = @import("std");
const builtin = @import("builtin");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    // check compiler version
    comptime {
        const desired = try std.SemanticVersion.parse("0.10.0");
        const version = builtin.zig_version;
        if (version.order(desired).compare(.neq)) {
            const msg = std.fmt.comptimePrint(
                "expected zig version {}, found {}",
                .{desired, version}
            );
            @compileError(msg);
        }
    }

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

    // tests
    const tests = b.addTest("src/tests.zig");
    tests.setTarget(target);
    tests.setBuildMode(mode);
    tests.setOutputDir(".");

    tests.addPackagePath("kritzler", "lib/kritzler/kritzler.zig");
    tests.addPackagePath("util", "lib/util/util.zig");

    const test_step = b.step("test", "run fluent tests");
    test_step.dependOn(&tests.step);
}
