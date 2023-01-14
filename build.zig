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

    // packages
    const Pkg = std.build.Pkg;
    const rel_fs = std.build.FileSource.relative;

    const linenoize = Pkg{
        .name = "linenoize",
        .source = rel_fs("lib/linenoize/linenoize.zig")
    };
    const kritzler = Pkg{
        .name = "kritzler",
        .source = rel_fs("lib/kritzler/kritzler.zig"),
    };
    const util = Pkg{
        .name = "util",
        .source = rel_fs("lib/util/util.zig"),
        .dependencies = &[_]Pkg{kritzler},
    };

    exe.addPackage(linenoize);
    exe.addPackage(kritzler);
    exe.addPackage(util);

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
