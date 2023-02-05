const std = @import("std");
const builtin = @import("builtin");

fn addPackages(step: *std.build.LibExeObjStep) void {
    const Pkg = std.build.Pkg;
    const rel_fs = std.build.FileSource.relative;

    const linenoize = Pkg{
        .name = "linenoize",
        .source = rel_fs("lib/linenoize/linenoize.zig"),
    };
    const kritzler = Pkg{
        .name = "kritzler",
        .source = rel_fs("lib/kritzler/kritzler.zig"),
    };
    const common = Pkg{
        .name = "common",
        .source = rel_fs("lib/common/common.zig"),
        .dependencies = &[_]Pkg{kritzler},
    };

    step.addPackage(linenoize);
    step.addPackage(kritzler);
    step.addPackage(common);
}

pub fn build(b: *std.build.Builder) void {
    // check compiler version
    comptime {
        const desired = try std.SemanticVersion.parse("0.10.1");
        const version = builtin.zig_version;
        if (version.order(desired).compare(.neq)) {
            const msg = std.fmt.comptimePrint(
                "expected zig version {}, found {}",
                .{ desired, version },
            );
            @compileError(msg);
        }
    }

    const target = b.standardTargetOptions(.{});
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("fluent", "src/main.zig");
    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.setOutputDir(".");
    addPackages(exe);

    exe.install();

    // tests
    const tests = b.addTest("src/main.zig");
    tests.setTarget(target);
    tests.setBuildMode(mode);
    addPackages(tests);

    const test_step = b.step("test", "run fluent tests");
    test_step.dependOn(&tests.step);

    // docs
    const docs = b.addTest("src/main.zig");
    docs.setTarget(target);
    docs.setBuildMode(mode);
    addPackages(docs);
    docs.emit_docs = .emit;

    const docs_step = b.step("docs", "generate compiler documentation");
    docs_step.dependOn(&docs.step);
}
