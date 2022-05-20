const std = @import("std");

const PackageDef = struct {
    name : [] const u8,
    path : [] const u8,

    pub fn get_name(self: *const PackageDef) []const u8 {
        return self.name;
    }

    pub fn get_path(self: *const PackageDef) []const u8 {
        return self.path;
    }
};

pub fn build(b: *std.build.Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();
    const exe = b.addExecutable("nez", "src/main.zig");

    const packages = [_]PackageDef {
        PackageDef{
            .name = "cpu",
            .path = "src/lib/cpu.zig"
        },
        PackageDef {
            .name = "ines",
            .path = "src/lib/ines.zig"
        }
    };

    for (packages) |package| {
        exe.addPackage(.{
            .name = package.get_name(),
            .path = package.get_path()
        });
    }


    exe.setTarget(target);
    exe.setBuildMode(mode);
    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const tst = b.step("test", "test all");

    const tests = [_]*std.build.LibExeObjStep {
        b.addTest("tests/tests_cpu.zig"),
        b.addTest("tests/tests_cpu_roms.zig")
    };

    for (tests) |test_def| {
        for (packages) |package| {
            test_def.addPackage(.{
                .name = package.get_name(),
                .path = package.get_path()
            });
        }
        tst.dependOn(&test_def.step);
    }


    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
