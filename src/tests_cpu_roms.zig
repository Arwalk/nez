const ines = @import("ines.zig");
const cpu = @import("cpu.zig");

const all_instrs = @embedFile("test_files/all_instrs.nes");

test "official only" {
    const official_only = @embedFile("test_files/official_only.nes");

    

}