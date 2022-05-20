const ines = @import("ines");
const cpu = @import("cpu");
const std = @import("std");
const test_allocator = std.testing.allocator;
const expect = std.testing.expect;

const all_instrs = @embedFile("test_files/all_instrs.nes");

test "official only" {
    const official_only = @embedFile("test_files/official_only.nes");

    var rom = try ines.INesROM.from_buffer(official_only[0..official_only.len], test_allocator);
    defer rom.free(test_allocator);
    expect((rom.pgr_rom.len / 16384) == 16);
    expect(rom.chr_rom == null);
    expect(rom.flags.mirroring == .VERTICAL);
    expect(rom.flags.battery_backed_prg_ram == false);
    expect(rom.trainer == null);
    expect(rom.flags.hard_wired_four_screen_mode == false);
    expect(rom.mapper == 1);
    expect(rom.pgr_rom[0x10006 - 0x10] == 0x83);
}