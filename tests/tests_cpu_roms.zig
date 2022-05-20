const ines = @import("ines");
const proco = @import("cpu");
const std = @import("std");
const test_allocator = std.testing.allocator;
const expect = std.testing.expect;


test "official only" {
    const nes_test = @embedFile("test_files/nestest.nes");
    const results = @embedFile("test_files/nestest.log.txt");

    var rom = try ines.INesROM.from_buffer(nes_test[0..nes_test.len], test_allocator);
    defer rom.free(test_allocator);
    expect((rom.pgr_rom.len / 16384) == 1);
    expect(rom.chr_rom.?.len / 8192 == 1);
    expect(rom.flags.mirroring == .HORIZONTAL_OR_MAPPER);
    expect(rom.flags.battery_backed_prg_ram == false);
    expect(rom.trainer == null);
    expect(rom.flags.hard_wired_four_screen_mode == false);
    expect(rom.mapper == 0);

    var cpu = proco.NesCpu.init();
    cpu.load(&rom);
    expect(cpu.memory[0x8000] == 0x4C);
    expect(cpu.memory[0xBFFF] == 0xC5);
    expect(cpu.memory[0xC000] == 0x4C);
    expect(cpu.memory[0xFFFF] == 0xC5);
    cpu.pc = 0xC000;

}