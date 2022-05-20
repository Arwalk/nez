const ines = @import("ines");
const cpu = @import("cpu");
const std = @import("std");
const test_allocator = std.testing.allocator;

const all_instrs = @embedFile("test_files/all_instrs.nes");

test "official only" {
    const official_only = @embedFile("test_files/official_only.nes");

    var rom = try ines.INesROM.from_buffer(official_only[0..official_only.len], test_allocator);
    defer rom.free(test_allocator);
}