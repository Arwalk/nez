const std = @import("std");

const iNesROMVersion = enum {
    V1,
    V2
};

const TvSystem = enum(bool) {
    NTSC = 0,
    PAL = 1
};

const INesROMV1 = struct {

    const INesHeader = struct {

        const INesFlags = struct {
            // f6
            mirroring: bool,                // b0 horizontal (vertical arrangement) (CIRAM A10 = PPU A11) | vertical (horizontal arrangement) (CIRAM A10 = PPU A10)
            battery_backed_prg_ram: bool,   // b1 Cartridge contains battery-backed PRG RAM ($6000-7FFF) or other persistent memory
            is_trainer_present: bool,       // b2 512-byte trainer at $7000-$71FF (stored before PRG data)
            ignore_mirroring_control: bool, // b3 Ignore mirroring control or above mirroring bit; instead provide four-screen VRAM
            // f9
            tv_system: TvSystem,
            // f10
            has_bus_conflicts: bool,
        };

        const PgrData = struct {
            size: u8,
            ram_size: u8
        };

        pgr: PgrData,
        size_chr: u8,
        mapper: u8,
        flags: INesFlags,
    };

    pub fn from_file_path(file_path: []const u8) INesROMV1 {
        var file = std.os.open(file_path, 0, 0);

    }

};