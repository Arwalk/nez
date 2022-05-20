const std = @import("std");
const debug = std.log.debug;

const iNesROMVersion = enum {
    V1,
    V2
};

const TvSystem = enum(u1) {
    NTSC = 0,
    PAL = 1
};

pub const INesROM = struct {

    const InvalidFileError = error {
        NotINes
    };

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
        ram_size: u8,
        program_rom: [16384 * 255]u8
    };

    const ChrData = struct {
        size: u8,
        rom: [8192 * 255]u8
    };

    pgr: PgrData,
    chr: ChrData,
    mapper: u8,
    flags: INesFlags,
    trainer: ?[512]u8,

    pub fn from_file_path(file_path: []const u8) !void {
        debug("from_file_path : opening file {s}", .{file_path});
        
        var file_handle = try std.fs.openFileAbsolute(file_path, .{.read = true, .write = false});
        defer file_handle.close();

        var buffer = [_]u8{0} ** (16 + 512 + (16384 * 255) + (8192 * 256) + 32);
        const size_read = file_handle.readAll(&buffer);

        try from_buffer(&buffer);
    }

    pub fn from_buffer(buffer: []const u8) !void {
        if(
            buffer[0] != 0x4E or
            buffer[1] != 0x45 or
            buffer[2] != 0x53 or
            buffer[3] != 0x1A) {
                return error.NotINes;
        }
        debug("file is INes!", .{});


    }

};