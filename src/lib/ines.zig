const std = @import("std");
const debug = std.log.debug;

const iNesROMVersion = enum {
    V1,
    V2
};

const MirroringType = enum(u1) {
    HORIZONTAL_OR_MAPPER= 0,
    VERTICAL = 1
};

pub const INesROM = struct {

    const InvalidFileError = error {
        NotINes,
        NotManaged, // for V2 ines format, for now
    };

    const INesFlags = struct {
        // f6
        mirroring: MirroringType,               // b0 horizontal (vertical arrangement) (CIRAM A10 = PPU A11) | vertical (horizontal arrangement) (CIRAM A10 = PPU A10)
        battery_backed_prg_ram: bool,           // b1 Cartridge contains battery-backed PRG RAM ($6000-7FFF) or other persistent memory
        is_trainer_present: bool,               // b2 512-byte trainer at $7000-$71FF (stored before PRG data)
        hard_wired_four_screen_mode: bool,      // b3 Ignore mirroring control or above mirroring bit; instead provide four-screen VRAM
    };

    const prgrdata_multiplier = 16384;
    const chrdata_multiplier = 8192;

    pgr_rom: []u8,
    chr_rom: ?[]u8,
    mapper: u8,
    flags: INesFlags,
    trainer: ?[]u8,

    pub fn from_file_path(file_path: []const u8, allocator: *std.mem.Allocator) !INesROM {
        debug("from_file_path : opening file {s}", .{file_path});
        
        var file_handle = try std.fs.openFileAbsolute(file_path, .{.read = true, .write = false});
        defer file_handle.close();

        var buffer = try file_handle.readToEndAlloc(allocator);
        defer allocator.free(buffer);

        return try from_buffer(&buffer, allocator);
    }

    pub fn from_buffer(buffer: []const u8, allocator: *std.mem.Allocator) !INesROM {
        if(
            buffer[0] != 0x4E or
            buffer[1] != 0x45 or
            buffer[2] != 0x53 or
            buffer[3] != 0x1A) {
                return error.NotINes;
        }
        debug("file is INes!", .{});


        // parsing header
        const header = buffer[0..16];
        if((header[7]&0x0C)==0x08)
        {
            return InvalidFileError.NotManaged;
        }

        const pgr_size : usize = header[4] + @intCast(u12, header[9] & 0xF);
        const chr_size : usize = header[5] + @intCast(u12, header[9] >> 4);
        debug("pgr_size = {}", .{pgr_size});
        debug("chr_size = {}", .{chr_size});

        const flags : INesFlags = .{
            .mirroring = @intToEnum(MirroringType, @intCast(u1, header[6] & 1)),
            .battery_backed_prg_ram = ((header[6] & (1 << 1)) != 0),
            .is_trainer_present = ((header[6] & (1 << 2)) != 0),
            .hard_wired_four_screen_mode = ((header[6] & (1 << 3)) != 0),
        };

        const mapper: u8 = ((header[6] & 0xF0) >> 4) + (header[7] & 0xF0);

        const pgr_rom = try allocator.alloc(u8, pgr_size * prgrdata_multiplier);
        errdefer {
            allocator.free(pgr_rom);
        }


        var chr_rom : ?[]u8 = undefined;
        if(chr_size != 0)
        {
            chr_rom = try allocator.alloc(u8, chr_size * chrdata_multiplier);
            errdefer {
                allocator.free(chr_rom);
            }
        }
        else
        {
            chr_rom = null;
        }
        
        var trainer : ?[]u8 = undefined;
        if(flags.is_trainer_present){
            trainer = try allocator.alloc(u8, 512);
        }
        else
        {
            trainer = null;
        }

        const rom = INesROM{
            .pgr_rom = pgr_rom,
            .chr_rom = chr_rom,
            .mapper = mapper,
            .flags = flags,
            .trainer = trainer
        };
        return rom;
    }

    pub fn free(self: *INesROM, allocator: *std.mem.Allocator) void {
        allocator.free(self.pgr_rom);
        if(self.chr_rom) |chr| {
            allocator.free(chr);
        }
    }

};