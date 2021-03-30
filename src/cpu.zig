const std = @import("std");
const Int = std.meta.Int;
const expect = std.testing.expect;
const warn = std.log.warn;
//const debug = std.debug.print;
const debug = std.log.debug;

const Operation = struct {

    const AdressingMode = enum {
        implicit,
        accumulator,
        immediate,
        zero_page,
        zero_page_x,
        zero_page_y,
        relative,
        absolute,
        absolute_x,
        absolute_y,
        indirect,
        indexed_indirect,
        indirect_indexed,

        pub fn is_absolute(addressing_mode: AdressingMode) bool {
            switch(addressing_mode) {
                .absolute, .absolute_x, .absolute_y => return true,
                else => return false
            }
        }

        pub fn is_plus_register(addressing_mode: AdressingMode) bool {
            switch(addressing_mode) {
                .zero_page_x, .zero_page_y, .absolute_x, .absolute_y => return true,
                else => return false
            }
        }

        pub fn get_register_to_add(addressing_mode: AdressingMode, cpu: *NesCpu) u8 {
            switch(addressing_mode) {
                .zero_page_x, .absolute_x => return cpu.x,
                .zero_page_y, .absolute_y => return cpu.y,
                else => @panic("wrong adressing mode"),
            }
        }
    };

    const operation_fn = fn (cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void;

    op_code_value: u8,
    op_fn: operation_fn,
    addressing_mode: AdressingMode,

    // operation lookup table

    pub fn build(op_code_value: u8, op_fn: operation_fn, addressing_mode: AdressingMode) Operation {
        return Operation{
            .op_code_value = op_code_value,
            .op_fn = op_fn,
            .addressing_mode = addressing_mode,
        };
    } 

    const known_operations = [_]Operation{
        build(0x00, brk, .implicit),
        build(0xEA, nop, .implicit),

        // load register
        build(0xA9, lda, .immediate),
        build(0xA5, lda, .zero_page),
        build(0xB5, lda, .zero_page_x),
        build(0xAD, lda, .absolute),
        build(0xBD, lda, .absolute_x),
        build(0xB9, lda, .absolute_y),
        build(0xA1, lda, .indexed_indirect),
        build(0xB1, lda, .indirect_indexed),

        build(0xA2, ldx, .immediate),
        build(0xA6, ldx, .zero_page),
        build(0xB6, ldx, .zero_page_y),
        build(0xAE, ldx, .absolute),
        build(0xBE, ldx, .absolute_y),
        
        build(0xA0, ldy, .immediate),
        build(0xA4, ldy, .zero_page),
        build(0xB4, ldy, .zero_page_x),
        build(0xAC, ldy, .absolute),
        build(0xBC, ldy, .absolute_x),

        // increment
        build(0xE6, inc, .zero_page),
        build(0xF6, inc, .zero_page_x),
        build(0xEE, inc, .absolute),
        build(0xFE, inc, .absolute_x),

        build(0xE8, inx, .implicit),
        build(0xC8, iny, .implicit),

        // decrement
        build(0xCA, dex, .implicit),
        build(0x88, dey, .implicit),
        build(0xC6, dec, .zero_page),
        build(0xD6, dec, .zero_page_x),
        build(0xCE, dec, .absolute),
        build(0xDE, dec, .absolute_x),

        // store register
        build(0x8E, stx, .absolute),
        build(0x86, stx, .zero_page),
        build(0x96, stx, .zero_page_y),

        // clear flags
        build(0x18, clc, .implicit),
        build(0xD8, cld, .implicit),
        build(0x58, cli, .implicit),

        // set flags
        build(0x38, sec, .implicit),
        build(0xF8, sed, .implicit),
        build(0x78, sei, .implicit),

        build(0xC9, cmp, .immediate),
        build(0xAA, tax, .implicit),
        build(0xE0, cpx, .immediate),
        build(0xD0, bne, .relative),
    };

    const op_lookup : [0xFF]?*const Operation = _get_lookup_table();

    fn _get_lookup_table() [0xFF]?*const Operation {
        var cmp_op_lookup : [0xFF]?*const Operation = .{null} ** 0xFF;

        inline for (known_operations) |*op| {
            cmp_op_lookup[op.op_code_value] = op;
        }

        return cmp_op_lookup;
    }

    fn _get_max_operation_frame_size() usize {
        var size : usize = 0;
        inline for (known_operations) |operation| {
            const op_size = @sizeOf(@Frame(operation.op_fn));
            if (op_size > size) {
                size = op_size;
            }
        }
        return size;
    }

    const max_frame_size = _get_max_operation_frame_size();

    pub fn get_operation(op_code: u8) Operation {
        if(op_lookup[op_code]) |op| {
            return op.*;
        }
        warn("Unknown operation opcode: {x}", .{op_code});
        @panic("Unknown operation for get_operation");
    }

    // operations

    fn sec (cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void {
        cycling.* = false;
        cpu.p.carry = true;
    }

    fn sed (cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void {
        cycling.* = false;
        cpu.p.decimal_mode = true;
    }

    fn sei (cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void {
        cycling.* = false;
        cpu.p.interrupt_disable = true;
    }

    fn clc (cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void {
        cycling.* = false;
        cpu.p.carry = false;
    }

    fn cld (cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void {
        cycling.* = false;
        cpu.p.decimal_mode = false;
    }

    fn cli (cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void {
        cycling.* = false;
        cpu.p.interrupt_disable = false;
    }

    fn inc (cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void {
        var op_frame = async memory_dec_or_inc(cpu, .increment, addressing_mode, cycling);
        while(cycling.*){
            suspend;
            resume op_frame;
        }
    }

    fn dec (cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void {
        var op_frame = async memory_dec_or_inc(cpu, .decrement, addressing_mode, cycling);
        while(cycling.*){
            suspend;
            resume op_frame;
        }
    }

    fn nop (cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void {
        debug("nop", .{});
        cycling.* = false;
    }

    fn cmp (cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void {
        debug("---> cmp\n", .{});
        defer cycling.* = false;

        switch (addressing_mode) {
            .immediate => register_compare_immediate(cpu, &cpu.a),
            else => @panic("Unknown adressing mode for CPX"),
        }

        debug("<--- cmp\n", .{});
    }

    fn bne (cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void {
        debug("---> bne\n", .{});
        defer cycling.* = false;

        const relative_addr : i8 = @bitCast(i8, cpu.fetch());

        if(!cpu.p.zero) {
            suspend;
            debug("bne: jumping to {x} bytes in pc\n", .{relative_addr});
            debug("bne: current pc {x}\n", .{cpu.pc});
            var pc_as_int = @intCast(i32, cpu.pc);
            pc_as_int += relative_addr;
            cpu.pc = @intCast(u16, pc_as_int);
            debug("bne: new pc {x}\n", .{cpu.pc});
        }
        debug("<--- bne\n", .{});
    }

    fn cpx (cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void {
        debug("---> cpx\n", .{});
        defer cycling.* = false;

        switch (addressing_mode) {
            .immediate => register_compare_immediate(cpu, &cpu.x),
            else => @panic("Unknown adressing mode for CPX"),
        }
        debug("<--- cpx\n", .{});
    }

    fn stx (cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void {
        debug("---> stx\n", .{});
        switch(addressing_mode) {
            .absolute, .zero_page, .zero_page_y => {
                var store_frame = async register_store(cpu, &cpu.x, addressing_mode, cycling);
                while(cycling.*) {
                    suspend;
                    resume store_frame;
                }
            },

            else => {
                warn("Unknown adressing mode {x} for stx", .{addressing_mode});
                @panic("Invalid use of stx function");
            }
        }
        
        debug("<--- stx\n", .{});
    }

    fn dex (cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void {
        debug("---> dex cpu.x: {x}\n", .{cpu.x});
        defer cycling.* = false;
        register_decrement(cpu, &cpu.x);
        debug("<--- dex cpu.x: {x}\n", .{cpu.x});
    }

    fn dey (cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void {
        debug("---> dey cpu.y: {x}\n", .{cpu.y});
        defer cycling.* = false;
        register_decrement(cpu, &cpu.y);
        debug("<--- dey cpu.y: {x}\n", .{cpu.y});
    }

    fn inx (cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void {
        debug("---> inx x: {}\n", .{cpu.x});
        defer cycling.* = false;
        _ = @addWithOverflow(u8, cpu.x, 1, &cpu.x);
        cpu.p.set_flag_val_neg(cpu.x);
        cpu.p.set_flag_val_zero(cpu.x);
        debug("<--- inx x: {x}\n", .{cpu.x});
    }

    fn iny (cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void {
        debug("---> inx x: {}\n", .{cpu.y});
        defer cycling.* = false;
        _ = @addWithOverflow(u8, cpu.y, 1, &cpu.y);
        cpu.p.set_flag_val_neg(cpu.y);
        cpu.p.set_flag_val_zero(cpu.y);
        debug("<--- inx x: {x}\n", .{cpu.y});
    }

    fn tax (cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void {
        debug("---> tax a: {}, x: {}\n", .{cpu.a, cpu.x});
        defer cycling.* = false;

        cpu.x = cpu.a;
        cpu.p.set_flag_val_neg(cpu.x);
        cpu.p.set_flag_val_zero(cpu.x);
        debug("<--- tax x: {x}\n", .{cpu.x});
    }

    fn lda (cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void {
        debug("---> lda, adressing: {}, cpu.x: {x}, cpu.y: {x}\n", .{addressing_mode, cpu.x, cpu.y});
        var load_frame = async register_load(cpu, addressing_mode, &cpu.a, cycling);
        suspend;
        while(cycling.*)
        {
            resume load_frame;
            suspend;
        }
        debug("<-- lda value: {x}\n", .{cpu.a});
    }

    fn ldx (cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void {
        debug("---> ldx, adressing: {}, cpu.y: {x}\n", .{addressing_mode, cpu.y});
        var load_frame = async register_load(cpu, addressing_mode, &cpu.x, cycling);
        suspend;
        while(cycling.*)
        {
            resume load_frame;
            suspend;
        }
        debug("<-- ldx value: {x}\n", .{cpu.x});
    }

    fn ldy (cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void {
        debug("---> ldy, adressing: {}, cpu.x: {x}\n", .{addressing_mode, cpu.x});
        var load_frame = async register_load(cpu, addressing_mode, &cpu.y, cycling);
        suspend;
        while(cycling.*)
        {
            resume load_frame;
            suspend;
        }
        debug("<-- ldy value: {x}\n", .{cpu.y});
    }

    fn register_load (cpu: *NesCpu, addressing_mode: AdressingMode, register_target: *u8, cycling: *bool) void {
        defer cycling.* = false;
        
        switch(addressing_mode){
            
            .immediate => {
                register_target.* = cpu.fetch();
            },
            
            .zero_page, .zero_page_x, .zero_page_y => {
                var low_part = cpu.fetch();
                debug("register_load: address low part = {x}", .{low_part});
                suspend; // low part fetch

                if(addressing_mode.is_plus_register()) {
                    const register = addressing_mode.get_register_to_add(cpu);
                    debug("register_load: adding register value {x} to low_part", .{register});
                    const carry = @addWithOverflow(u8, low_part, register, &low_part);
                    debug("register_load: new address low part = {x}", .{low_part});
                    suspend;
                }

                debug("register_load: zero page mode: loading value @{x}", .{low_part});
                register_target.* = cpu.mem_read(u8, low_part);
                debug("register_load: value loaded in register: {x}", .{register_target.*});
            },

            .absolute, .absolute_y, .absolute_x => {
                var low_part = cpu.fetch();
                debug("register_load: address low part = {x}", .{low_part});
                suspend; // low part fetch

                var high_part = cpu.fetch();
                debug("register_load: address high part = {}", .{high_part});
                suspend; // hi part fetch

                if(addressing_mode.is_plus_register()) {
                    const register = addressing_mode.get_register_to_add(cpu);
                    debug("register_load: adding register value {x} to low_part", .{register});
                    const carry = @addWithOverflow(u8, low_part, register, &low_part);
                    debug("register_load: new address low part = {x}", .{low_part});
                    if(carry) {
                        debug("register_load: page crossed on absolute,[register] adressing", .{});
                        _ = @addWithOverflow(u8, high_part, 1, &high_part);
                        debug("register_load: new address high part = {}", .{high_part});
                        suspend;
                    }
                }

                const addr = low_part + (@intCast(u16, high_part) << 8);

                debug("register_load: zero page mode: loading value @{x}", .{addr});
                register_target.* = cpu.mem_read(u8, addr);
                debug("register_load: value loaded in register: {x}", .{register_target.*});
            },

            .indexed_indirect => {
                var param = cpu.fetch();
                debug("register_load: indeXed indirect param = {x}", .{param});
                suspend;

                _ = @addWithOverflow(u8, param, cpu.x, &param);
                suspend;

                const address = cpu.mem_read(u16, param);
                suspend;
                suspend;

                register_target.* = cpu.memory[address];
            },

            .indirect_indexed => {
                var param = cpu.fetch();
                debug("register_load: indirect indexed param = {x}", .{param});
                suspend;

                var address_lsb = cpu.mem_read(u8, param);
                debug("register_load: address lsb for indirect indexed @param: {x}", .{address_lsb});
                const carry = @addWithOverflow(u8, address_lsb, cpu.y, &address_lsb);
                debug("register_load: address lsb + y = {}, carry = {}", .{address_lsb, carry});
                suspend;

                var address_msb = cpu.mem_read(u8, param+1);
                debug("register_load: address msb for indirect indexed @param+1: {x}", .{address_msb});
                suspend;

                if(carry) {
                    address_msb += 1;
                    debug("register_load: carry was positive, msb += 1, now {x}", .{address_msb});
                    suspend;
                }

                const address : u16 = address_lsb + (@intCast(u16, address_msb) << 8); 
                debug("register_load: address is now {x}", .{address});
                register_target.* = cpu.memory[address];
                debug("register_load: register value loaded from @address {}", .{register_target.*});
            },

            else => @panic("Unknown adressing mode for register_load")
        }
        cpu.p.set_flags_val_zero_and_neg(register_target.*);
    }

    fn brk(cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void {
        debug("---> brk\n", .{});
        defer cycling.* = false;
        suspend;
        suspend;
        suspend;
        suspend;
        suspend;
        suspend;
        debug("<--- brk\n", .{});
    }

    // helpers

    const SubOp = enum {
        increment,
        decrement
    };

    fn memory_dec_or_inc (cpu: *NesCpu, sub_op: SubOp, addressing_mode: AdressingMode, cycling: *bool, ) void {
        defer cycling.* = false;
         
        var address : u16 = 0;
        var get_address_cycling = true;
        var get_address_frame = async get_address(cpu, addressing_mode, &get_address_cycling, &address);
        
        while(get_address_cycling) {
            suspend;
            resume get_address_frame;
        }

        var value = cpu.mem_read(u8, address);
        suspend;

        if(sub_op == .increment) {
            _ = @addWithOverflow(u8, value, 1, &value);
        }
        else 
        {
            _ = @subWithOverflow(u8, value, 1, &value);
        }
        suspend;

        cpu.mem_write(u8, address, value);
        cpu.p.set_flags_val_zero_and_neg(value);           
    }

    fn register_decrement(cpu: *NesCpu, register: *u8) void {
        _ = @subWithOverflow(u8, register.*, 1, register);
    }

    fn get_address(cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool, out_address: *u16) void {
        defer cycling.* = false;
        out_address.* = 0;

        switch(addressing_mode) {
            .absolute, .absolute_x => {
                out_address.* = cpu.fetch();
                suspend; // low part fetch

                out_address.* += (@intCast(u16, cpu.fetch()) << 8);
                suspend; // high part fetch

                if(addressing_mode.is_plus_register()) {
                    const register_to_add = addressing_mode.get_register_to_add(cpu);
                    _ = @addWithOverflow(u16, out_address.*, register_to_add, out_address);
                    suspend;
                }
            },

            .zero_page, .zero_page_y, .zero_page_x => {
                out_address.* = cpu.fetch();
                suspend;

                if(addressing_mode.is_plus_register()) {
                    const register_to_add = addressing_mode.get_register_to_add(cpu);
                    out_address.* = (out_address.* + register_to_add) & 0xFF;
                    suspend;
                }
            },  

            else => {
                warn("adressing mode {} not implemented yet", .{addressing_mode});
                @panic("Error in get_address");
            }
        }
        debug("get_address: {x}", .{out_address.*});
    }

    fn register_store(cpu: *NesCpu, register: *u8, addressing_mode: AdressingMode, cycling: *bool) void {
        debug("---> register_store adressing: {}\n", .{addressing_mode});
        defer cycling.* = false;
        var address : u16 = 0;
        
        var get_address_cycling = true;
        var get_address_frame = async get_address(cpu, addressing_mode, &get_address_cycling, &address);
        
        while(get_address_cycling) {
            suspend;
            resume get_address_frame;
        }

        cpu.mem_write(u8, address, register.*);
        
        debug("<--- register_store\n", .{});
    }

    fn register_compare(cpu: *NesCpu, register_value: u8, compare_value: u8) void {
        debug("---> register_compare register_value: {x}, compare_value: {x}\n", .{register_value, compare_value});
        if(register_value >= compare_value) {
            cpu.p.carry = true;
        } else {
            cpu.p.carry = false;
        }
        
        cpu.p.zero = (register_value == compare_value);
        var cmp_value_calc : u8 = 0;
        _ = @subWithOverflow(u8, register_value, compare_value, &cmp_value_calc);

        cpu.p.negative = ((cmp_value_calc >> 7) == 1);
        debug("<--- register_compare carry: {}, zero: {}, cmp_value_calc: {x}, negative: {}\n", .{cpu.p.carry, cpu.p.zero, cmp_value_calc, cpu.p.negative});
    }

    fn register_compare_immediate(cpu: *NesCpu, register: *u8) void {
        debug("---> register_compare_immediate\n", .{});
        register_compare(cpu, register.*, cpu.fetch());
        debug("<--- register_compare_immediate\n", .{});
    }
};

const NesCpu = struct {

    const ProcessorStatus = struct {
        carry: bool,
        zero: bool,
        interrupt_disable: bool,
        decimal_mode: bool,
        break_cmd: bool,
        unused: bool,
        overflow: bool,
        negative: bool,

        fn init() ProcessorStatus {
            return ProcessorStatus {
                .carry = false,
                .zero = false,
                .interrupt_disable = false,
                .decimal_mode = false,
                .break_cmd = false,
                .unused = true,
                .overflow = false,
                .negative = false,
            };
        }

        pub fn set_flag_val_zero(self: *ProcessorStatus, value: u8) void {
            if(value == 0){
                self.zero = true;
            } else {
                self.zero = false;
            }
        }

        pub fn set_flag_val_neg(self: *ProcessorStatus, value: u8) void {
            if((value & 0b10000000) != 0){
                self.negative = true;
            } else {
                self.negative = false;
            }
        }

        pub fn set_flags_val_zero_and_neg(self: *ProcessorStatus, value: u8) void {
            self.set_flag_val_neg(value);
            self.set_flag_val_zero(value);
        }

        pub fn debug(self: *ProcessorStatus) void {
            debug("{}", .{self});
        }
    };

    const Internal = struct {
        cycle_count: usize,
        fetch_count: usize,
        progam_len: usize,
        require_new_cycle_frame: bool,
        progam_running: bool,
    };

    internal: Internal,

    a: u8, //accumulator
    x: u8,
    y: u8,
    sp: u8, // stack pointer
    p: ProcessorStatus, // processor status flags
    pc: u16, // program counter
    memory: [0xFFFF]u8,

    pub fn init() NesCpu {
        var nescpu = NesCpu{
            .a = 0,
            .x = 0,
            .y = 0,
            .sp = 0,
            .p = ProcessorStatus.init(),
            .pc = 0x8000,
            .memory = [_]u8{0} ** 0xFFFF,
            .internal = Internal{
                .cycle_count = 0,
                .fetch_count = 0,
                .progam_len = 0,
                .progam_running = false,
                .require_new_cycle_frame = true,
            }
        };
        return nescpu;
    }

    fn mem_write(self: *NesCpu, comptime T: type, address: u16, value: T) void {
        comptime {
            expect(@bitSizeOf(T) >= 8);
            expect((@bitSizeOf(T) % 8) == 0);
        }

        var index : u16 = 0;
        var val = value;
        comptime const num_shifts : u8 = @bitSizeOf(T) / 8;
        
        while (index < num_shifts) : (index += 1) {
            self.memory[address + index] = @intCast(u8, val & 0xFF);
            val = @intCast(T, @intCast(usize, value) >> 8);
        }
    }

    fn mem_read(self: *NesCpu, comptime T: type, address: u16) T {
        comptime {
            expect(@bitSizeOf(T) >= 8);
            expect((@bitSizeOf(T) % 8) == 0);
        }

        comptime const num_bytes : u3 = @bitSizeOf(T) / 8;
        var value : T = 0;
        comptime var index: u4 = 1;
        comptime const bitshift: u4 = 8;

        value = self.memory[address];

        inline while (index < num_bytes) : (index += 1) {
            var temp : T = 0;
            temp = self.memory[address + index];
            temp <<= (index * bitshift);
            value += temp;
        }

        return value;
    }

    pub fn load(self: *NesCpu, program: []u8) void {
        debug("--> load\n", .{});
        for (program[0..program.len]) |b, i| self.memory[i + 0x8000] = b;
        self.mem_write(u16, 0xFFFC, 0x8000);
        self.internal.progam_len = program.len + 0x8000;
        debug("<-- load\n", .{});
    }

    pub fn reset(self: *NesCpu) void {
        debug("--> reset\n", .{});
        self.a = 0;
        self.x = 0;
        self.sp = 0;
        self.p = ProcessorStatus.init();
        self.y = 0;
        self.pc = self.mem_read(u16, 0xFFFC);
        debug("<-- reset\n", .{});
    }

    fn fetch(self: *NesCpu) u8 {
        debug("--> fetch self.pc: {x}\n", .{self.pc});
        const val: u8 = self.memory[self.pc];
        self.pc += 1;
        self.internal.fetch_count += 1;
        debug("<-- fetch {x}\n", .{val});
        return val;
    }

    
    pub fn start_cpu_frame(self: *NesCpu) void {
        self.internal.progam_running = true;
        self.internal.cycle_count = 0;
        defer self.internal.progam_running = false;
        while(self.pc < self.internal.progam_len){
            var internal_frame = async self.cycle();
            suspend;

            while(self.internal.require_new_cycle_frame == false) {
                resume internal_frame;
                suspend;
            }
        }
    }

    fn cycle(self: *NesCpu) void {
        debug("--> cycle fetch op\n", .{});
        const opcode = self.fetch();
        self.internal.require_new_cycle_frame = false;
        defer self.internal.require_new_cycle_frame = true;
        _ = @addWithOverflow(usize, self.internal.cycle_count, 1, &self.internal.cycle_count);
        debug("<-- fetch op cycle\n", .{});
        suspend; // first fetch always costs a cycle
        debug("--> cycle start op\n", .{});
        const op = Operation.get_operation(opcode);
        var cycling = true;
        
        var bytes: [Operation.max_frame_size]u8 align(16) = undefined;

        var op_frame = @asyncCall(&bytes, {}, op.op_fn, .{self, op.addressing_mode, &cycling});
        while(cycling) {
            self.internal.cycle_count += 1;
            debug("<-- cycle continue op\n", .{});
            suspend;
            debug("--> cycle continue op\n", .{});
            resume op_frame;
        }
        self.internal.cycle_count += 1;
        debug("<-- cycle end op\n", .{});
    }

    pub fn interpret(self: *NesCpu) void {
        var cpu_frame = async self.start_cpu_frame();
        while(self.internal.progam_running) {
            resume cpu_frame;
        }
    }

    pub fn load_and_interpret(self: *NesCpu, program: []u8) void {
        self.load(program);
        self.reset();
        self.interpret();
    }

};

test "test_5_ops_working_together" {
    var basic_progam = [_]u8{0xA9, 0xC0, 0xAA, 0xE8, 00};
    var cpu =  NesCpu.init();
    cpu.load(&basic_progam);
    cpu.reset();

    var cpu_frame = async cpu.start_cpu_frame();
    // A9 C0 (2 cycles)
    expect(cpu.a == 0x00);
    expect(cpu.pc == 0x8000 + 1);
    resume cpu_frame;
    expect(cpu.a == 0xC0);
    expect(cpu.pc == 0x8000 + 2);

    // AA (2 cycles)
    resume cpu_frame;
    expect(cpu.pc == 0x8000 + 3);
    expect(cpu.x == 0);
    resume cpu_frame;
    expect(cpu.pc == 0x8000 + 3);
    expect(cpu.x == 0xC0);

    // INX (2 cycles)
    resume cpu_frame;
    resume cpu_frame;
    expect(cpu.pc == 0x8000 + 4);
    expect(cpu.x == 0xC1);

    // BRK (7 cycles)
    resume cpu_frame;
    resume cpu_frame;
    resume cpu_frame;
    resume cpu_frame;
    resume cpu_frame;
    resume cpu_frame;
    resume cpu_frame;

    expect(cpu.pc == 0x8000 + 5);
}

test "test_inx_overflow" {
    var basic_progam = [_]u8{0xE8, 0xE8, 0x00};    
    var cpu =  NesCpu.init();
    cpu.load(&basic_progam);
    cpu.reset();
    cpu.x = 0xFF;
    cpu.interpret();
    expect(cpu.x == 1);
}

test "Branching from https://skilldrick.github.io/easy6502/" {
    var basic_progam = [_]u8{0xa2, 0x08, 0xca, 0x8e, 0x00, 0x02, 0xe0, 0x03, 0xd0, 0xf8, 0x8e, 0x01, 0x02, 0x00};
    var cpu =  NesCpu.init();
    cpu.load_and_interpret(&basic_progam);

    expect(cpu.x == 3);
}

test "relative adressing" {
    var cpu =  NesCpu.init();
    var basic_program = [_]u8{0xa9, 0x01, 0xc9, 0x02, 0xd0, 0x02, 0x85, 0x22, 0x00};
    cpu.load_and_interpret(&basic_program);

    expect(cpu.a == 1);
}

test "lda" {
    var cpu =  NesCpu.init();

    cpu.memory[0x0001] = 0x02;
    cpu.memory[0x0002] = 0x03;
    cpu.memory[0x0004] = 0x00;
    cpu.memory[0x0005] = 0x02;
    cpu.memory[0x0200] = 0xAA;
    cpu.memory[0x0210] = 0xAB;
    cpu.memory[0x0300] = 0xBB;

    // immediate mode
    var lda_immediate = [_]u8{0xA9, 0x01};
    cpu.load_and_interpret(&lda_immediate);
    expect(cpu.a == 1);
    expect(cpu.internal.cycle_count == 2);


    // checking flag setting in immediate mode
    // zero
    var lda_immediate_flag_0 = [_]u8{0xA9, 0x00};
    cpu.load_and_interpret(&lda_immediate_flag_0);
    expect(cpu.a == 0);
    expect(cpu.p.zero);
    expect(!cpu.p.negative);
    expect(cpu.internal.cycle_count == 2);

    // negative
    var lda_immediate_flag_neg = [_]u8{0xA9, 0xFE};
    cpu.load_and_interpret(&lda_immediate_flag_neg);
    expect(cpu.a == 0xFE);
    expect(!cpu.p.zero);
    expect(cpu.p.negative);
    expect(cpu.internal.cycle_count == 2);

    // neither
    var lda_immediate_flag_default = [_]u8{0xA9, 0x05};
    cpu.load_and_interpret(&lda_immediate_flag_default);
    expect(cpu.a == 0x05);
    expect(!cpu.p.zero);
    expect(!cpu.p.negative);
    expect(cpu.internal.cycle_count == 2);


    // checking adress modes
    var lda_zero_page = [_]u8{0xA5, 0x01};
    cpu.load_and_interpret(&lda_zero_page);
    expect(cpu.a == 2);
    expect(cpu.internal.cycle_count == 3);


    var lda_zero_page_x = [_]u8{0xB5, 0x01};
    cpu.load(&lda_zero_page_x);
    cpu.reset();
    cpu.x = 0x01;
    cpu.interpret();
    expect(cpu.a == 0x03);
    expect(cpu.internal.cycle_count == 4);


    var lda_zero_page_x_wrapparound = [_]u8{0xB5, 0x03};
    cpu.load(&lda_zero_page_x_wrapparound);
    cpu.reset();
    cpu.x = 0xFF;
    cpu.interpret();
    expect(cpu.a == 0x03);
    expect(cpu.internal.cycle_count == 4);


    var lda_absolute = [_]u8{0xAD, 0x00, 0x02};
    cpu.load_and_interpret(&lda_absolute);
    expect(cpu.a == 0xAA);
    expect(cpu.internal.cycle_count == 4);


    var lda_absolute_x = [_]u8{0xBD, 0x00, 0x02};
    cpu.load(&lda_absolute_x);
    cpu.reset();
    cpu.x = 0x10;
    cpu.interpret();
    expect(cpu.a == 0xAB);
    expect(cpu.internal.cycle_count == 4);


    var lda_absolute_x_page_crossed = [_]u8{0xBD, 0xFF, 0x02};
    cpu.load(&lda_absolute_x_page_crossed);
    cpu.reset();
    cpu.x = 0x01;
    cpu.interpret();
    expect(cpu.a == 0xBB);
    expect(cpu.internal.cycle_count == 5);


    var lda_absolute_y = [_]u8{0xB9, 0x00, 0x02};
    cpu.load(&lda_absolute_y);
    cpu.reset();
    cpu.y = 0x10;
    cpu.interpret();
    expect(cpu.a == 0xAB);
    expect(cpu.internal.cycle_count == 4);


    var lda_absolute_y_page_crossed = [_]u8{0xB9, 0xFF, 0x02};
    cpu.load(&lda_absolute_y_page_crossed);
    cpu.reset();
    cpu.y = 0x01;
    cpu.interpret();
    expect(cpu.a == 0xBB);
    expect(cpu.internal.cycle_count == 5);


    for (cpu.memory) |*b| b.* = 0; //resetting memory
    var lda_indexed_indirect = [_]u8{0xa1, 0x00};
    cpu.load(&lda_indexed_indirect);
    cpu.reset();
    cpu.memory[0x0705] = 0xAA;
    cpu.memory[0x0001] = 0x05;
    cpu.memory[0x0002] = 0x07;
    cpu.x = 0x01;
    cpu.interpret();
    expect(cpu.a == 0xAA);
    expect(cpu.internal.cycle_count == 6);
    // basically: param + x = 0x0001. u16 @ 0x0001 = 0x0705, so load value @0x0705 in LDA.
    // fetch op -> fetch param -> calc param +x (wrapparound u8) -> fetch lsb address-> fetch msb address-> load lda w/ @address
    //not doing wrapparound, its on param + x, easy to handle.
    

    for (cpu.memory) |*b| b.* = 0; //resetting memory
    var lda_indirect_indexed = [_]u8{0xB1, 0x01};
    cpu.load(&lda_indirect_indexed);
    cpu.reset();
    cpu.memory[0x0203] = 0xAA;
    cpu.memory[0x0001] = 0x00;
    cpu.memory[0x0002] = 0x02;
    cpu.y = 0x03;
    cpu.interpret();
    expect(cpu.a == 0xAA);
    expect(cpu.internal.cycle_count == 5);
    // fetch op -> fetch param -> fetch lsb address + y (+1 if wrapparound) -> fetch msb address -> load lda w/ @address

    for (cpu.memory) |*b| b.* = 0; //resetting memory
    var lda_indirect_indexed_page_crossed = [_]u8{0xB1, 0x01};
    cpu.load(&lda_indirect_indexed_page_crossed);
    cpu.reset();
    cpu.memory[0x0301] = 0xAA;
    cpu.memory[0x0001] = 0x02;
    cpu.memory[0x0002] = 0x02;
    cpu.y = 0xFF;
    cpu.interpret();
    expect(cpu.a == 0xAA);
    expect(cpu.internal.cycle_count == 6);
}

test "stx" {
    var cpu =  NesCpu.init();

    var stx_zero_page = [_]u8{0x86, 0x50};
    cpu.load(&stx_zero_page);
    cpu.reset();
    cpu.x = 0xAA;
    cpu.interpret();
    expect(cpu.memory[0x50] == 0xAA);
    expect(cpu.internal.cycle_count == 3);

    var stx_zero_page_y = [_]u8{0x96, 0x50};
    cpu.load(&stx_zero_page_y);
    cpu.reset();
    cpu.x = 0xBB;
    cpu.y = 0x05;
    cpu.interpret();
    expect(cpu.memory[0x55] == 0xBB);
    expect(cpu.internal.cycle_count == 4);

    // with wrapparound
    cpu.load(&stx_zero_page_y);
    cpu.reset();
    cpu.x = 0xCC;
    cpu.y = 0xB2;
    cpu.interpret();
    expect(cpu.memory[0x02] == 0xCC);
    expect(cpu.internal.cycle_count == 4);
}

test "inc" {
    var cpu =  NesCpu.init();

    var zero_page = [_]u8{0xE6, 0x20};
    cpu.load_and_interpret(&zero_page);
    expect(cpu.memory[0x0020] == 1);
    expect(cpu.internal.cycle_count == 5);

    // with overflow
    cpu.memory[0x0020] = 0xFF;
    cpu.load_and_interpret(&zero_page);
    expect(cpu.memory[0x0020] == 0);
    expect(cpu.internal.cycle_count == 5);

    var zero_page_x = [_]u8{0xF6, 0x20};
    cpu.load(&zero_page_x);
    cpu.reset();
    cpu.x = 0x05;
    cpu.interpret();
    expect(cpu.memory[0x0025] == 1);
    expect(cpu.internal.cycle_count == 6);

    var absolute = [_]u8{0xEE, 0x50, 0x10};
    cpu.load_and_interpret(&absolute);
    expect(cpu.memory[0x1050] == 1);
    expect(cpu.internal.cycle_count == 6);

    var absolute_x = [_]u8{0xFE, 0x50, 0x10};
    cpu.load(&absolute_x);
    cpu.reset();
    cpu.x = 0x05;
    cpu.interpret();
    expect(cpu.memory[0x1055] == 1);
    expect(cpu.internal.cycle_count == 7);
}

test "clc" {
    var cpu =  NesCpu.init();

    var clc = [_]u8{0x18};
    cpu.load(&clc);
    cpu.reset();
    cpu.p.carry = true;
    cpu.interpret();

    expect(cpu.p.carry == false);
    expect(cpu.internal.cycle_count == 2);

    cpu.load(&clc);
    cpu.reset();
    cpu.p.carry = false;
    cpu.interpret();
    
    expect(cpu.p.carry == false);
    expect(cpu.internal.cycle_count == 2);

}

test "cld" {
    var cpu =  NesCpu.init();

    var cld = [_]u8{0xD8};
    cpu.load(&cld);
    cpu.reset();
    cpu.p.decimal_mode = true;
    cpu.interpret();

    expect(cpu.p.decimal_mode == false);
    expect(cpu.internal.cycle_count == 2);

    cpu.load(&cld);
    cpu.reset();
    cpu.p.decimal_mode = false;
    cpu.interpret();
    
    expect(cpu.p.decimal_mode == false);
    expect(cpu.internal.cycle_count == 2);

}

test "cli" {
    var cpu = NesCpu.init();

    var cli = [_]u8{0x58};
    cpu.load(&cli);
    cpu.reset();
    cpu.p.interrupt_disable = true;
    cpu.interpret();

    expect(cpu.p.interrupt_disable == false);
    expect(cpu.internal.cycle_count == 2);

    cpu.load(&cli);
    cpu.reset();
    cpu.p.interrupt_disable = false;
    cpu.interpret();
    
    expect(cpu.p.interrupt_disable == false);
    expect(cpu.internal.cycle_count == 2);
}

test "sec" {
    var cpu = NesCpu.init();

    var sec = [_]u8{0x38};
    cpu.load(&sec);
    cpu.reset();
    cpu.p.carry = true;
    cpu.interpret();

    expect(cpu.p.carry == true);
    expect(cpu.internal.cycle_count == 2);

    cpu.load(&sec);
    cpu.reset();
    cpu.p.carry = false;
    cpu.interpret();
    
    expect(cpu.p.carry == true);
    expect(cpu.internal.cycle_count == 2);
}

test "sed" {
    var cpu = NesCpu.init();

    var sed = [_]u8{0xF8};
    cpu.load(&sed);
    cpu.reset();
    cpu.p.decimal_mode = true;
    cpu.interpret();

    expect(cpu.p.decimal_mode == true);
    expect(cpu.internal.cycle_count == 2);

    cpu.load(&sed);
    cpu.reset();
    cpu.p.decimal_mode = false;
    cpu.interpret();
    
    expect(cpu.p.decimal_mode == true);
    expect(cpu.internal.cycle_count == 2);
}

test "sei" {
    var cpu = NesCpu.init();

    var sei = [_]u8{0x78};
    cpu.load(&sei);
    cpu.reset();
    cpu.p.interrupt_disable = true;
    cpu.interpret();

    expect(cpu.p.interrupt_disable == true);
    expect(cpu.internal.cycle_count == 2);

    cpu.load(&sei);
    cpu.reset();
    cpu.p.interrupt_disable = false;
    cpu.interpret();
    
    expect(cpu.p.interrupt_disable == true);
    expect(cpu.internal.cycle_count == 2);
}

test "dex" {
    var cpu = NesCpu.init();

    var program = [_]u8{0xCA};
    cpu.load(&program);
    cpu.reset();
    cpu.x = 1;
    cpu.interpret();

    expect(cpu.internal.cycle_count == 2);
    expect(cpu.x == 0);
}

test "dey" {
    var cpu = NesCpu.init();

    var program = [_]u8{0x88};
    cpu.load(&program);
    cpu.reset();
    cpu.y = 1;
    cpu.interpret();

    expect(cpu.internal.cycle_count == 2);
    expect(cpu.y == 0);
}

test "dec" {
    var cpu =  NesCpu.init();

    var zero_page = [_]u8{0xC6, 0x20};
    cpu.load_and_interpret(&zero_page);
    expect(cpu.memory[0x0020] == 0xFF);
    expect(cpu.internal.cycle_count == 5);

    // with overflow
    cpu.memory[0x0020] = 0xFF;
    cpu.load_and_interpret(&zero_page);
    expect(cpu.memory[0x0020] == 0xFE);
    expect(cpu.internal.cycle_count == 5);

    var zero_page_x = [_]u8{0xD6, 0x20};
    cpu.load(&zero_page_x);
    cpu.reset();
    cpu.x = 0x05;
    cpu.interpret();
    expect(cpu.memory[0x0025] == 0xFF);
    expect(cpu.internal.cycle_count == 6);

    var absolute = [_]u8{0xCE, 0x50, 0x10};
    cpu.load_and_interpret(&absolute);
    expect(cpu.memory[0x1050] == 0xFF);
    expect(cpu.internal.cycle_count == 6);

    var absolute_x = [_]u8{0xDE, 0x50, 0x10};
    cpu.load(&absolute_x);
    cpu.reset();
    cpu.x = 0x05;
    cpu.interpret();
    expect(cpu.memory[0x1055] == 0xFF);
    expect(cpu.internal.cycle_count == 7);
}