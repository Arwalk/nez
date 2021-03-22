const std = @import("std");
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

        // a register
        build(0xA9, lda, .immediate),
        build(0xA5, lda, .zero_page),

        build(0xAA, tax, .implicit),

        // x register
        build(0xE8, inx, .implicit),
        
        build(0xA2, ldx, .immediate),

        build(0xCA, dex, .implicit),

        build(0x8E, stx, .absolute),

        build(0xE0, cpx, .immediate),

        // branching
        build(0xD0, bne, .relative),
    };

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
        for (known_operations) |op| {
            if(op.op_code_value == op_code){
                return op;
            }
        }
        warn("Unknown operation opcode: {x}", .{op_code});
        @panic("Unknown operation for KnownOps.get_operation");
    }
    
    // helpers

    fn register_load_immediate(cpu: *NesCpu, register: *u8) void {
        register.* = cpu.fetch();
        cpu.p.set_flags_val_and_neg(register.*);
    }

    fn register_decrement(cpu: *NesCpu, register: *u8) void {
        _ = @subWithOverflow(u8, register.*, 1, register);
    }

    fn register_store_absolute(cpu: *NesCpu, register: *u8, cycling: *bool) void {
        debug("---> register_store_absolute\n", .{});
        defer cycling.* = false;
        
        var address : u16 = cpu.fetch();
        suspend; // first part fetch

        address += (@intCast(u16, cpu.fetch()) << 8);
        suspend; // second part fetch
    
        debug("register_store_absolute: store value {x} @{x} \n", .{register.*, address});

        cpu.mem_write_u8(address, register.*);
        debug("<--- register_store_absolute\n", .{});
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

    // operations

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
        defer cycling.* = false;

        switch (addressing_mode) {
            .absolute => {
                var store_frame = async register_store_absolute(cpu, &cpu.x, cycling);
                while(cycling.*) {
                    suspend;
                    resume store_frame;
                }
            },

            else => @panic("Unknown adresing mode for STX"),
        }
        debug("<--- stx\n", .{});
    }

    fn dex (cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void {
        debug("---> dex cpu.x: {x}\n", .{cpu.x});
        defer cycling.* = false;
        register_decrement(cpu, &cpu.x);
        debug("<--- dex cpu.x: {x}\n", .{cpu.x});
    }

    fn ldx (cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void {
        debug("---> ldx\n", .{});
        defer cycling.* = false;

        switch(addressing_mode) {
            .immediate => {
                register_load_immediate(cpu, &cpu.x);
            },

            else => @panic("Unknown adresing mode for LDX"),
        }
        debug("<--- ldx cpu.x: {x}\n", .{cpu.x});
    }

    fn inx (cpu: *NesCpu, addressing_mode: AdressingMode, cycling: *bool) callconv(.Async) void {
        debug("---> inx x: {}\n", .{cpu.x});
        defer cycling.* = false;
        _ = @addWithOverflow(u8, cpu.x, 1, &cpu.x);
        cpu.p.set_flag_val_neg(cpu.x);
        cpu.p.set_flag_val_zero(cpu.x);
        debug("<--- inx x: {x}\n", .{cpu.x});
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
        debug("---> lda\n", .{});
        defer cycling.* = false;
        
        switch(addressing_mode){
            
            .immediate => {
                register_load_immediate(cpu, &cpu.a);
            },
            
            .zero_page => {
                const addr = cpu.fetch();
                suspend;
                debug("lda zero page mode: loading value @{x}", .{addr});
                cpu.a = cpu.mem_read_u8(addr);
                cpu.p.set_flags_val_and_neg(cpu.a);
            },

            else => @panic("Unknown adresing mode for LDA")
        }
        debug("<--- lda a: {x}\n", .{cpu.a});
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

        fn get_raw_value(self: ProcessorStatus) u8 {
            var value: u8 = 0;
            if(self.carry)
            {
                value += 1;
            }

            if(self.zero)
            {
                value += (1 << 1);
            }
            if(self.interrupt_disable)
            {
                value += (1 << 2);
            }
            if(self.decimal_mode)
            {
                value += (1 << 3);
            }
            if(self.break_cmd)
            {
                value += (1 << 4);
            }
            if(self.unused)
            {
                value += (1 << 5);
            }
            if(self.overflow)
            {
                value += (1 << 6);
            }
            if(self.negative)
            {
                value += (1 << 7);
            }
            return value;
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

        pub fn set_flags_val_and_neg(self: *ProcessorStatus, value: u8) void {
            self.set_flag_val_neg(value);
            self.set_flag_val_zero(value);
        }

        pub fn debug(self: *ProcessorStatus) void {
            debug("{}", .{self});
        }
    };

    const Internal = struct {
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
                .fetch_count = 0,
                .progam_len = 0,
                .progam_running = false,
                .require_new_cycle_frame = true,
            }
        };
        return nescpu;
    }

    fn mem_write_u16(self: *NesCpu, addr: u16, val: u16) void {
        self.memory[addr] = @intCast(u8, val & 0xFF);
        self.memory[addr+1] = @intCast(u8, val >> 8);
    }

    fn mem_write_u8(self: *NesCpu, addr: u16, val: u8) void {
        self.memory[addr] = val;
    }

    fn mem_read_u16(self: *NesCpu, addr: u16) u16 {
        var val : u16 = self.memory[addr];
        val += (@intCast(u16, self.memory[addr+1]) << 8);
        return val;
    }

    fn mem_read_u8(self: *NesCpu, addr: u16) u8 {
        return self.memory[addr];
    }

    pub fn load(self: *NesCpu, program: []u8) void {
        debug("--> load\n", .{});
        for (program[0..program.len]) |b, i| self.memory[i + 0x8000] = b;
        self.mem_write_u16(0xFFFC, 0x8000);
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
        self.pc = self.mem_read_u16(0xFFFC);
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
        debug("--> cycle\n", .{});
        const opcode = self.fetch();
        self.internal.require_new_cycle_frame = false;
        defer self.internal.require_new_cycle_frame = true;
        debug("<-- cycle\n", .{});
        suspend; // first fetch always costs a cycle
        debug("--> cycle\n", .{});
        const op = Operation.get_operation(opcode);
        var cycling = true;
        
        var bytes: [Operation.max_frame_size]u8 align(16) = undefined;

        var op_frame = @asyncCall(&bytes, {}, op.op_fn, .{self, op.addressing_mode, &cycling});
        while(cycling) {
            debug("<-- cycle\n", .{});
            suspend;
            debug("--> cycle\n", .{});
            resume op_frame;
        }
        debug("<-- cycle\n", .{});
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

test "get raw value ProcessorStatus" {
    var p = NesCpu.ProcessorStatus.init();
    p.unused = false;

    expect(0 == p.get_raw_value());

    p.carry = true;
    expect(1 == p.get_raw_value());

    p.zero = true;
    expect(3 == p.get_raw_value());

    p.interrupt_disable = true;
    expect(7 == p.get_raw_value());

    p.decimal_mode = true;
    expect(15 == p.get_raw_value());
    
    p.break_cmd = true;
    expect(31 == p.get_raw_value());
    
    p.unused = true;
    expect(63 == p.get_raw_value());
    
    p.overflow = true;
    expect(127 == p.get_raw_value());
    
    p.negative = true;
    expect(255 == p.get_raw_value());
}

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

test "test_lda_from_memory" {
    var basic_progam = [_]u8{0xa5, 0x10, 0x00};    
    var cpu =  NesCpu.init();
    cpu.mem_write_u8(0x10, 0x55);
    cpu.load_and_interpret(&basic_progam);

    expect(cpu.a == 0x55);
}

test "Branching from https://skilldrick.github.io/easy6502/" {
    var basic_progam = [_]u8{0xa2, 0x08, 0xca, 0x8e, 0x00, 0x02, 0xe0, 0x03, 0xd0, 0xf8, 0x8e, 0x01, 0x02, 0x00};
    var cpu =  NesCpu.init();
    cpu.load_and_interpret(&basic_progam);

    expect(cpu.x == 3);
    expect(cpu.pc == 0x8000 + 0xE);
}