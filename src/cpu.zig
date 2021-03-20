const std = @import("std");
const expect = std.testing.expect;
const debug = std.log.debug;

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
};

const OpCode = enum(u8) {
    BRK = 0, // break!
    LDA_IMMEDIATE = 0xA9, // load accumulator immediate adressing
    TAX = 0xAA, // Transfer accumulator to X
    INX = 0xE8, // Increment X
    _,
};

const NesCpu = struct {
    const Internal = struct {
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

    program: []u8, // program pointer

    pub fn init(program: []u8) NesCpu {
        return NesCpu{
            .a = 0,
            .x = 0,
            .y = 0,
            .sp = 0,
            .p = ProcessorStatus.init(),
            .pc = 0,
            .program = program,
            .internal = Internal{
                .progam_running = false,
                .require_new_cycle_frame = true,
            }
        };
    }

    fn fetch(self: *NesCpu) u8 {
        debug("--> fetch", .{});
        debug("fetching data at index self.pc: {}", .{self.pc});
        const val: u8 = self.program[self.pc];
        self.pc += 1;
        debug("<-- fetch {}", .{val});
        return val;
    }

    
    pub fn start_cpu_frame(self: *NesCpu) void {
        self.internal.progam_running = true;
        while(self.pc < self.program.len){
            var internal_frame = async self.cycle();
            suspend;

            while(self.internal.require_new_cycle_frame == false) {
                resume internal_frame;
                suspend;
            }
        }
        self.internal.progam_running = false;
    }

    fn cycle(self: *NesCpu) void {
        const opcode = @intToEnum(OpCode, self.fetch());
        self.internal.require_new_cycle_frame = false;
        suspend; // first fetch always costs a cycle

        switch (opcode){
            OpCode.LDA_IMMEDIATE => { // LDA : 
                // load the value in accumulator
                self.a = self.fetch();
                // set flags
                self.p.set_flag_val_neg(self.a);
                self.p.set_flag_val_zero(self.a);
            },
            
            OpCode.BRK => {
                suspend;
                suspend;
                suspend;
                suspend;
                suspend;
                suspend;
            },

            OpCode.TAX => {
                self.x = self.a;
                self.p.set_flag_val_neg(self.x);
                self.p.set_flag_val_zero(self.x);
            },

            OpCode.INX => {
                _ = @addWithOverflow(u8, self.x, 1, &self.x);
                self.p.set_flag_val_neg(self.x);
                self.p.set_flag_val_zero(self.x);
            },

            else => @panic("unknown instruction"),
        }

        self.internal.require_new_cycle_frame = true;
    }

    pub fn interpret(self: *NesCpu) void {
        var cpu_frame = async self.start_cpu_frame();
        while(self.internal.progam_running) {
            resume cpu_frame;
        }
    }

};

test "get raw value ProcessorStatus" {
    var p = ProcessorStatus.init();
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
    var cpu =  NesCpu.init(&basic_progam);

    var cpu_frame = async cpu.start_cpu_frame();
    // A9 C0 (2 cycles)
    expect(cpu.a == 0x00);
    expect(cpu.pc == 1);
    resume cpu_frame;
    expect(cpu.a == 0xC0);
    expect(cpu.pc == 2);

    // AA (2 cycles)
    resume cpu_frame;
    expect(cpu.pc == 3);
    expect(cpu.x == 0);
    resume cpu_frame;
    expect(cpu.pc == 3);
    expect(cpu.x == 0xC0);

    // INX (2 cycles)
    resume cpu_frame;
    resume cpu_frame;
    expect(cpu.pc == 4);
    expect(cpu.x == 0xC1);

    // BRK (7 cycles)
    resume cpu_frame;
    resume cpu_frame;
    resume cpu_frame;
    resume cpu_frame;
    resume cpu_frame;
    resume cpu_frame;
    resume cpu_frame;

    expect(cpu.pc == 5);
}

test "test_inx_overflow" {
    var basic_progam = [_]u8{0xE8, 0xE8, 0x00};    
    var cpu =  NesCpu.init(&basic_progam);
    cpu.x = 0xFF;
    cpu.interpret();
    expect(cpu.x == 1);
}
