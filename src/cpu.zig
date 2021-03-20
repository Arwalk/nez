const std = @import("std");
const expect = std.testing.expect;

const PRegister = struct {
    carry: bool,
    zero: bool,
    interrupt_disable: bool,
    decimal_mode: bool,
    break_cmd: bool,
    unused: bool,
    overflow: bool,
    negative: bool,

    fn init() PRegister {
        return PRegister {
            .carry = false,
            .zero = false,
            .interrupt_disable = false,
            .decimal_mode = false,
            .break_cmd = false,
            .unused = false,
            .overflow = false,
            .negative = false,
        };
    }

    fn get_raw_value(self: PRegister) u8 {
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
};

const NesCpu = struct {
    a: u8,
    x: u8,
    y: u8,
    sp: u8,
    p: PRegister,
    pc: u16
};

test "get raw value PRegister" {
    var p = PRegister.init();

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