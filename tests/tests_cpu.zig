const std = @import("std");
const expect = std.testing.expect;
const cpu_import = @import("cpu");
const NesCpu = cpu_import.NesCpu;

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

fn test_sta(y: u8, x: u8, address_expected: u16, program_1 : u8, program_2 : u8, program_3 : u8, cycle_expected: u8) void {
    var cpu =  NesCpu.init();
    var program = [_]u8{program_1, program_2, program_3};
    cpu.load(&program);
    cpu.reset();
    cpu.memory[0x0020] = 0x15;
    cpu.memory[0x0021] = 0x00;
    cpu.memory[0x0200] = 0x15;
    cpu.memory[0x0201] = 0x00;
    cpu.a = 0x0A;
    cpu.x = x;
    cpu.y = y;
    cpu.interpret();
    expect(cpu.memory[address_expected] == 0x0A);
    expect(cpu.internal.cycle_count == cycle_expected);
}

test "sta" {
    test_sta(0, 0, 0x15,    0x85, 0x15, 0x00, 3 + 7);
    test_sta(0, 5, 0x15,    0x95, 0x10, 0x00, 4 + 7);
    test_sta(0, 0, 0x1020,  0x8D, 0x20, 0x10, 4);
    test_sta(0, 5, 0x1025,  0x9D, 0x20, 0x10, 5);
    test_sta(5, 0, 0x1025,  0x99, 0x20, 0x10, 5);
    test_sta(0, 5, 0x15,    0x81, (0x20-5), 0x00, 6+7);
    test_sta(5, 0, 0x1A, 0x91, 0x20, 0x00, 6 + 7);
}
