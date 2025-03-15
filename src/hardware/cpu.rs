use super::cartridge::Cartridge;
use super::instructions::{Instruction, InstructionCB};
use super::memory::Memory;
use super::registers::{flags, Registers};
use log::{debug, error};

pub struct Cpu {
    reg: Registers,
    mem: Memory,
    ime: bool,
    ime_next: bool,
}

impl Cpu {
    pub fn new() -> Self {
        let mut ret = Cpu {
            reg: Registers::default(),
            mem: Memory::new(),
            ime: false,
            ime_next: false,
        };
        ret.reg.set_pc(0x100);
        ret
    }
    pub fn new_with_cart(path: &str) -> Self {
        let cart = Cartridge::new(path);
        // let cart = Cartridge::new("./roms/tloz.gb");
        let mut ret = Cpu {
            reg: Registers::default(),
            mem: Memory::new_with_cart(cart),
            ime: false,
            ime_next: false,
        };
        ret.reg.set_pc(0x100);
        // for i in 0x100..=0x14F {
        //     debug!("adress {:#06X}", i);
        //     debug!("byte {:#010b} aka {:#04X}", ret.mem[i], ret.mem[i]);
        // }
        ret
    }
    fn pc(&self) -> u16 {
        self.reg.pc()
    }
    fn sp(&self) -> u16 {
        self.reg.sp()
    }
    fn inc_pc(&mut self) {
        self.reg.set_pc(self.pc() + 1);
    }
    fn dec_pc(&mut self) {
        self.reg.set_pc(self.pc() - 1);
    }
    fn inc_sp(&mut self) {
        self.reg.set_sp(self.sp() + 1);
    }
    fn dec_sp(&mut self) {
        self.reg.set_sp(self.sp() - 1);
    }
    fn fetch_byte(&self) -> u8 {
        self.mem[self.pc() as usize]
    }
    fn imm8_unsigned(&mut self) -> u8 {
        self.inc_pc();
        self.fetch_byte()
    }
    fn imm16_unsigned(&mut self) -> u16 {
        self.inc_pc();
        let lsb = self.fetch_byte() as u16;
        self.inc_pc();
        let msb = self.fetch_byte() as u16;
        debug!("Got {:#06X}", msb << 8 | lsb);
        msb << 8 | lsb
    }
    fn imm8_signed(&mut self) -> i8 {
        self.inc_pc();
        let byte = self.fetch_byte();
        // neg
        // -6 is 1010
        // to get 6: -1 -> 1001, then not -> 0110
        Self::i8_from_u8(byte)
    }
    fn pop_stack(&mut self) -> u16 {
        let lsb = self.mem[self.sp() as usize];
        self.inc_sp();
        let msb = self.mem[self.sp() as usize];
        self.inc_sp();
        (msb as u16) << 8 | (lsb as u16)
    }
    fn resolve_opcode(&self, byte: u8) -> Instruction {
        let opcode = byte.try_into();
        match opcode {
            Ok(code) => code,
            Err(e) => {
                panic!("{:?}", e);
                // panic!("Unrecognized code: {:#010b}", byte);
            }
        }
    }
    fn resolve_cb_opcode(&mut self, byte: u8) -> InstructionCB {
        // let opcode = InstructionCB::from_byte(byte);
        let opcode = byte.try_into();
        match opcode {
            Ok(code) => code,
            Err(e) => {
                panic!("{:?}", e);
            }
        }
    }
    fn execute_cb_opcode(&mut self, instruction: InstructionCB) {
        use InstructionCB::*;
        match instruction {
            RLCR8 { operand } => self.rlc_r8(operand),
            RRCR8 { operand } => self.rrc_r8(operand),
            RLR8 { operand } => self.rl_r8(operand),
            RRR8 { operand } => self.rr_r8(operand),
            SLAR8 { operand } => self.sla_r8(operand),
            SRAR8 { operand } => self.sra_r8(operand),
            SwapR8 { operand } => self.swap_r8(operand),
            SRLR8 { operand } => self.srl_r8(operand),
            _ => {
                println!("Not implemented: {:?}", instruction);
            }
        }
    }
    fn execute_opcode(&mut self, instruction: Instruction) {
        use Instruction::*;
        match instruction {
            // block 0
            Nop => self.nop(),
            LdR16Imm16 { dest } => self.ld_r16_imm16(dest),
            LdR16MemA { dest } => self.ld_r16mem_a(dest),
            LdAR16Mem { source } => self.ld_a_r16mem(source),
            LdImm16SP => self.ld_imm16_sp(),
            IncR16 { operand } => self.inc_r16(operand),
            DecR16 { operand } => self.dec_r16(operand),
            AddHLR16 { operand } => self.add_hl_r16(operand),
            IncR8 { operand } => self.inc_r8(operand),
            DecR8 { operand } => self.dec_r8(operand),
            LdR8Imm8 { dest } => self.ld_r8_imm8(dest),
            RLCA => self.rlca(),
            RRCA => self.rrca(),
            RLA => self.rla(),
            RRA => self.rra(),
            DAA => self.daa(),
            CPL => self.cpl(),
            SCF => self.scf(),
            CCF => self.ccf(),
            JRImm8 => self.jr_imm8(),
            JRCondImm8 { cond } => self.jr_cond_imm8(cond),
            Stop => self.stop(),

            // block 1
            Halt => self.halt(),
            LdR8R8 { dest, source } => self.ld_r8_r8(dest, source),

            // block 2
            AddAR8 { operand } => self.add_a_r8(operand),
            AdcAR8 { operand } => self.adc_a_r8(operand),
            SubAR8 { operand } => self.sub_a_r8(operand),
            SbcAR8 { operand } => self.sbc_a_r8(operand),
            AndAR8 { operand } => self.and_a_r8(operand),
            XorAR8 { operand } => self.xor_a_r8(operand),
            OrAR8 { operand } => self.or_a_r8(operand),
            CpAR8 { operand } => self.cp_a_r8(operand),

            // block 3
            AddAImm8 => self.add_a_imm8(),
            AdcAImm8 => self.adc_a_imm8(),
            SubAImm8 => self.sub_a_imm8(),
            SbcAImm8 => self.sbc_a_imm8(),
            AndAImm8 => self.and_a_imm8(),
            XorAImm8 => self.xor_a_imm8(),
            OrAImm8 => self.or_a_imm8(),
            CpAImm8 => self.cp_a_imm8(),
            RetCond { cond } => self.ret_cond(cond),
            Ret => self.ret(),
            RetI => self.ret_i(),
            JpCondImm16 { cond } => self.jp_cond_imm16(cond),
            JpImm16 => self.jp_imm16(),
            JpHL => self.jp_hl(),
            CallCondImm16 { cond } => self.call_cond_imm16(cond),
            CallImm16 => self.call_imm16(),
            RstTgt3 { target } => self.rst_tgt3(target),
            PopR16Stk { reg } => self.pop_r16_stk(reg),
            PushR16Stk { reg } => self.push_r16_stk(reg),
            Prefix => self.prefix(),
            LdhCA => self.ldh_c_a(),
            LdhImm8A => self.ldh_imm8_a(),
            LdImm16A => self.ld_imm16_a(),
            LdhAC => self.ldh_a_c(),
            LdhAImm8 => self.ldh_a_imm8(),
            LdAImm16 => self.ld_a_imm16(),
            AddSPImm8 => self.add_sp_e8(),
            LdHLSPImm8 => self.ld_hl_sp_e8(),
            LdSPHL => self.ld_sp_hl(),
            DI => self.di(),
            EI => self.ei(),
            Data { byte } => error!("Instruction {:#010b} is invalid!", byte),
        };
    }
    pub fn step(&mut self) {
        let byte = self.fetch_byte();
        let opcode = self.resolve_opcode(byte);
        debug!(
            "{:#06X}: {:#010b} aka {:#04X} -> {:?}",
            self.pc(),
            byte,
            byte,
            opcode
        );
        self.execute_opcode(opcode);
        self.inc_pc();
    }
    pub fn step_cb(&mut self) {
        let byte = self.fetch_byte();
        let opcode = self.resolve_cb_opcode(byte);
        self.execute_cb_opcode(opcode);
        self.inc_pc();
    }

    fn preserve_flag(curr_flags: u8, preserve_flag: u8) -> u8 {
        curr_flags & preserve_flag
    }
    // fn add_carry_flag_u8(vals: Vec<u8>, bit: u8) -> bool {
    //     // overflow from bit 3: want masks to be 0b_0001_0000 and 0b_1111
    //     let carry_mask = 0x1 << (bit + 1);
    //     let addition_mask = 0xFF >> (8 - (bit + 1));
    //     let res: u16 = vals
    //         .into_iter()
    //         .map(|val| (val & addition_mask) as u16)
    //         .sum();
    //     res & carry_mask == carry_mask
    // }
    // fn add_carry_flag_u16(vals: Vec<u16>, bit: u8) -> bool {
    //     let carry_mask: u32 = 0x1 << (bit + 1);
    //     let addition_mask = 0xFFFF >> (16 - (bit + 1));
    //     let res: u32 = vals
    //         .into_iter()
    //         .map(|val| (val & addition_mask) as u32)
    //         .sum();
    //     res & carry_mask == carry_mask
    // }
    fn half_carry_add_8(val: u8, add: u8) -> bool {
        (val & 0xF) + (add & 0xF) > 0xF
    }
    fn carry_add_8(val: u8, add: u8) -> bool {
        (val as u16) + (add as u16) > 0xFF
    }
    fn half_carry_adc_8(val: u8, add: u8, carry: bool) -> bool {
        (val & 0xF) + (add & 0xF) + carry as u8 > 0xF
    }
    fn carry_adc_8(val: u8, add: u8, carry: bool) -> bool {
        (val as u16) + (add as u16) + carry as u16 > 0xFF
    }
    fn carry_add_16(val: u16, add: u16) -> bool {
        (val as u32) + (add as u32) > 0xFFFF
    }
    fn half_carry_add_16(val: u16, add: u16) -> bool {
        (val & 0xFFF) + (add & 0xFFF) > 0xFFF
    }
    fn half_carry_add_sp(val: u16, add: i8) -> bool {
        ((val & 0xF) + ((add as u16) & 0xF)) > 0xF
    }
    fn carry_add_sp(val: u16, add: i8) -> bool {
        let add_u16 = add as u16;
        let adj = if add_u16 > 0x7F {
            add_u16 | 0xFF00
        } else {
            add_u16
        };
        (val & 0xFF) + adj > 0xFF
    }
    fn half_carry_sub_8(val: u8, sub: u8) -> bool {
        (val & 0xF) < (sub & 0xF)
    }
    fn carry_sub_8(val: u8, sub: u8) -> bool {
        val < sub
    }
    fn half_carry_sbc_8(val: u8, sub: u8, carry: bool) -> bool {
        (val & 0xF) < (sub & 0xF) + (carry as u8)
    }
    fn carry_sbc_8(val: u8, sub: u8, carry: bool) -> bool {
        val < sub + (carry as u8)
    }
    fn half_carry_sub_16(val: u16, sub: u16) -> bool {
        (val & 0xFFF) < (sub & 0xFFF)
    }
    fn carry_sub_16(val: u16, sub: u16) -> bool {
        val < sub
    }
    // fn sub_carry_flag_u8(mut left: u8, right: Vec<u8>, bit: u8) -> bool {
    //     let mask = 0xFF >> (8 - bit);
    //     for i in right.iter() {
    //         if i & mask > left & mask {
    //             return true;
    //         }
    //         left -= i;
    //     }
    //
    //     false
    // }
    fn i8_from_u8(byte: u8) -> i8 {
        if byte & 0x80 == 0x80 {
            let mut ret = byte;
            ret -= 1;
            ret = !ret;
            (ret as i8) * -1
        }
        // pos
        else {
            byte as i8
        }
    }
    fn get_r8(&self, operand: u8) -> u8 {
        use super::registers::RegResult;
        let r8 = self.reg.get_r8(operand);
        let r8_val: Result<u8, _> = match r8 {
            Ok(RegResult::Defer) => Ok(self.mem[self.reg.hl() as usize]),
            Ok(RegResult::ReturnU8 { val }) => Ok(val),
            Ok(_) => unreachable!("Unexpected RegResult variant"),
            Err(e) => Err(e),
        };

        r8_val.expect("Invalid r8 register was passed in")
    }
    fn resolve_condition(&self, cond: u8) -> Result<bool, String> {
        let flags = self.reg.f();
        // 0b_ZNHC_0000
        match cond {
            0b_00 => Ok(flags & flags::Z == 0), // NZ
            0b_01 => Ok(flags & flags::Z != 0), // Z
            0b_10 => Ok(flags & flags::C == 0), // NC
            0b_11 => Ok(flags & flags::C != 0), // C
            _ => Err(format!("Invalid conditional: {:#04b}", cond)),
        }
    }
}

// block 0 of instructions
impl Cpu {
    fn nop(&self) {}

    fn ld_r16_imm16(&mut self, dest: u8) {
        let val = self.imm16_unsigned();
        self.reg.set_r16(val, dest);
    }

    fn ld_r16mem_a(&mut self, dest: u8) {
        let location = self.reg.get_r16mem(dest).expect("Invalid r16mem register") as usize;
        self.mem[location] = self.reg.a();
    }

    fn ld_a_r16mem(&mut self, source: u8) {
        let location = self
            .reg
            .get_r16mem(source)
            .expect("Invalid r16mem register") as usize;
        println!("location: {}", location);
        self.reg.set_a(self.mem[location]);
    }

    // Copy SP & $FF at address n16 and SP >> 8 at address n16 + 1.
    fn ld_imm16_sp(&mut self) {
        let sp = self.reg.sp();

        let location = self.imm16_unsigned() as usize;

        self.mem[location] = (sp & 0xFF) as u8;
        self.mem[location + 1] = (sp >> 8) as u8;
    }

    fn inc_r16(&mut self, operand: u8) {
        let r16_val = self.reg.get_r16(operand).expect("Invalid r16 register");
        self.reg.set_r16(r16_val.wrapping_add(1), operand);
    }

    fn dec_r16(&mut self, operand: u8) {
        let r16_val = self.reg.get_r16(operand).expect("Invalid r16 register");
        self.reg.set_r16(r16_val.wrapping_sub(1), operand);
    }

    fn add_hl_r16(&mut self, operand: u8) {
        let hl_val = self.reg.hl();
        let r16_val = self.reg.get_r16(operand).expect("Invalid r16 register");
        let res = hl_val.wrapping_add(r16_val);
        self.reg.set_hl(res);

        let mut flags = 0x00;
        flags |= Self::preserve_flag(self.reg.f(), flags::Z);
        // N should be 0
        if Self::half_carry_add_16(hl_val, r16_val) {
            flags |= flags::H;
        }
        if Self::carry_add_16(hl_val, r16_val) {
            flags |= flags::C;
        }

        self.reg.set_f(flags);
    }

    fn inc_r8(&mut self, operand: u8) {
        let val = self.get_r8(operand);

        let res = val.wrapping_add(1);
        self.reg.set_r8(res, operand);

        let mut flags = 0x00;
        if res == 0 {
            flags |= flags::Z;
        }
        // N should be 0
        if Self::half_carry_add_8(val, 1) {
            flags |= flags::H;
        }
        flags |= Self::preserve_flag(self.reg.f(), flags::C);
        self.reg.set_f(flags);
    }
    fn dec_r8(&mut self, operand: u8) {
        let val = self.get_r8(operand);
        let res = val.wrapping_sub(1);
        self.reg.set_r8(res, operand);

        let mut flags = 0x00;
        if res == 0 {
            flags |= flags::Z;
        }
        flags |= flags::N;
        if Self::half_carry_sub_8(val, 1) {
            flags |= flags::H;
        }
        flags |= Self::preserve_flag(self.reg.f(), flags::C);
        self.reg.set_f(flags);
    }
    fn ld_r8_imm8(&mut self, dest: u8) {
        self.inc_pc();
        let byte = self.imm8_unsigned();
        self.reg.set_r8(byte, dest);
    }
    fn rlca(&mut self) {
        let val = self.reg.a().rotate_left(1);
        self.reg.set_a(val);

        let mut flags = 0x00;
        // Z, N, and H are 0
        if val & 1 == 1 {
            flags |= flags::C;
        }
        self.reg.set_f(flags);
    }
    fn rrca(&mut self) {
        let val = self.reg.a().rotate_right(1);
        self.reg.set_a(val);

        let mut flags = 0x00;
        // Z, N, and H are 0
        if val & 0x80 == 0x80 {
            flags |= flags::C;
        }
        self.reg.set_f(flags);
    }
    fn rla(&mut self) {
        let c_flag: bool = self.reg.f() & flags::C == flags::C;
        let set_c: bool = self.reg.a() & 0x80 == 0x80;

        let mut new_a = self.reg.a() << 1;
        if c_flag {
            new_a |= 0x01;
        }
        self.reg.set_a(new_a);

        let mut flags = 0x80;
        // Z, N, and H are 0
        if set_c {
            flags |= flags::C;
        }
        self.reg.set_f(flags);
    }

    fn rra(&mut self) {
        let c_flag: bool = self.reg.f() & flags::C == flags::C;
        let set_c: bool = self.reg.a() & 0x01 == 0x01;

        let mut new_a = self.reg.a() >> 1;
        if c_flag {
            new_a |= 0x80
        }
        self.reg.set_a(new_a);

        let mut flags = 0x80;
        // Z, N, and H are 0
        if set_c {
            flags |= flags::C;
        }
        self.reg.set_f(flags);
    }
    fn daa(&mut self) {
        let mut adjustment = 0;
        let read_flags = self.reg.f();
        let mut flags = 0x00;
        let result;

        if read_flags & flags::N == flags::N {
            if read_flags & flags::H == flags::H {
                adjustment += 0x6;
            }
            if read_flags & flags::C == flags::C {
                adjustment += 0x60;
            }
            result = self.reg.a() - adjustment;
        } else {
            if read_flags & flags::H == flags::H || self.reg.a() & 0xF > 0x9 {
                adjustment += 0x6;
            }
            if read_flags & flags::C == flags::C || self.reg.a() > 0x99 {
                adjustment += 0x60;
                flags |= flags::C;
            }
            result = self.reg.a() + adjustment;
        }
        self.reg.set_a(result);

        if result == 0 {
            flags |= flags::Z;
        }
        flags |= Self::preserve_flag(flags, flags::N);
        // H is 0
        // C is set above
        self.reg.set_f(flags);
    }
    fn cpl(&mut self) {
        self.reg.set_a(!self.reg.a());

        let mut flags = 0x00;
        flags |= Self::preserve_flag(flags, flags::Z);
        flags |= flags::N;
        flags |= flags::H;
        flags |= Self::preserve_flag(flags, flags::C);
        self.reg.set_f(flags);
    }
    fn scf(&mut self) {
        let mut flags = 0x00;
        flags |= Self::preserve_flag(flags, flags::Z);
        // N, H is 0
        flags |= flags::C;
        self.reg.set_f(flags);
    }
    fn ccf(&mut self) {
        let mut flags = 0x00;
        flags |= Self::preserve_flag(flags, flags::Z);
        // N, H is 0
        if self.reg.f() & flags::C == 0x00 {
            flags |= flags::C;
        }
        self.reg.set_f(flags);
    }
    fn jr_imm8(&mut self) {
        let relative_value = self.imm8_unsigned();
        self.reg.set_pc(self.pc() + relative_value as u16);
    }
    fn jr_cond_imm8(&mut self, cond: u8) {
        let relative_value = self.imm8_unsigned();
        debug!("Read byte {:04X}, or {}", relative_value, relative_value);
        if self.resolve_condition(cond).unwrap() {
            self.reg.set_pc(self.pc() + relative_value as u16);
        }
    }
    fn stop(&mut self) {
        debug!("{:?}", self.fetch_byte());
        todo!()
    }
}
impl Cpu {
    // block 1
    fn halt(&mut self) {
        debug!("{:?}", self.fetch_byte());
        todo!()
    }
    fn ld_r8_r8(&mut self, dest: u8, source: u8) {
        let val = self.get_r8(source);

        self.reg.set_r8(val, dest);
    }
}
impl Cpu {
    // block 2
    fn add_a_r8(&mut self, operand: u8) {
        let val = self.get_r8(operand);
        let result = self.reg.a() + val;
        self.reg.set_a(result);

        let mut flags = 0x00;
        if result == 0 {
            flags |= flags::Z;
        }
        // N is 0
        if Self::half_carry_add_8(self.reg.a(), val) {
            flags |= flags::H;
        }
        if Self::carry_add_8(self.reg.a(), val) {
            flags |= flags::C;
        }
        self.reg.set_f(flags);
    }
    fn adc_a_r8(&mut self, operand: u8) {
        let val = self.get_r8(operand);
        let a_val = self.reg.a();
        let carry = self.reg.f() & flags::C == flags::C;

        let result = a_val + val + carry as u8;
        self.reg.set_a(result);

        let mut flags = 0x00;
        if result == 0 {
            flags |= flags::Z;
        }
        // N is 0
        if Self::half_carry_adc_8(a_val, val, carry) {
            flags |= flags::H;
        }
        if Self::carry_adc_8(a_val, val, carry) {
            flags |= flags::C;
        }
        self.reg.set_f(flags);
    }
    fn sub_a_r8(&mut self, operand: u8) {
        let val = self.get_r8(operand);
        let a_val = self.reg.a();
        let result = a_val - val;

        self.reg.set_a(result);

        let mut flags = 0x00;
        if result == 0 {
            flags |= flags::Z;
        }
        flags |= flags::N;
        if Self::half_carry_sub_8(a_val, val) {
            flags |= flags::H;
        }
        if Self::carry_sub_8(a_val, val) {
            flags |= flags::C;
        }
        self.reg.set_f(flags);
    }
    fn sbc_a_r8(&mut self, operand: u8) {
        let val = self.get_r8(operand);
        let a_val = self.reg.a();
        let carry = self.reg.f() & flags::C == flags::C;

        let result = a_val - val - carry as u8;

        self.reg.set_a(result);

        let mut flags = 0x00;
        if result == 0 {
            flags |= flags::Z;
        }
        flags |= flags::N;
        if Self::half_carry_sbc_8(a_val, val, carry) {
            flags |= flags::H;
        }
        if Self::carry_sbc_8(a_val, val, carry) {
            flags |= flags::C;
        }
        self.reg.set_f(flags);
    }
    fn and_a_r8(&mut self, operand: u8) {
        let val = self.get_r8(operand);
        let a_val = self.reg.a();
        let result = val & a_val;

        self.reg.set_a(result);

        let mut flags = 0x00;
        if result == 0 {
            flags |= flags::Z;
        }
        // N is 0
        flags |= flags::H;
        // C is 0
        self.reg.set_f(flags);
    }
    fn xor_a_r8(&mut self, operand: u8) {
        let val = self.get_r8(operand);
        let a_val = self.reg.a();
        // debug!("Before: {:?}", self.reg);
        let result = val ^ a_val;

        self.reg.set_a(result);

        let mut flags = 0x00;
        if result == 0 {
            flags |= flags::Z;
        }
        // N, H, and C is 0
        self.reg.set_f(flags);
        // debug!("After: {:?}", self.reg);
    }
    fn or_a_r8(&mut self, operand: u8) {
        let val = self.get_r8(operand);
        let a_val = self.reg.a();
        let result = val | a_val;

        self.reg.set_a(result);

        let mut flags = 0x00;
        if result == 0 {
            flags |= flags::Z;
        }
        // N, H, and C is 0
        self.reg.set_f(flags);
    }
    //CP A, r8 discards the result of the operation, just setting flags based on the operation
    fn cp_a_r8(&mut self, operand: u8) {
        let val = self.get_r8(operand);
        let a_val = self.reg.a();
        let result = a_val - val;

        let mut flags = 0x00;
        if result == 0 {
            flags |= flags::Z;
        }
        flags |= flags::N;
        if Self::half_carry_sub_8(a_val, val) {
            flags |= flags::H;
        }
        if Self::carry_sub_8(a_val, val) {
            flags |= flags::C;
        }
        self.reg.set_f(flags);
    }
}
impl Cpu {
    // block 3
    fn add_a_imm8(&mut self) {
        let a_val = self.reg.a();

        self.inc_pc();
        let val = self.fetch_byte();

        let result = val + a_val;

        let mut flags = 0x00;
        if result == 0 {
            flags |= flags::Z;
        }
        // N is 0
        if Self::half_carry_add_8(self.reg.a(), val) {
            flags |= flags::H;
        }
        if Self::carry_add_8(self.reg.a(), val) {
            flags |= flags::C;
        }
        self.reg.set_f(flags);
    }
    fn adc_a_imm8(&mut self) {
        let a_val = self.reg.a();

        self.inc_pc();
        let val = self.fetch_byte();
        let carry = self.reg.f() & flags::C == flags::C;

        let result = val + a_val + carry as u8;

        let mut flags = 0x00;
        if result == 0 {
            flags |= flags::Z;
        }
        // N is 0
        if Self::half_carry_adc_8(self.reg.a(), val, carry) {
            flags |= flags::H;
        }
        if Self::carry_adc_8(self.reg.a(), val, carry) {
            flags |= flags::C;
        }
        self.reg.set_f(flags);
    }
    fn sub_a_imm8(&mut self) {
        let a_val = self.reg.a();

        self.inc_pc();
        let val = self.fetch_byte();

        let result = a_val - val;

        let mut flags = 0x00;
        if result == 0 {
            flags |= flags::Z;
        }
        flags |= flags::N;
        if Self::half_carry_sub_8(a_val, val) {
            flags |= flags::H;
        }
        if val > a_val {
            flags |= flags::C;
        }
        self.reg.set_f(flags);
    }
    fn sbc_a_imm8(&mut self) {
        let a_val = self.reg.a();
        let val = self.imm8_unsigned();

        let carry = self.reg.f() & flags::C == flags::C;

        let result = a_val - val - carry as u8;
        self.reg.set_a(result);

        let mut flags = 0x00;
        if result == 0 {
            flags |= flags::Z;
        }
        flags |= flags::N;
        if Self::half_carry_sbc_8(a_val, val, carry) {
            flags |= flags::H;
        }
        if val + carry as u8 > a_val {
            flags |= flags::C;
        }
        self.reg.set_f(flags);
    }
    fn and_a_imm8(&mut self) {
        let a_val = self.reg.a();
        let val = self.imm8_unsigned();

        let result = val & a_val;

        self.reg.set_a(result);

        let mut flags = 0x00;
        if result == 0 {
            flags |= flags::Z;
        }
        // N is 0
        flags |= flags::H;
        // C is 0
        self.reg.set_f(flags);
    }
    fn xor_a_imm8(&mut self) {
        let a_val = self.reg.a();
        let val = self.imm8_unsigned();

        let result = val ^ a_val;

        let mut flags = 0x00;
        if result == 0 {
            flags |= flags::Z;
        }
        // N, H, and C is 0
        self.reg.set_f(flags);
    }
    fn or_a_imm8(&mut self) {
        let a_val = self.reg.a();
        let val = self.imm8_unsigned();

        let result = val | a_val;

        let mut flags = 0x00;
        if result == 0 {
            flags |= flags::Z;
        }
        // N, H, and C is 0
        self.reg.set_f(flags);
    }
    //CP A, r8 discards the result of the operation, just setting flags based on the operation
    fn cp_a_imm8(&mut self) {
        let a_val = self.reg.a();
        let val = self.imm8_unsigned();
        let result = a_val.wrapping_sub(val);

        let mut flags = 0x00;
        if result == 0 {
            flags |= flags::Z;
        }
        flags |= flags::N;
        if Self::half_carry_sub_8(a_val, val) {
            flags |= flags::H;
        }
        if val > a_val {
            flags |= flags::C;
        }
        self.reg.set_f(flags);
    }
    fn ret_cond(&mut self, cond: u8) {
        if self.resolve_condition(cond).unwrap() {
            let new_pc = self.pop_stack();
            self.reg.set_pc(new_pc);
        }
    }
    fn ret(&mut self) {
        let new_pc = self.pop_stack();
        self.reg.set_pc(new_pc);
    }
    fn ret_i(&mut self) {
        self.ei();
        self.ret();
    }
    fn jp_cond_imm16(&mut self, cond: u8) {
        if self.resolve_condition(cond).unwrap() {
            self.jp_imm16();
        }
    }
    fn jp_imm16(&mut self) {
        let dest_addr = self.imm16_unsigned();
        debug!("Jumping to {:#018b} aka {:#06X}", dest_addr, dest_addr);
        self.reg.set_pc(dest_addr);
        self.dec_pc();
    }
    fn jp_hl(&mut self) {
        let hl = self.reg.hl();
        self.reg.set_pc(hl);
        self.dec_pc();
    }
    fn call_cond_imm16(&mut self, cond: u8) {
        let call_addr = self.imm16_unsigned();
        if self.resolve_condition(cond).unwrap() {
            self.dec_sp();
            let sp = self.sp() as usize;
            self.mem[sp] = (self.pc() >> 8) as u8;

            self.dec_sp();
            let sp = self.sp() as usize;
            self.mem[sp] = (self.pc() & 0xFF) as u8;
            self.reg.set_pc(call_addr);
        }
    }
    fn call_imm16(&mut self) {
        let call_addr = self.imm16_unsigned();

        self.dec_sp();
        let sp = self.sp() as usize;
        self.mem[sp] = (self.pc() >> 8) as u8;

        self.dec_sp();
        let sp = self.sp() as usize;
        self.mem[sp] = (self.pc() & 0xFF) as u8;

        self.reg.set_pc(call_addr);
    }
    fn rst_tgt3(&mut self, target: u8) {
        let call_addr = (target as u16) << 8;
        if self.reg.f() & flags::Z == flags::Z {
            self.dec_sp();
            let sp = self.sp() as usize;
            self.mem[sp] = (self.pc() >> 8) as u8;

            self.dec_sp();
            let sp = self.sp() as usize;
            self.mem[sp] = (self.pc() & 0xFF) as u8;
        }

        self.reg.set_pc(call_addr);
    }
    fn pop_r16_stk(&mut self, reg: u8) {
        let val: u16 = self.pop_stack();
        self.reg.set_r16stk(val, reg);
    }
    fn push_r16_stk(&mut self, reg: u8) {
        let val: u16 = self.reg.get_r16stk(reg).expect("Invalid r16 register");

        self.dec_sp();
        let sp = self.sp() as usize;
        self.mem[sp] = (val >> 8) as u8;

        self.dec_sp();
        let sp = self.sp() as usize;
        self.mem[sp] = (val & 0xFF) as u8;
    }
    fn prefix(&mut self) {
        self.step_cb();
    }
    fn ldh_c_a(&mut self) {
        let addr = 0xFF00 + self.reg.c() as usize;
        self.mem[addr] = self.reg.a();
    }
    fn ldh_imm8_a(&mut self) {
        let addr = 0xFF00 + self.imm8_unsigned() as usize;
        self.mem[addr] = self.reg.a();
    }
    fn ld_imm16_a(&mut self) {
        let addr = self.imm16_unsigned() as usize;
        self.reg.set_a(self.mem[addr]);
    }
    fn ldh_a_c(&mut self) {
        let addr = 0xFF00 + self.reg.c() as usize;
        self.reg.set_a(self.mem[addr]);
    }
    fn ldh_a_imm8(&mut self) {
        let addr = 0xFF00 + self.imm8_unsigned() as usize;
        self.reg.set_a(self.mem[addr]);
    }
    fn ld_a_imm16(&mut self) {
        let addr = self.imm16_unsigned() as usize;
        self.reg.set_a(self.mem[addr]);
    }
    fn add_sp_e8(&mut self) {
        let val = self.imm8_signed();
        let sp = self.sp();

        let result = (sp as i16 + val as i16) as u16;
        self.reg.set_sp(result);

        let mut flags = 0x00;
        // Z and N are 0
        if Self::half_carry_add_sp(sp, val) {
            flags |= flags::H;
        }
        if Self::carry_add_sp(sp, val) {
            flags |= flags::C;
        }
        self.reg.set_f(flags);
    }
    fn ld_hl_sp_e8(&mut self) {
        let val = self.imm8_signed();
        let sp = self.sp();

        let result = (sp as i16 + val as i16) as u16;
        self.reg.set_hl(result);

        let mut flags = 0x00;
        // Z and N are 0
        if Self::half_carry_add_sp(sp, val) {
            flags |= flags::H;
        }
        if Self::carry_add_sp(sp, val) {
            flags |= flags::C;
        }
        self.reg.set_f(flags);
    }
    fn ld_sp_hl(&mut self) {
        let val = self.reg.hl();
        self.reg.set_sp(val);
    }
    fn di(&mut self) {
        debug!("{:?}", self.fetch_byte());
        self.ime = false;
    }
    fn ei(&mut self) {
        debug!("{:?}", self.fetch_byte());
        todo!()
    }
}

impl Cpu {
    fn rlc_r8(&mut self, operand: u8) {
        let val = self.get_r8(operand).rotate_left(1);

        let mut flags = 0x00;
        if val == 0 {
            flags |= flags::Z;
        }
        // N and H are 0
        if val & 0x01 == 0x01 {
            flags |= flags::C;
        }
        self.reg.set_f(flags);
    }
    fn rrc_r8(&mut self, operand: u8) {
        let val = self.get_r8(operand).rotate_right(1);

        let mut flags = 0x00;
        if val == 0 {
            flags |= flags::Z;
        }
        // N and H are 0
        if val & 0x80 == 0x80 {
            flags |= flags::C;
        }
        self.reg.set_f(flags);
    }
    fn rl_r8(&mut self, operand: u8) {
        let c_flag: bool = self.reg.f() & flags::C == flags::C;
        let set_c: bool = self.get_r8(operand) & 0x80 == 0x80;

        let mut result = self.get_r8(operand) << 1;
        if c_flag {
            result |= 0x01;
        }
        self.reg.set_r8(result, operand);

        let mut flags = 0x80;
        if result == 0 {
            flags |= flags::Z;
        }
        // N and H are 0
        if set_c {
            flags |= flags::C;
        }
        self.reg.set_f(flags);
    }
    fn rr_r8(&mut self, operand: u8) {
        let c_flag: bool = self.reg.f() & flags::C == flags::C;
        let set_c: bool = self.get_r8(operand) & 0x01 == 0x01;

        let mut result = self.get_r8(operand) >> 1;
        if c_flag {
            result |= 0x80;
        }
        self.reg.set_r8(result, operand);

        let mut flags = 0x80;
        if result == 0 {
            flags |= flags::Z;
        }
        // N and H are 0
        if set_c {
            flags |= flags::C;
        }
        self.reg.set_f(flags);
    }
    fn sla_r8(&mut self, operand: u8) {
        let set_c: bool = self.get_r8(operand) & 0x80 == 0x80;

        let result = self.get_r8(operand) << 1;
        self.reg.set_r8(result, operand);

        let mut flags = 0x00;
        if result == 0 {
            flags |= flags::Z;
        }
        // N and H are 0
        if set_c {
            flags |= flags::C;
        }
        self.reg.set_f(flags);
    }
    fn sra_r8(&mut self, operand: u8) {
        let set_c: bool = self.get_r8(operand) & 0x01 == 0x01;

        let result = self.get_r8(operand) >> 1;
        self.reg.set_r8(result, operand);

        let mut flags = 0x00;
        if result == 0 {
            flags |= flags::Z;
        }
        // N and H are 0
        if set_c {
            flags |= flags::C;
        }
        self.reg.set_f(flags);
    }
    fn swap_r8(&mut self, operand: u8) {
        let val = self.get_r8(operand);
        let low = val & 0x0F;
        let high = val & 0xF0;

        let result = (low << 4) | (high >> 4);
        self.reg.set_r8(result, operand);

        let mut flags = 0x00;
        if result == 0 {
            flags |= flags::Z;
        }
        // N, H, and C are 0
        self.reg.set_f(flags);
    }
    fn srl_r8(&mut self, operand: u8) {
        let set_c = self.get_r8(operand) & 0x01 == 0x01;

        let result = self.get_r8(operand) >> 1;
        self.reg.set_r8(result, operand);

        let mut flags = 0x00;
        if result == 0 {
            flags |= flags::Z;
        }
        // N and H are 0
        if set_c {
            flags |= flags::C;
        }
        self.reg.set_f(flags);
    }
}

#[cfg(test)]
mod test {
    use super::Cpu;

    mod general {
        use super::Cpu;
        // xor value with mask, then or it with other 1-mask, then check if 0
        #[test]
        fn read_first_instruction() {
            let cpu = Cpu::new_with_cart("./roms/tetris.gb");
            assert_eq!(cpu.mem[0x100], 0b00000000);
            assert_eq!(cpu.mem[0x101], 0b11000011);
        }

        // Deprecated
        // #[test]
        // fn add_carry_flag_u8_carry() {
        //     // fn add_carry_flag_u8(vals: Vec<u8>, bit: u8) -> bool {
        //     let res = Cpu::add_carry_flag_u8(vec![0b_1111, 0b_0001], 3);
        //     assert!(res);
        //     let res = Cpu::add_carry_flag_u8(vec![0b_1101, 0b_0011], 3);
        //     assert!(res);
        //     let res = Cpu::add_carry_flag_u8(vec![0b_1111_1111, 0b_1111_0001], 3);
        //     assert!(res);
        //     let res = Cpu::add_carry_flag_u8(vec![0b_1100, 0b_0011, 0b_0001], 3);
        //     assert!(res);
        // }

        // Deprecated
        // #[test]
        // fn add_carry_flag_u8_no_carry() {
        //     // fn add_carry_flag_u8(vals: Vec<u8>, bit: u8) -> bool {
        //     let res = Cpu::add_carry_flag_u8(vec![0b_1111, 0b_0000], 3);
        //     assert!(!res);
        //     let res = Cpu::add_carry_flag_u8(vec![0b_1100, 0b_0010], 3);
        //     assert!(!res);
        //     let res = Cpu::add_carry_flag_u8(vec![0b_1111_0101, 0b_1111_1010], 3);
        //     assert!(!res);
        // }
    }

    mod block_0 {
        use super::super::Instruction;
        use super::super::Registers;
        use super::Cpu;

        const DEFAULT_FLAG: u8 = 0xC0;

        fn setup(instructions: Vec<Instruction>, flags: Option<u8>) -> Cpu {
            let mut cpu = Cpu::new();
            if let Some(flag_bit) = flags {
                cpu.reg.set_f(flag_bit)
            } else {
                cpu.reg.set_f(DEFAULT_FLAG);
            }
            for i in 0..instructions.len() {
                cpu.mem[0x100 + i] = u8::from(instructions[i]);
            }
            // cpu.mem[0x100] = u8::from(Instruction::Nop);
            cpu
        }

        #[test]
        fn test_nop() {
            let mut cpu = setup(vec![Instruction::Nop], None);
            cpu.step();

            let mut result_reg = Registers::default();
            result_reg.set_pc(0x101);
            result_reg.set_f(DEFAULT_FLAG);

            assert_eq!(cpu.pc(), 0x101);
            assert_eq!(cpu.reg, result_reg);
        }

        #[test]
        fn test_ld_r16_imm16() {
            let mut cpu = setup(
                vec![
                    Instruction::LdR16Imm16 { dest: 0 },
                    Instruction::Data { byte: 0b_0000_1111 },
                    Instruction::Data { byte: 0b_1010_1010 },
                    Instruction::LdR16Imm16 { dest: 1 },
                    Instruction::Data { byte: 0b_1111_0000 },
                    Instruction::Data { byte: 0b_0101_0101 },
                    Instruction::LdR16Imm16 { dest: 2 },
                    Instruction::Data { byte: 0b_1100_0011 },
                    Instruction::Data { byte: 0b_0011_1100 },
                    Instruction::LdR16Imm16 { dest: 3 },
                    Instruction::Data { byte: 0b_1110_0111 },
                    Instruction::Data { byte: 0b_1000_0001 },
                ],
                None,
            );

            cpu.step();
            let mut result_reg = Registers::default();
            result_reg.set_pc(0x103);
            result_reg.set_f(DEFAULT_FLAG);
            result_reg.set_bc(0b_1010_1010_0000_1111);
            assert_eq!(cpu.reg, result_reg);

            cpu.step();
            result_reg.set_pc(0x106);
            result_reg.set_de(0b_0101_0101_1111_0000);
            assert_eq!(cpu.reg, result_reg);

            cpu.step();
            result_reg.set_pc(0x109);
            result_reg.set_hl(0b_0011_1100_1100_0011);
            assert_eq!(cpu.reg, result_reg);

            cpu.step();
            result_reg.set_pc(0x10C);
            result_reg.set_sp(0b_1000_0001_1110_0111);
            assert_eq!(cpu.reg, result_reg);
        }

        #[test]
        fn test_ld_r16mem_a() {
            let mut cpu = setup(
                vec![
                    Instruction::LdR16MemA { dest: 0 }, // BC
                    Instruction::LdR16MemA { dest: 1 }, // DE
                    Instruction::LdR16MemA { dest: 2 }, // HL+
                    Instruction::LdR16MemA { dest: 3 }, // HL-
                    Instruction::LdR16MemA { dest: 3 }, // HL-
                    Instruction::LdR16MemA { dest: 3 }, // HL-
                    Instruction::LdR16MemA { dest: 2 }, // HL+
                ],
                Some(0xC0),
            );
            cpu.reg.set_bc(0x8000);
            cpu.reg.set_de(0x8100);
            cpu.reg.set_hl(0x8200);
            cpu.reg.set_a(0x12);

            cpu.step();
            assert_eq!(cpu.reg.pc(), 0x101);
            assert_eq!(cpu.reg.a(), 0x12);
            assert_eq!(cpu.reg.f(), 0xC0);
            assert_eq!(cpu.mem[cpu.reg.bc() as usize], 0x12);

            cpu.reg.set_a(0xFF);
            cpu.step();
            assert_eq!(cpu.reg.pc(), 0x102);
            assert_eq!(cpu.reg.a(), 0xFF);
            assert_eq!(cpu.mem[cpu.reg.de() as usize], 0xFF);

            cpu.reg.set_a(0x0F);
            let old_hl = cpu.reg.hl();
            cpu.step();
            assert_eq!(cpu.reg.pc(), 0x103);
            assert_eq!(cpu.reg.a(), 0x0F);
            assert_eq!(cpu.reg.hl(), 0x8201);
            assert_eq!(cpu.mem[old_hl as usize], 0x0F);

            cpu.reg.set_a(0xF0);
            let old_hl = cpu.reg.hl();
            cpu.step();
            assert_eq!(cpu.reg.pc(), 0x104);
            assert_eq!(cpu.reg.a(), 0xF0);
            assert_eq!(cpu.reg.hl(), 0x8200);
            assert_eq!(cpu.mem[old_hl as usize], 0xF0);

            cpu.reg.set_a(0x01);
            let old_hl = cpu.reg.hl();
            cpu.step();
            assert_eq!(cpu.reg.pc(), 0x105);
            assert_eq!(cpu.reg.a(), 0x01);
            assert_eq!(cpu.reg.hl(), 0x81FF);
            assert_eq!(cpu.mem[old_hl as usize], 0x01);

            cpu.reg.set_a(0x02);
            cpu.reg.set_hl(0x0000);
            let old_hl = cpu.reg.hl();
            cpu.step();
            assert_eq!(cpu.reg.pc(), 0x106);
            assert_eq!(cpu.reg.a(), 0x02);
            assert_eq!(cpu.reg.hl(), 0xFFFF);
            assert_eq!(cpu.mem[old_hl as usize], 0x02);

            cpu.reg.set_a(0x0C);
            cpu.reg.set_hl(0xFFFF);
            let old_hl = cpu.reg.hl();
            cpu.step();
            assert_eq!(cpu.reg.pc(), 0x107);
            assert_eq!(cpu.reg.a(), 0x0C);
            assert_eq!(cpu.reg.hl(), 0x0000);
            assert_eq!(cpu.mem[old_hl as usize], 0x0C);
        }
        #[test]
        fn test_ld_a_r16mem() {
            let mut cpu = setup(
                vec![
                    Instruction::LdAR16Mem { source: 0 },
                    Instruction::LdAR16Mem { source: 1 },
                    Instruction::LdAR16Mem { source: 2 },
                    Instruction::LdAR16Mem { source: 3 },
                ],
                Some(0xC0),
            );

            cpu.reg.set_bc(0x3B87);
            cpu.mem[0x3B87] = 0b_1011_0100;
            cpu.step();
            assert_eq!(cpu.reg.a(), 0b_1011_0100);

            cpu.reg.set_de(0xFF12);
            cpu.mem[0xFF12] = 0b_0100_0101;
            cpu.step();
            assert_eq!(cpu.reg.a(), 0b_0100_0101);

            cpu.reg.set_hl(0x1234);
            cpu.mem[0x1234] = 0b_0000_1111;
            cpu.step();
            assert_eq!(cpu.reg.a(), 0b_0000_1111);
            assert_eq!(cpu.reg.hl(), 0x1235);

            cpu.reg.set_hl(0xFF12);
            cpu.mem[0xFF12] = 0b_0101_1101;
            cpu.step();
            assert_eq!(cpu.reg.a(), 0b_0101_1101);
            assert_eq!(cpu.reg.hl(), 0xFF11);
        }
        #[test]
        fn test_ld_imm16_sp() {
            let mut cpu = setup(
                vec![
                    Instruction::LdImm16SP,
                    Instruction::Data { byte: 0xF1 },
                    Instruction::Data { byte: 0x32 },
                ],
                Some(0xC0),
            );
            cpu.reg.set_sp(0x87BA);

            let mut old_regs = cpu.reg;
            cpu.step();
            assert_eq!(cpu.pc(), 0x103);
            old_regs.set_pc(0x103);
            assert_eq!(cpu.mem[0x32F1 as usize], 0xBA);
            assert_eq!(cpu.mem[0x32F2 as usize], 0x87);
            assert_eq!(old_regs, cpu.reg);
        }
        #[test]
        fn test_inc_r16() {
            let mut cpu = setup(
                vec![
                    Instruction::IncR16 { operand: 0 },
                    Instruction::IncR16 { operand: 1 },
                    Instruction::IncR16 { operand: 2 },
                    Instruction::IncR16 { operand: 3 },
                ],
                Some(0x30),
            );
        }
        // #[test]
        // fn test_dec_r16() {
        //     assert!(false)
        // }
        // #[test]
        // fn test_add_hl_r16() {
        //     assert!(false)
        // }
        // #[test]
        // fn test_inc_r8() {
        //     assert!(false)
        // }
        // #[test]
        // fn test_dec_r8() {
        //     assert!(false)
        // }
        // #[test]
        // fn test_ld_r8_imm8() {
        //     assert!(false)
        // }
        // #[test]
        // fn test_rlca() {
        //     assert!(false)
        // }
        // #[test]
        // fn test_rrca() {
        //     assert!(false)
        // }
        // #[test]
        // fn test_rla() {
        //     assert!(false)
        // }
        // #[test]
        // fn test_rra() {
        //     assert!(false)
        // }
        // #[test]
        // fn test_daa() {
        //     assert!(false)
        // }
        // #[test]
        // fn test_cpl() {
        //     assert!(false)
        // }
        // #[test]
        // fn test_scf() {
        //     assert!(false)
        // }
        // #[test]
        // fn test_ccf() {
        //     assert!(false)
        // }
        // #[test]
        // fn test_jr_imm8() {
        //     assert!(false)
        // }
        // #[test]
        // fn test_jr_cond_imm8() {
        //     assert!(false)
        // }
        // #[test]
        // fn test_stop() {
        //     assert!(false)
        // }
    }
}
