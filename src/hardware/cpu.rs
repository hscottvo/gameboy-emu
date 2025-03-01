use super::cartridge::Cartridge;
use super::instructions::{Instruction, PrefixOpcode};
use super::memory::Memory;
use super::registers::Registers;

pub struct Cpu {
    reg: Registers,
    mem: Memory,
}

impl Cpu {
    pub fn new_with_cart(path: &str) -> Self {
        let cart = Cartridge::new(path);
        // let cart = Cartridge::new("./roms/tloz.gb");
        let mut ret = Cpu {
            reg: Registers::default(),
            mem: Memory::new_with_cart(cart),
        };
        ret.reg.set_pc(0x100);
        ret
    }
    fn pc(&self) -> u16 {
        self.reg.pc()
    }
    fn inc_pc(&mut self) {
        self.reg.set_pc(self.pc() + 1);
    }
    fn fetch_byte(&self) -> u8 {
        self.mem[self.pc() as usize]
    }
    fn resolve_opcode(&mut self, byte: u8) -> Instruction {
        let opcode = Instruction::from_byte(byte);
        match opcode {
            Some(code) => code,
            None => {
                panic!("Unrecognized code: {:#010b}", byte);
            }
        }
    }
    fn execute_opcode(&mut self, instruction: Instruction) {
        use Instruction::*;
        match instruction {
            Nop => self.nop(),
            LdR16Imm16 { dest } => self.ld_r16_imm16(dest),
            LdR16MemA { dest } => self.ld_r16mem_a(dest),
            LdAR16Mem { source } => self.ld_a_r16mem(source),
            LdImm16SP => self.ld_imm16_sp(),
            IncR16 { operand } => self.inc_r16(operand),
            JpImm16 => self.jp_imm16(),
            _ => {
                println!("Not implemented: {:?}", instruction);
            }
        };
    }
    pub fn step(&mut self) {
        let byte = self.fetch_byte();
        self.inc_pc();
        let opcode = self.resolve_opcode(byte);
        self.execute_opcode(opcode);
    }

    pub fn imm16(&mut self) -> u16 {
        let lsb = self.fetch_byte() as u16;
        self.inc_pc();
        let msb = self.fetch_byte() as u16;
        self.inc_pc();
        msb << 8 | lsb
        // ((msb as u16) << 8) | lsb) as u16
    }
}

// block 0 of instructions
impl Cpu {
    fn nop(&self) {
        println!("Running NOP");
    }
    fn ld_r16_imm16(&mut self, dest: u8) {
        let val = self.imm16();
        self.reg.set_r16(val, dest);
    }
    fn ld_r16mem_a(&mut self, dest: u8) {
        let location = self.reg.get_r16(dest).expect("Invalid r16 register") as usize;
        self.mem[location] = self.reg.a();
    }
    fn ld_a_r16mem(&mut self, source: u8) {
        let location = self.reg.get_r16(source).expect("Invalid r16 register") as usize;
        self.reg.set_a(self.mem[location]);
    }
    // Copy SP & $FF at address n16 and SP >> 8 at address n16 + 1.
    fn ld_imm16_sp(&mut self) {
        let sp = self.reg.sp();

        self.inc_pc();
        let pc = self.pc() as usize;
        self.mem[pc] = (sp & 0xFF) as u8;

        self.inc_pc();
        let pc = self.pc() as usize;
        self.mem[pc] = (sp >> 8) as u8;

        self.inc_pc();
    }
    fn inc_r16(&mut self, operand: u8) {
        let r16 = self.reg.get_r16(operand).expect("Invalid r16 register");
        self.reg.set_r16(r16 + 1, operand);
    }
    // DecR16 { operand: u8 },
    // AddHLR16 { operand: u8 },
    // IncR8 { operand: u8 },
    // DecR8 { operand: u8 },
    // LdR8Imm8 { dest: u8 },
    // RLCA,
    // RRCA,
    // RLA,
    // RRA,
    // DAA,
    // CPL,
    // SCF,
    // CCF,
    // JRImm8,
    // JRCondImm8 { condition: u8 },
    // Stop,
}
impl Cpu {
    // // block 1
    // Halt,
    // LDR8R8 { dest: u8, source: u8 },
}
impl Cpu {
    // block 2
    // AddAR8 { operand: u8 },
    // AdcAR8 { operand: u8 },
    // SubAR8 { operand: u8 },
    // SbcAR8 { operand: u8 },
    // AndAR8 { operand: u8 },
    // XorAR8 { operand: u8 },
    // OrAR8 { operand: u8 },
    // CpAR8 { operand: u8 },
}
impl Cpu {
    //     // block 3
    //     AddAImm8,
    //     AdcAImm8,
    //     SubAImm8,
    //     SbcAImm8,
    //     AndAImm8,
    //     XorAImm8,
    //     OrAImm8,
    //     CpAImm8,
    //     RetCond { cond: u8 },
    //     Ret,
    //     RetI,
    //     JpCondImm16 { cond: u8 },
    //     JpImm16,
    fn jp_imm16(&mut self) {
        println!("Running jp_imm16, pc: {:#06X}", self.reg.pc());
        let dest_addr = self.imm16();
        self.reg.set_pc(dest_addr);
        println!("Finished running jp_imm16, pc: {:#06X}", self.reg.pc());
    }
    //     JpHL,
    //     CallCondImm16 { cond: u8 },
    //     CallImm16,
    //     RstTgt3 { target: u8 },
    //     PopR16Stk { reg: u8 },
    //     PushR16Stk { reg: u8 },
    //     Prefix,
    //     LdhCA,
    //     LdhImm8A,
    //     LdImm16A,
    //     LdhAC,
    //     LdhAImm8,
    //     LdAImm16,
    //     AddSPImm8,
    //     LdHLSPImm8,
    //     LdSPHL,
    //     DI,
    //     EI,
}

#[cfg(test)]
mod test {
    use super::Cpu;

    // xor value with mask, then or it with other 1-mask, then check if 0
    #[test]
    fn read_first_instruction() {
        let cpu = Cpu::new_with_cart("./roms/tetris.gb");
        assert_eq!(cpu.mem[0x100], 0b00000000);
        assert_eq!(cpu.mem[0x101], 0b11000011);
    }
}
