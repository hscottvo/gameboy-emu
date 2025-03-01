use super::cartridge::Cartridge;
use super::memory::Memory;
use super::registers::Registers;
enum Opcode {
    // block 0
    Nop,
    LdR16Imm16 { dest: u8 },
    LdR16MemA { dest: u8 },
    LdAR16Mem { source: u8 },
    LdImm16SP,
    IncR16 { operand: u8 },
    DecR16 { operand: u8 },
    AddHLR16 { operand: u8 },
    IncR8 { operand: u8 },
    DecR8 { operand: u8 },
    LdR8Imm8 { dest: u8 },
    RLCA,
    RRCA,
    RLA,
    RRA,
    DAA,
    CPL,
    SCF,
    CCF,
    JRImm8,
    JRCondImm8 { condition: u8 },
    Stop,

    // block 1
    Halt,
    LDR8R8 { dest: u8, source: u8 },

    // block 2
    AddAR8 { operand: u8 },
    AdcAR8 { operand: u8 },
    SubAR8 { operand: u8 },
    SbcAR8 { operand: u8 },
    AndAR8 { operand: u8 },
    XorAR8 { operand: u8 },
    OrAR8 { operand: u8 },
    CpAR8 { operand: u8 },

    // block 3
    AddAImm8,
    AdcAImm8,
    SubAImm8,
    SbcAImm8,
    AndAImm8,
    XorAImm8,
    OrAImm8,
    CpAImm8,
    RetCond { cond: u8 },
    Ret,
    RetI,
    JpCondImm16 { cond: u8 },
    JpImm16,
    JpHL,
    CallCondImm16 { cond: u8 },
    CallImm16,
    RstTgt3 { target: u8 },
    PopR16Stk { reg: u8 },
    PushR16Stk { reg: u8 },
    Prefix,
    LdhCA,
    LdhImm8A,
    LdImm16A,
    LdhAC,
    LdhAImm8,
    LdAImm16,
    AddSPImm8,
    LdHLSPImm8,
    LdSPHL,
    DI,
    EI,
}
enum PrefixOpcode {
    RLCR8 { operand: u8 },
    RRCR8 { operand: u8 },
    RLR8 { operand: u8 },
    RRR8 { operand: u8 },
    SLAR8 { operand: u8 },
    SRAR8 { operand: u8 },
    SwapR8 { operand: u8 },
    SRLR8 { operand: u8 },

    BitB3R8 { bit_index: u8, operand: u8 },
    ResB3R8 { bit_index: u8, operand: u8 },
    SetB3R8 { bit_index: u8, operand: u8 },
}
impl Opcode {
    pub fn from_byte(byte: u8) -> Option<Self> {
        use Opcode::*;
        // block 0
        if check_mask(byte, 0b_0000_0000, 0b_0000_0000) {
            Some(Nop)
        } else if check_mask(byte, 0b_0000_0001, 0b_0011_0000) {
            Some(LdR16Imm16 {
                dest: (byte >> 4) & 0b_0000_0011,
            })
        } else if check_mask(byte, 0b_0000_0010, 0b_0011_0000) {
            Some(LdR16MemA {
                dest: (byte >> 4) & 0b_0000_0011,
            })
        } else if check_mask(byte, 0b_0000_1010, 0b_0011_0000) {
            Some(LdAR16Mem {
                source: (byte >> 4) & 0b_0000_0011,
            })
        } else if check_mask(byte, 0b_0000_1000, 0b_0000_0000) {
            Some(LdImm16SP)
        } else if check_mask(byte, 0b_0000_0011, 0b_0011_0000) {
            Some(IncR16 {
                operand: (byte >> 4) & 0b_0000_0011,
            })
        } else if check_mask(byte, 0b_0000_1011, 0b_0011_0000) {
            Some(DecR16 {
                operand: (byte >> 4) & 0b_0000_0011,
            })
        } else if check_mask(byte, 0b_0000_1001, 0b_0011_0000) {
            Some(AddHLR16 {
                operand: (byte >> 4) & 0b_0000_0011,
            })
        } else if check_mask(byte, 0b_0000_1001, 0b_0011_0000) {
            Some(IncR8 {
                operand: (byte >> 3) & 0b_0000_0111,
            })
        } else if check_mask(byte, 0b_0000_1001, 0b_0011_0000) {
            Some(DecR8 {
                operand: (byte >> 3) & 0b_0000_0111,
            })
        } else if check_mask(byte, 0b_0000_0110, 0b_0011_1000) {
            Some(LdR8Imm8 {
                dest: (byte >> 3) & 0b_0000_0111,
            })
        } else if check_mask(byte, 0b_0000_0111, 0b_0000_0000) {
            Some(RLCA)
        } else if check_mask(byte, 0b_0000_1111, 0b_0000_0000) {
            Some(RRCA)
        } else if check_mask(byte, 0b_0001_0111, 0b_0000_0000) {
            Some(RLA)
        } else if check_mask(byte, 0b_0001_1111, 0b_0000_0000) {
            Some(RRA)
        } else if check_mask(byte, 0b_0010_0111, 0b_0000_0000) {
            Some(DAA)
        } else if check_mask(byte, 0b_0010_1111, 0b_0000_0000) {
            Some(CPL)
        } else if check_mask(byte, 0b_0011_0111, 0b_0000_0000) {
            Some(SCF)
        } else if check_mask(byte, 0b_0011_1111, 0b_0000_0000) {
            Some(CCF)
        } else if check_mask(byte, 0b_0001_1000, 0b_0000_0000) {
            Some(JRImm8)
        } else if check_mask(byte, 0b_0010_0000, 0b_0001_1000) {
            Some(JRCondImm8 {
                condition: (byte >> 3) & 0b_0000_0011,
            })
        } else if check_mask(byte, 0b_0001_0000, 0b_0000_0000) {
            Some(Stop)
        }
        // block 1
        else if check_mask(byte, 0b_0111_0110, 0b_0000_0000) {
            Some(Halt)
        } else if check_mask(byte, 0b_0100_0000, 0b_0011_1111) {
            Some(LDR8R8 {
                dest: (byte >> 3) & 0b_0000_0111,
                source: byte & 0b_0000_0111,
            })
        }
        // block 2
        else if check_mask(byte, 0b_1000_0000, 0b_0000_0111) {
            Some(AddAR8 {
                operand: byte & 0b_0000_0111,
            })
        }
        // block 3
        else if check_mask(byte, 0b_1100_0110, 0b_0000_0000) {
            Some(AddAImm8)
        } else if check_mask(byte, 0b_1100_1110, 0b_0000_0000) {
            Some(AdcAImm8)
        } else if check_mask(byte, 0b_1101_0110, 0b_0000_0000) {
            Some(SubAImm8)
        } else if check_mask(byte, 0b_1101_1110, 0b_0000_0000) {
            Some(SbcAImm8)
        } else if check_mask(byte, 0b_1110_0110, 0b_0000_0000) {
            Some(AndAImm8)
        } else if check_mask(byte, 0b_1110_1110, 0b_0000_0000) {
            Some(XorAImm8)
        } else if check_mask(byte, 0b_1111_0110, 0b_0000_0000) {
            Some(OrAImm8)
        } else if check_mask(byte, 0b_1111_1110, 0b_0000_0000) {
            Some(AdcAImm8)
        } else {
            None
        }
    }
}
pub struct Cpu {
    reg: Registers,
    mem: Memory,
}
fn check_mask(val: u8, mask: u8, params_mask: u8) -> bool {
    let mut bits = val ^ mask;
    bits = bits & !params_mask;
    bits == 0
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
        *self.reg.pc()
    }
    fn inc_pc(&mut self) {
        self.reg.set_pc(self.pc() + 1);
    }
    fn fetch_opcode(&self) -> u8 {
        self.mem[self.pc() as usize]
    }
    fn resolve_opcode(&mut self) -> Opcode {
        let code = self.fetch_opcode();
        self.inc_pc();

        if check_mask(code, 0b0000_0000, 0b0000_0000) {
            Opcode::Nop
        } else {
            Opcode::Nop
        }
    }
}

// block 0 of opcodes
impl Cpu {
    fn nop(&self) {}
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

    #[test]
    fn check_mask() {
        use super::super::cpu::check_mask;
        assert!(check_mask(0b_0000_0001, 0b_0000_0001, 0b_0011_0000));
        assert!(check_mask(0b_0000_0001, 0b_0010_0001, 0b_0011_0000));
        assert!(check_mask(0b_0000_0001, 0b_0001_0001, 0b_0011_0000));
        assert!(check_mask(0b_0000_0001, 0b_0011_0001, 0b_0011_0000));
        assert!(!check_mask(0b_0000_0001, 0b_1011_0001, 0b_0011_0000));
    }
}
