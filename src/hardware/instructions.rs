fn check_mask(val: u8, mask: u8, params_mask: u8) -> bool {
    let mut bits = val ^ mask;
    bits = bits & !params_mask;
    bits == 0
}
#[derive(Debug)]
pub enum Instruction {
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

impl Instruction {
    pub fn from_byte(byte: u8) -> Option<Self> {
        use Instruction::*;
        // block 0
        if check_mask(byte, 0b_0000_0000, 0) {
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
        } else if check_mask(byte, 0b_0000_1000, 0) {
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
        } else if check_mask(byte, 0b_0000_0111, 0) {
            Some(RLCA)
        } else if check_mask(byte, 0b_0000_1111, 0) {
            Some(RRCA)
        } else if check_mask(byte, 0b_0001_0111, 0) {
            Some(RLA)
        } else if check_mask(byte, 0b_0001_1111, 0) {
            Some(RRA)
        } else if check_mask(byte, 0b_0010_0111, 0) {
            Some(DAA)
        } else if check_mask(byte, 0b_0010_1111, 0) {
            Some(CPL)
        } else if check_mask(byte, 0b_0011_0111, 0) {
            Some(SCF)
        } else if check_mask(byte, 0b_0011_1111, 0) {
            Some(CCF)
        } else if check_mask(byte, 0b_0001_1000, 0) {
            Some(JRImm8)
        } else if check_mask(byte, 0b_0010_0000, 0b_0001_1000) {
            Some(JRCondImm8 {
                condition: (byte >> 3) & 0b_0000_0011,
            })
        } else if check_mask(byte, 0b_0001_0000, 0) {
            Some(Stop)
        }
        // block 1
        else if check_mask(byte, 0b_0111_0110, 0) {
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
        } else if check_mask(byte, 0b_1000_1000, 0b_0000_0111) {
            Some(AdcAR8 {
                operand: byte & 0b_0000_0111,
            })
        } else if check_mask(byte, 0b_1001_0000, 0b_0000_0111) {
            Some(SubAR8 {
                operand: byte & 0b_0000_0111,
            })
        } else if check_mask(byte, 0b_1001_1000, 0b_0000_0111) {
            Some(SbcAR8 {
                operand: byte & 0b_0000_0111,
            })
        } else if check_mask(byte, 0b_1010_0000, 0b_0000_0111) {
            Some(AndAR8 {
                operand: byte & 0b_0000_0111,
            })
        } else if check_mask(byte, 0b_1010_1000, 0b_0000_0111) {
            Some(XorAR8 {
                operand: byte & 0b_0000_0111,
            })
        } else if check_mask(byte, 0b_1011_0000, 0b_0000_0111) {
            Some(OrAR8 {
                operand: byte & 0b_0000_0111,
            })
        } else if check_mask(byte, 0b_1011_1000, 0b_0000_0111) {
            Some(CpAR8 {
                operand: byte & 0b_0000_0111,
            })
        }
        // block 3
        else if check_mask(byte, 0b_1100_0110, 0) {
            Some(AddAImm8)
        } else if check_mask(byte, 0b_1100_1110, 0) {
            Some(AdcAImm8)
        } else if check_mask(byte, 0b_1101_0110, 0) {
            Some(SubAImm8)
        } else if check_mask(byte, 0b_1101_1110, 0) {
            Some(SbcAImm8)
        } else if check_mask(byte, 0b_1110_0110, 0) {
            Some(AndAImm8)
        } else if check_mask(byte, 0b_1110_1110, 0) {
            Some(XorAImm8)
        } else if check_mask(byte, 0b_1111_0110, 0) {
            Some(OrAImm8)
        } else if check_mask(byte, 0b_1111_1110, 0) {
            Some(CpAImm8)
        } else if check_mask(byte, 0b_1100_0000, 0b_0001_1000) {
            Some(RetCond {
                cond: (byte >> 3) & 0b_0000_0011,
            })
        } else if check_mask(byte, 0b_1100_1001, 0) {
            Some(Ret)
        } else if check_mask(byte, 0b_1101_1001, 0) {
            Some(RetI)
        } else if check_mask(byte, 0b_1100_0010, 0b_0001_1000) {
            Some(JpCondImm16 {
                cond: (byte >> 3) & 0b_0000_0011,
            })
        } else if check_mask(byte, 0b_1100_0011, 0) {
            Some(JpImm16)
        } else if check_mask(byte, 0b_1110_1001, 0) {
            Some(JpHL)
        } else if check_mask(byte, 0b_1100_0100, 0b_0001_1000) {
            Some(CallCondImm16 {
                cond: (byte >> 3) & 0b_0000_0011,
            })
        } else if check_mask(byte, 0b_1100_1101, 0) {
            Some(CallImm16)
        } else if check_mask(byte, 0b_1100_0111, 0b_0011_1000) {
            Some(RstTgt3 {
                target: (byte >> 3) & 0b_0000_0111,
            })
        } else if check_mask(byte, 0b_1100_0001, 0b_0011_0000) {
            Some(PopR16Stk {
                reg: (byte >> 4) & 0b_0000_0011,
            })
        } else if check_mask(byte, 0b_1100_0101, 0b_0011_0000) {
            Some(PushR16Stk {
                reg: (byte >> 4) & 0b_0000_0011,
            })
        } else if check_mask(byte, 0b_1100_1011, 0) {
            Some(Prefix)
        } else if check_mask(byte, 0b_1110_0010, 0) {
            Some(LdhCA)
        } else if check_mask(byte, 0b_1110_0000, 0) {
            Some(LdhImm8A)
        } else if check_mask(byte, 0b_1110_1010, 0) {
            Some(LdhAC)
        } else if check_mask(byte, 0b_1111_0000, 0) {
            Some(LdhAImm8)
        } else if check_mask(byte, 0b_1111_1010, 0) {
            Some(LdAImm16)
        } else if check_mask(byte, 0b_1110_1000, 0) {
            Some(AddSPImm8)
        } else if check_mask(byte, 0b_1111_1000, 0) {
            Some(LdHLSPImm8)
        } else if check_mask(byte, 0b_1111_1001, 0) {
            Some(LdSPHL)
        } else if check_mask(byte, 0b_1111_0011, 0) {
            Some(DI)
        } else if check_mask(byte, 0b_1111_1011, 0) {
            Some(EI)
        } else {
            None
        }
    }
}

pub enum PrefixOpcode {
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

impl PrefixOpcode {
    pub fn from_byte(byte: u8) -> Option<Self> {
        use PrefixOpcode::*;
        let operand = byte & 0b_0000_0111;
        if check_mask(byte, 0b_0000_0000, 0) {
            Some(RLCR8 { operand })
        } else if check_mask(byte, 0b_0000_0001, 0) {
            Some(RRCR8 { operand })
        } else if check_mask(byte, 0b_0000_0010, 0) {
            Some(RLR8 { operand })
        } else if check_mask(byte, 0b_0000_0011, 0) {
            Some(RRR8 { operand })
        } else if check_mask(byte, 0b_0000_0100, 0) {
            Some(SLAR8 { operand })
        } else if check_mask(byte, 0b_0000_0101, 0) {
            Some(SRAR8 { operand })
        } else if check_mask(byte, 0b_0000_0110, 0) {
            Some(SwapR8 { operand })
        } else if check_mask(byte, 0b_0000_0111, 0) {
            Some(SRLR8 { operand })
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test {

    #[test]
    fn check_mask() {
        use super::check_mask;
        assert!(check_mask(0b_0000_0001, 0b_0000_0001, 0b_0011_0000));
        assert!(check_mask(0b_0000_0001, 0b_0010_0001, 0b_0011_0000));
        assert!(check_mask(0b_0000_0001, 0b_0001_0001, 0b_0011_0000));
        assert!(check_mask(0b_0000_0001, 0b_0011_0001, 0b_0011_0000));
        assert!(!check_mask(0b_0000_0001, 0b_1011_0001, 0b_0011_0000));
    }
}
