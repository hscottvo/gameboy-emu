use log::debug;
fn check_mask(val: u8, mask: u8, params_mask: u8) -> bool {
    let mut bits = val ^ mask;
    bits = bits & !params_mask;
    bits == 0
}
#[derive(Debug, Clone, Copy, PartialEq)]
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
    LdR8R8 { dest: u8, source: u8 },

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
    Data { byte: u8 },
}

impl TryFrom<u8> for Instruction {
    type Error = String;
    fn try_from(byte: u8) -> Result<Instruction, String> {
        use Instruction::*;
        // block 0
        if check_mask(byte, 0b_0000_0000, 0) {
            // debug!("0b_0000_0000 => Nop");
            Ok(Nop)
        } else if check_mask(byte, 0b_0000_0001, 0b_0011_0000) {
            let dest = (byte >> 4) & 0b_0000_0011;
            // debug!("0b_00xx_0001: LdR16Imm16, destination {:#010b}", dest);
            Ok(LdR16Imm16 { dest })
        } else if check_mask(byte, 0b_0000_0010, 0b_0011_0000) {
            let dest = (byte >> 4) & 0b_0000_0011;
            // debug!("0b_00xx_0010: LdR16MemA, destination {:#010b}", dest);
            Ok(LdR16MemA { dest })
        } else if check_mask(byte, 0b_0000_1010, 0b_0011_0000) {
            let source = (byte >> 4) & 0b_0000_0011;
            // debug!("0b_00xx_1010: LdAR16Mem, source {:#010b}", source);
            Ok(LdAR16Mem { source })
        } else if check_mask(byte, 0b_0000_1000, 0) {
            Ok(LdImm16SP)
        } else if check_mask(byte, 0b_0000_0011, 0b_0011_0000) {
            Ok(IncR16 {
                operand: (byte >> 4) & 0b_0000_0011,
            })
        } else if check_mask(byte, 0b_0000_1011, 0b_0011_0000) {
            Ok(DecR16 {
                operand: (byte >> 4) & 0b_0000_0011,
            })
        } else if check_mask(byte, 0b_0000_1001, 0b_0011_0000) {
            Ok(AddHLR16 {
                operand: (byte >> 4) & 0b_0000_0011,
            })
        } else if check_mask(byte, 0b_0000_0100, 0b_0011_1000) {
            Ok(IncR8 {
                operand: (byte >> 3) & 0b_0000_0111,
            })
        } else if check_mask(byte, 0b_0000_0101, 0b_0011_1000) {
            Ok(DecR8 {
                operand: (byte >> 3) & 0b_0000_0111,
            })
        } else if check_mask(byte, 0b_0000_0110, 0b_0011_1000) {
            Ok(LdR8Imm8 {
                dest: (byte >> 3) & 0b_0000_0111,
            })
        } else if check_mask(byte, 0b_0000_0111, 0) {
            Ok(RLCA)
        } else if check_mask(byte, 0b_0000_1111, 0) {
            Ok(RRCA)
        } else if check_mask(byte, 0b_0001_0111, 0) {
            Ok(RLA)
        } else if check_mask(byte, 0b_0001_1111, 0) {
            Ok(RRA)
        } else if check_mask(byte, 0b_0010_0111, 0) {
            Ok(DAA)
        } else if check_mask(byte, 0b_0010_1111, 0) {
            Ok(CPL)
        } else if check_mask(byte, 0b_0011_0111, 0) {
            Ok(SCF)
        } else if check_mask(byte, 0b_0011_1111, 0) {
            Ok(CCF)
        } else if check_mask(byte, 0b_0001_1000, 0) {
            Ok(JRImm8)
        } else if check_mask(byte, 0b_0010_0000, 0b_0001_1000) {
            Ok(JRCondImm8 {
                condition: (byte >> 3) & 0b_0000_0011,
            })
        } else if check_mask(byte, 0b_0001_0000, 0) {
            Ok(Stop)
        }
        // block 1
        else if check_mask(byte, 0b_0111_0110, 0) {
            Ok(Halt)
        } else if check_mask(byte, 0b_0100_0000, 0b_0011_1111) {
            Ok(LdR8R8 {
                dest: (byte >> 3) & 0b_0000_0111,
                source: byte & 0b_0000_0111,
            })
        }
        // block 2
        else if check_mask(byte, 0b_1000_0000, 0b_0000_0111) {
            Ok(AddAR8 {
                operand: byte & 0b_0000_0111,
            })
        } else if check_mask(byte, 0b_1000_1000, 0b_0000_0111) {
            Ok(AdcAR8 {
                operand: byte & 0b_0000_0111,
            })
        } else if check_mask(byte, 0b_1001_0000, 0b_0000_0111) {
            Ok(SubAR8 {
                operand: byte & 0b_0000_0111,
            })
        } else if check_mask(byte, 0b_1001_1000, 0b_0000_0111) {
            Ok(SbcAR8 {
                operand: byte & 0b_0000_0111,
            })
        } else if check_mask(byte, 0b_1010_0000, 0b_0000_0111) {
            Ok(AndAR8 {
                operand: byte & 0b_0000_0111,
            })
        } else if check_mask(byte, 0b_1010_1000, 0b_0000_0111) {
            Ok(XorAR8 {
                operand: byte & 0b_0000_0111,
            })
        } else if check_mask(byte, 0b_1011_0000, 0b_0000_0111) {
            Ok(OrAR8 {
                operand: byte & 0b_0000_0111,
            })
        } else if check_mask(byte, 0b_1011_1000, 0b_0000_0111) {
            Ok(CpAR8 {
                operand: byte & 0b_0000_0111,
            })
        }
        // block 3
        else if check_mask(byte, 0b_1100_0110, 0) {
            Ok(AddAImm8)
        } else if check_mask(byte, 0b_1100_1110, 0) {
            Ok(AdcAImm8)
        } else if check_mask(byte, 0b_1101_0110, 0) {
            Ok(SubAImm8)
        } else if check_mask(byte, 0b_1101_1110, 0) {
            Ok(SbcAImm8)
        } else if check_mask(byte, 0b_1110_0110, 0) {
            Ok(AndAImm8)
        } else if check_mask(byte, 0b_1110_1110, 0) {
            Ok(XorAImm8)
        } else if check_mask(byte, 0b_1111_0110, 0) {
            Ok(OrAImm8)
        } else if check_mask(byte, 0b_1111_1110, 0) {
            Ok(CpAImm8)
        } else if check_mask(byte, 0b_1100_0000, 0b_0001_1000) {
            Ok(RetCond {
                cond: (byte >> 3) & 0b_0000_0011,
            })
        } else if check_mask(byte, 0b_1100_1001, 0) {
            Ok(Ret)
        } else if check_mask(byte, 0b_1101_1001, 0) {
            Ok(RetI)
        } else if check_mask(byte, 0b_1100_0010, 0b_0001_1000) {
            Ok(JpCondImm16 {
                cond: (byte >> 3) & 0b_0000_0011,
            })
        } else if check_mask(byte, 0b_1100_0011, 0) {
            Ok(JpImm16)
        } else if check_mask(byte, 0b_1110_1001, 0) {
            Ok(JpHL)
        } else if check_mask(byte, 0b_1100_0100, 0b_0001_1000) {
            Ok(CallCondImm16 {
                cond: (byte >> 3) & 0b_0000_0011,
            })
        } else if check_mask(byte, 0b_1100_1101, 0) {
            Ok(CallImm16)
        } else if check_mask(byte, 0b_1100_0111, 0b_0011_1000) {
            Ok(RstTgt3 {
                target: (byte >> 3) & 0b_0000_0111,
            })
        } else if check_mask(byte, 0b_1100_0001, 0b_0011_0000) {
            Ok(PopR16Stk {
                reg: (byte >> 4) & 0b_0000_0011,
            })
        } else if check_mask(byte, 0b_1100_0101, 0b_0011_0000) {
            Ok(PushR16Stk {
                reg: (byte >> 4) & 0b_0000_0011,
            })
        } else if check_mask(byte, 0b_1100_1011, 0) {
            Ok(Prefix)
        } else if check_mask(byte, 0b_1110_0010, 0) {
            Ok(LdhCA)
        } else if check_mask(byte, 0b_1110_0000, 0) {
            Ok(LdhImm8A)
        } else if check_mask(byte, 0b_1110_1010, 0) {
            Ok(LdImm16A)
        } else if check_mask(byte, 0b_1111_0010, 0) {
            Ok(LdhAC)
        } else if check_mask(byte, 0b_1111_0000, 0) {
            Ok(LdhAImm8)
        } else if check_mask(byte, 0b_1111_1010, 0) {
            Ok(LdAImm16)
        } else if check_mask(byte, 0b_1110_1000, 0) {
            Ok(AddSPImm8)
        } else if check_mask(byte, 0b_1111_1000, 0) {
            Ok(LdHLSPImm8)
        } else if check_mask(byte, 0b_1111_1001, 0) {
            Ok(LdSPHL)
        } else if check_mask(byte, 0b_1111_0011, 0) {
            Ok(DI)
        } else if check_mask(byte, 0b_1111_1011, 0) {
            Ok(EI)
        } else {
            Err(format!("Unrecognized instruction {:#04x}", byte))
        }
    }
}

impl From<Instruction> for u8 {
    fn from(i: Instruction) -> u8 {
        use Instruction::*;
        match i {
            // block 0
            Nop => 0x00,
            LdR16Imm16 { dest } => 0x01 | (dest << 4),
            LdR16MemA { dest } => 0x02 | (dest << 4),
            LdAR16Mem { source } => 0x06 | (source << 4),
            LdImm16SP => 0x08,
            IncR16 { operand } => 0x03 | (operand << 4),
            DecR16 { operand } => 0x0B | (operand << 4),
            AddHLR16 { operand } => 0x09 | (operand << 4),
            IncR8 { operand } => 0x04 | (operand << 3),
            DecR8 { operand } => 0x05 | (operand << 3),
            LdR8Imm8 { dest } => 0x06 | (dest << 3),
            RLCA => 0x07,
            RRCA => 0x0F,
            RLA => 0x17,
            RRA => 0x1F,
            DAA => 0x27,
            CPL => 0x2F,
            SCF => 0x37,
            CCF => 0x3F,
            JRImm8 => 0x18,
            JRCondImm8 { condition } => 0x20 | (condition << 3),
            Stop => 0x10,

            // block 1
            Halt => 0x76,
            LdR8R8 { dest, source } => 0x40 | (dest << 3) | source,

            // block 2
            AddAR8 { operand } => 0x80 | operand,
            AdcAR8 { operand } => 0x88 | operand,
            SubAR8 { operand } => 0x90 | operand,
            SbcAR8 { operand } => 0x98 | operand,
            AndAR8 { operand } => 0xA0 | operand,
            XorAR8 { operand } => 0xA8 | operand,
            OrAR8 { operand } => 0xB0 | operand,
            CpAR8 { operand } => 0xB8 | operand,

            // block 3
            AddAImm8 => 0xC6,
            AdcAImm8 => 0xCE,
            SubAImm8 => 0xD6,
            SbcAImm8 => 0xDE,
            AndAImm8 => 0xE6,
            XorAImm8 => 0xEE,
            OrAImm8 => 0xF6,
            CpAImm8 => 0xFE,
            RetCond { cond } => 0xC0 | (cond << 3),
            Ret => 0xC9,
            RetI => 0xD9,
            JpCondImm16 { cond } => 0xC2 | (cond << 3),
            JpImm16 => 0xC3,
            JpHL => 0xE9,
            CallCondImm16 { cond } => 0xC4 | (cond << 3),
            CallImm16 => 0xCD,
            RstTgt3 { target } => 0xC7 | (target << 3),
            PopR16Stk { reg } => 0xC1 | (reg << 4),
            PushR16Stk { reg } => 0xC5 | (reg << 4),
            Prefix => 0xCB,
            LdhCA => 0xE2,
            LdhImm8A => 0xE0,
            LdImm16A => 0xEA,
            LdhAC => 0xF2,
            LdhAImm8 => 0xF0,
            LdAImm16 => 0xFA,
            AddSPImm8 => 0xE8,
            LdHLSPImm8 => 0xF8,
            LdSPHL => 0xF9,
            DI => 0xF3,
            EI => 0xFB,
            Data { byte } => byte,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum InstructionCB {
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

impl TryFrom<u8> for InstructionCB {
    type Error = String;
    fn try_from(byte: u8) -> Result<InstructionCB, String> {
        use InstructionCB::*;
        let operand_mask = 0b_0000_0111;
        let operand = byte & operand_mask;

        let bit_index_mask = 0b_0011_1000;
        let bit_index = (byte & bit_index_mask) >> 3;
        if check_mask(byte, 0b_0000_0000, operand_mask) {
            Ok(RLCR8 { operand })
        } else if check_mask(byte, 0b_0000_1000, operand_mask) {
            Ok(RRCR8 { operand })
        } else if check_mask(byte, 0b_0001_0000, operand_mask) {
            Ok(RLR8 { operand })
        } else if check_mask(byte, 0b_0001_1000, operand_mask) {
            Ok(RRR8 { operand })
        } else if check_mask(byte, 0b_0010_0000, operand_mask) {
            Ok(SLAR8 { operand })
        } else if check_mask(byte, 0b_0010_1000, operand_mask) {
            Ok(SRAR8 { operand })
        } else if check_mask(byte, 0b_0011_0000, operand_mask) {
            Ok(SwapR8 { operand })
        } else if check_mask(byte, 0b_0011_1000, operand_mask) {
            Ok(SRLR8 { operand })
        } else if check_mask(byte, 0b0100_0000, operand_mask | bit_index_mask) {
            Ok(BitB3R8 { bit_index, operand })
        } else if check_mask(byte, 0b1000_0000, operand_mask | bit_index_mask) {
            Ok(ResB3R8 { bit_index, operand })
        } else if check_mask(byte, 0b1100_0000, operand_mask | bit_index_mask) {
            Ok(SetB3R8 { bit_index, operand })
        } else {
            Err(format!("Unrecognized instruction_cb {:#04x}", byte))
        }
    }
}
impl From<InstructionCB> for u8 {
    fn from(i: InstructionCB) -> u8 {
        use InstructionCB::*;
        match i {
            RLCR8 { operand } => 0x00 | operand,
            RRCR8 { operand } => 0x08 | operand,
            RLR8 { operand } => 0x10 | operand,
            RRR8 { operand } => 0x18 | operand,
            SLAR8 { operand } => 0x20 | operand,
            SRAR8 { operand } => 0x28 | operand,
            SwapR8 { operand } => 0x30 | operand,
            SRLR8 { operand } => 0x38 | operand,

            BitB3R8 { bit_index, operand } => 0x40 | (bit_index << 3) | operand,
            ResB3R8 { bit_index, operand } => 0x80 | (bit_index << 3) | operand,
            SetB3R8 { bit_index, operand } => 0xC0 | (bit_index << 3) | operand,
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
