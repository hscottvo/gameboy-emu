use log::error;
// https://gbdev.io/pandocs/CPU_Registers_and_Flags.html
pub mod flags {
    pub const Z: u8 = 0b_1000_0000;
    pub const N: u8 = 0b_0100_0000;
    pub const H: u8 = 0b_0010_0000;
    pub const C: u8 = 0b_0001_0000;
}
#[derive(Debug, Default, PartialEq, PartialOrd, Copy, Clone)]
pub struct Registers {
    // registers: they are 16-bit registers that can be read 1 or 2 bytes at a time
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: u8,
    h: u8,
    l: u8,
    pc: u16,
    sp: u16,
}

// getters
impl Registers {
    pub fn new() -> Self {
        Registers {
            a: 0x01,
            b: 0x00,
            c: 0x13,
            d: 0x00,
            e: 0xD8,
            f: 0x80,
            h: 0x01,
            l: 0x4D,
            pc: 0x0100,
            sp: 0xFFFE,
        }
    }
    pub fn get_r8(&self, reg: u8) -> Result<RegResult, String> {
        use R8Index::*;
        use RegResult::*;
        if reg == 6 {
            return Ok(Defer);
        }
        let index: Result<R8Index, String> = reg.try_into();
        match index {
            Ok(B) => Ok(ReturnU8 { val: self.b() }),
            Ok(C) => Ok(ReturnU8 { val: self.c() }),
            Ok(D) => Ok(ReturnU8 { val: self.d() }),
            Ok(E) => Ok(ReturnU8 { val: self.e() }),
            Ok(H) => Ok(ReturnU8 { val: self.h() }),
            Ok(L) => Ok(ReturnU8 { val: self.l() }),
            Ok(A) => Ok(ReturnU8 { val: self.a() }),
            _ => Err(format!("Invalid R16 index {}", reg)),
        }
    }
    pub fn get_r16(&self, reg: u8) -> Result<u16, String> {
        use R16Index::*;
        let index: Result<R16Index, String> = reg.try_into();
        match index {
            Ok(BC) => Ok(self.bc()),
            Ok(DE) => Ok(self.de()),
            Ok(HL) => Ok(self.hl()),
            Ok(SP) => Ok(self.sp()),
            _ => Err(format!("Invalid R16 index {}", reg)),
        }
    }
    pub fn get_r16stk(&self, reg: u8) -> Result<u16, String> {
        use R16StkIndex::*;
        let index: Result<R16StkIndex, String> = reg.try_into();
        match index {
            Ok(BC) | Ok(DE) | Ok(HL) => self.get_r16(reg),
            Ok(AF) => Ok(self.af()),
            _ => Err(format!("Invalid R16Stk index {}", reg)),
        }
    }
    pub fn get_r16mem(&mut self, reg: u8) -> Result<u16, String> {
        use R16MemIndex::*;
        let index: Result<R16MemIndex, String> = reg.try_into();
        match index {
            Ok(BC) | Ok(DE) => self.get_r16(reg),
            Ok(HLPlus) => Ok(self.hl_plus()),
            Ok(HLMinus) => Ok(self.hl_minus()),
            _ => Err(format!("Invalid R16Mem index {}", reg)),
        }
    }
    pub fn a(&self) -> u8 {
        self.a
    }
    pub fn b(&self) -> u8 {
        self.b
    }
    pub fn c(&self) -> u8 {
        self.c
    }
    pub fn d(&self) -> u8 {
        self.d
    }
    pub fn e(&self) -> u8 {
        self.e
    }
    pub fn f(&self) -> u8 {
        self.f
    }
    pub fn h(&self) -> u8 {
        self.h
    }
    pub fn l(&self) -> u8 {
        self.l
    }
    pub fn af(&self) -> u16 {
        ((self.a as u16) << 8) + self.f as u16
    }
    pub fn bc(&self) -> u16 {
        ((self.b as u16) << 8) + self.c as u16
    }
    pub fn de(&self) -> u16 {
        ((self.d as u16) << 8) + self.e as u16
    }
    pub fn hl(&self) -> u16 {
        ((self.h as u16) << 8) + self.l as u16
    }
    pub fn hl_plus(&mut self) -> u16 {
        let ret = ((self.h as u16) << 8) + self.l as u16;
        self.set_hl(self.hl().wrapping_add(1));
        ret
    }
    pub fn hl_minus(&mut self) -> u16 {
        let ret = ((self.h as u16) << 8) + self.l as u16;
        self.set_hl(self.hl().wrapping_sub(1));
        ret
    }
    pub fn sp(&self) -> u16 {
        self.sp
    }
    pub fn pc(&self) -> u16 {
        self.pc
    }
}

// setters
impl Registers {
    pub fn set_r8(&mut self, val: u8, dest: u8) -> RegResult {
        if dest == 6 {
            return RegResult::Defer;
        }
        match dest {
            0 => self.set_b(val),
            1 => self.set_c(val),
            2 => self.set_d(val),
            3 => self.set_e(val),
            4 => self.set_h(val),
            5 => self.set_l(val),
            7 => self.set_a(val),
            // _ => panic!("Invalid register R8"),
            _ => error!("Invalid R8 register {}", dest),
        };
        RegResult::Success
    }
    pub fn set_r16(&mut self, val: u16, dest: u8) -> RegResult {
        use R16Index::*;
        let index: Result<R16Index, String> = dest.try_into();
        match index {
            Ok(BC) => {
                self.set_bc(val);
                RegResult::Success
            }
            Ok(DE) => {
                self.set_de(val);
                RegResult::Success
            }
            Ok(HL) => {
                self.set_hl(val);
                RegResult::Success
            }
            Ok(SP) => {
                self.set_sp(val);
                RegResult::Success
            }
            Err(e) => {
                error!("{}", e);
                RegResult::Failure
            }
        };
        RegResult::Success
    }
    pub fn set_r16stk(&mut self, val: u16, dest: u8) -> RegResult {
        use R16StkIndex::*;
        let index: Result<R16StkIndex, String> = dest.try_into();
        match index {
            Ok(BC) | Ok(DE) | Ok(HL) => {
                self.set_r16(val, dest);
                RegResult::Success
            }
            Ok(AF) => {
                self.set_af(val);
                RegResult::Success
            }
            Err(e) => {
                error!("{}", e);
                RegResult::Failure
            }
        }
    }
    pub fn set_a(&mut self, value: u8) {
        self.a = value;
    }
    pub fn set_b(&mut self, value: u8) {
        self.b = value;
    }
    pub fn set_c(&mut self, value: u8) {
        self.c = value;
    }
    pub fn set_d(&mut self, value: u8) {
        self.d = value;
    }
    pub fn set_e(&mut self, value: u8) {
        self.e = value;
    }
    pub fn set_f(&mut self, value: u8) {
        self.f = value;
    }
    pub fn set_h(&mut self, value: u8) {
        self.h = value;
    }
    pub fn set_l(&mut self, value: u8) {
        self.l = value;
    }
    pub fn set_af(&mut self, value: u16) {
        self.set_a((value >> 8) as u8);
        self.set_f(value as u8);
    }
    pub fn set_bc(&mut self, value: u16) {
        self.set_b((value >> 8) as u8);
        self.set_c(value as u8);
    }
    pub fn set_de(&mut self, value: u16) {
        self.set_d((value >> 8) as u8);
        self.set_e(value as u8);
    }
    pub fn set_hl(&mut self, value: u16) {
        self.set_h((value >> 8) as u8);
        self.set_l(value as u8);
    }
    pub fn set_sp(&mut self, value: u16) {
        self.sp = value
    }
    pub fn set_pc(&mut self, value: u16) {
        self.pc = value
    }
}

pub enum RegResult {
    Success,
    ReturnU8 { val: u8 },
    ReturnU16 { val: u16 },
    Defer,
    Failure,
}

enum R8Index {
    B,
    C,
    D,
    E,
    H,
    L,
    A,
}
impl TryFrom<u8> for R8Index {
    type Error = String;
    fn try_from(reg: u8) -> Result<R8Index, String> {
        use R8Index::*;
        match reg {
            0 => Ok(B),
            1 => Ok(C),
            2 => Ok(D),
            3 => Ok(E),
            4 => Ok(H),
            5 => Ok(L),
            6 => Err(format!("References to [hl] must defer")),
            7 => Ok(A),
            _ => Err(format!("Invalid R8 index {}", reg)),
        }
    }
}

enum R16Index {
    BC,
    DE,
    HL,
    SP,
}
impl TryFrom<u8> for R16Index {
    type Error = String;
    fn try_from(reg: u8) -> Result<R16Index, String> {
        use R16Index::*;
        match reg {
            0 => Ok(BC),
            1 => Ok(DE),
            2 => Ok(HL),
            3 => Ok(SP),
            _ => Err(format!("Invalid R16 index {}", reg)),
        }
    }
}

enum R16StkIndex {
    BC,
    DE,
    HL,
    AF,
}
impl TryFrom<u8> for R16StkIndex {
    type Error = String;
    fn try_from(reg: u8) -> Result<R16StkIndex, String> {
        use R16StkIndex::*;
        match reg {
            0 => Ok(BC),
            1 => Ok(DE),
            2 => Ok(HL),
            3 => Ok(AF),
            _ => Err(format!("Invalid R16Stk index {}", reg)),
        }
    }
}

enum R16MemIndex {
    BC,
    DE,
    HLPlus,
    HLMinus,
}
impl TryFrom<u8> for R16MemIndex {
    type Error = String;
    fn try_from(reg: u8) -> Result<R16MemIndex, String> {
        use R16MemIndex::*;
        match reg {
            0 => Ok(BC),
            1 => Ok(DE),
            2 => Ok(HLPlus),
            3 => Ok(HLMinus),
            _ => Err(format!("Invalid R16Mem index {}", reg)),
        }
    }
}

enum Cond {
    NZ,
    Z,
    NC,
    C,
}
impl TryFrom<u8> for Cond {
    type Error = String;
    fn try_from(reg: u8) -> Result<Cond, String> {
        use Cond::*;
        match reg {
            0 => Ok(NZ),
            1 => Ok(Z),
            2 => Ok(NC),
            3 => Ok(C),
            _ => Err(format!("Invalid condition index {}", reg)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Registers;

    #[test]
    fn r16_getters() {
        let mut reg: Registers = Registers::default();

        reg.set_a(0x3);
        reg.set_f(0x8);
        assert_eq!(reg.af(), 0x0308);

        reg.set_b(0xFF);
        reg.set_c(0x0F);
        assert_eq!(reg.bc(), 0xFF0F);

        reg.set_h(0x31);
        reg.set_l(0x15);
        assert_eq!(reg.hl(), 0x3115);
    }

    #[test]
    fn r16_setters() {
        let mut reg: Registers = Registers::default();

        reg.set_af(0x231F);
        assert_eq!(reg.a, 0x23);
        assert_eq!(reg.f, 0x1F);

        reg.set_bc(0x8A3E);
        assert_eq!(reg.b, 0x8A);
        assert_eq!(reg.c, 0x3E);

        reg.set_de(0xC312);
        assert_eq!(reg.d, 0xC3);
        assert_eq!(reg.e, 0x12);

        reg.set_hl(0x89BC);
        assert_eq!(reg.h, 0x89);
        assert_eq!(reg.l, 0xBC);
    }

    #[test]
    fn mix_setters() {
        let mut reg: Registers = Registers::default();

        reg.set_af(0xFF00);
        reg.set_a(0xF9);
        reg.set_f(0x9F);
        assert_eq!(reg.af(), 0xF99F);
    }

    #[test]
    fn get_r16mem() {
        let mut reg: Registers = Registers::default();

        reg.set_bc(0x3982);
        assert_eq!(reg.get_r16mem(0), Ok(0x3982));
    }
    #[test]
    fn hl_plus() {
        let mut reg = Registers::default();
        reg.set_hl(0x1233);
        let val = reg.hl_plus();
        assert_eq!(val, 0x1233);
        assert_eq!(reg.hl(), 0x1234);
    }
    #[test]
    fn hl_minus() {
        let mut reg = Registers::default();
        reg.set_hl(0x1233);
        let val = reg.hl_minus();
        assert_eq!(val, 0x1233);
        assert_eq!(reg.hl(), 0x1232);
    }
}
