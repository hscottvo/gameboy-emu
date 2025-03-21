// https://gbdev.io/pandocs/CPU_Registers_and_Flags.html
#[derive(Debug, Default)]
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
    sp: u16,
    pc: u16,
}

// getters
impl Registers {
    pub fn a(&self) -> &u8 {
        &self.a
    }
    pub fn b(&self) -> &u8 {
        &self.b
    }
    pub fn c(&self) -> &u8 {
        &self.c
    }
    pub fn d(&self) -> &u8 {
        &self.d
    }
    pub fn e(&self) -> &u8 {
        &self.e
    }
    pub fn f(&self) -> &u8 {
        &self.f
    }
    pub fn h(&self) -> &u8 {
        &self.h
    }
    pub fn l(&self) -> &u8 {
        &self.l
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
    pub fn sp(&self) -> &u16 {
        &self.sp
    }
    pub fn pc(&self) -> &u16 {
        &self.pc
    }
}

// setters
impl Registers {
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
}
