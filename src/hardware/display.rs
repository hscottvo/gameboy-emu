use super::memory::Memory;
use std::ops::Index;

pub struct Display<'a> {
    mem: &'a Memory,
}
impl<'a> Display<'a> {
    pub fn new(mem: &'a Memory) -> Self {
        Display { mem }
    }
    pub fn print(&self) {
        // println!("all zeroes: {}", self.mem.data().iter().any(|x| *x != 0));
    }
}

impl<'a> Index<usize> for Display<'a> {
    type Output = u8;
    fn index(&self, index: usize) -> &Self::Output {
        &self.mem[index + 0x8000]
    }
}

#[cfg(test)]
mod test {
    use super::{Display, Memory};

    #[test]
    fn new() {
        let mut mem = Memory::new();
        mem[0x8000] = 1;
        let disp = Display::new(&mem);
        assert_eq!(disp[0], 1);
    }
}
