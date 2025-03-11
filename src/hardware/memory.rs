use super::cartridge::Cartridge;
use std::cmp::min;
use std::ops::{Index, IndexMut};

pub struct Memory {
    data: Vec<u8>,
}

impl Memory {
    pub fn new() -> Self {
        let ret = Memory {
            data: vec![0; 0x10000],
        };
        ret
    }
    pub fn new_with_cart(cart: Cartridge) -> Self {
        let mut ret = Memory {
            data: vec![0; 0x10000],
        };
        let data = cart.data();
        for i in 0..min(data.len(), 0x8000) {
            ret.data[i] = data[i];
        }
        ret
    }
}

impl Index<usize> for Memory {
    type Output = u8;
    fn index(&self, index: usize) -> &Self::Output {
        &self.data[index]
    }
}
impl IndexMut<usize> for Memory {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.data[index]
    }
}

#[cfg(test)]
mod test {
    use super::{Cartridge, Memory};

    #[test]
    fn new() {
        let mem = Memory::new();
        assert_ne!(mem.data, Vec::new());
    }

    #[test]
    fn load_pokemon_red() {
        let cart = Cartridge::new("./roms/pkmn-red.gb");
        let mem = Memory::new_with_cart(cart);

        assert_eq!(mem[0x0147], 0x13);
    }

    #[test]
    fn load_tetris() {
        let cart = Cartridge::new("./roms/tetris.gb");
        let mem = Memory::new_with_cart(cart);
        // for byte in &mem.data[0x015..0x030] {
        //     println!("{:#010b}", byte);
        // }
        // println!("main: {:#02X?}", &mem.data[0x015..0x030]);

        assert_eq!(mem[0x0147], 0);
    }

    #[test]
    fn load_tloz() {
        let cart = Cartridge::new("./roms/tloz.gb");
        let mem = Memory::new_with_cart(cart);

        assert_eq!(mem[0x0147], 0x03);
    }
}
