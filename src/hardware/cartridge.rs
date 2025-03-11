use log::debug;
use std::fs::read;
use std::ops::Index;

pub struct Cartridge {
    data: Vec<u8>,
}

impl Cartridge {
    pub fn new(path: &str) -> Self {
        let read_result = read(path);

        let data = match read_result {
            Ok(bytes) => bytes,
            Err(e) => panic!("{:?}", e), //{println!("{:?}", e); Vec::new()}
        };

        // debug!("Read {:} bytes", data.len());
        debug!("Read {:#06X} bytes", data.len());
        Cartridge { data }
    }

    pub fn mbc_type(&self) -> &u8 {
        &self.data[0x0147]
    }
    pub fn data(&self) -> &Vec<u8> {
        &self.data
    }
}

impl Index<usize> for Cartridge {
    type Output = u8;
    fn index(&self, index: usize) -> &Self::Output {
        &self.data[index]
    }
}

#[cfg(test)]
mod tests {
    use super::Cartridge;

    #[test]
    fn read_pokemon_red() {
        let cart = Cartridge::new("./roms/pkmn-red.gb");
        assert_ne!(cart.data, Vec::new());
        assert_eq!(*cart.mbc_type(), 0x13);
    }

    #[test]
    fn read_tetris() {
        let cart = Cartridge::new("./roms/tetris.gb");
        assert_eq!(*cart.mbc_type(), 0x00);
        assert_ne!(cart.data, Vec::new());
    }

    #[test]
    fn read_tloz() {
        let cart = Cartridge::new("./roms/tloz.gb");
        assert_ne!(cart.data, Vec::new());
        assert_eq!(*cart.mbc_type(), 0x03);
    }

    #[test]
    #[should_panic]
    fn dne() {
        let _ = Cartridge::new("./roms/dne.gb");
    }
}
