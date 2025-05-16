use std::fs::read;
use std::ops::Index;
use tracing::debug;

pub struct Cartridge {
    data: Vec<u8>,
}

impl Cartridge {
    /// # Errors
    ///
    /// Will return 'Err' if the the file is unable to be read
    pub fn new(path: &str) -> std::io::Result<Self> {
        let data = read(path)?;

        debug!("Read {:#06X} bytes", data.len());
        Ok(Cartridge { data })
    }

    #[must_use]
    pub fn mbc_type(&self) -> &u8 {
        &self.data[0x0147]
    }
    #[must_use]
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

    // #[test]
    // fn read_pokemon_red() {
    //     let cart = Cartridge::new("./roms/pkmn-red.gb").unwrap();
    //     assert_ne!(cart.data, Vec::new());
    //     assert_eq!(*cart.mbc_type(), 0x13);
    // }
    //
    // #[test]
    // fn read_tetris() {
    //     let cart = Cartridge::new("./roms/tetris.gb").unwrap();
    //     assert_eq!(*cart.mbc_type(), 0x00);
    //     assert_ne!(cart.data, Vec::new());
    // }
    //
    // #[test]
    // fn read_tloz() {
    //     let cart = Cartridge::new("./roms/tloz.gb").unwrap();
    //     assert_ne!(cart.data, Vec::new());
    //     assert_eq!(*cart.mbc_type(), 0x03);
    // }

    #[test]
    #[should_panic = "No such file or directory"]
    fn dne() {
        let _ = Cartridge::new("./roms/dne.gb").unwrap();
    }
}
