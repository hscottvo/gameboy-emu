use std::ops::Index;
use std::fs::read;

pub struct Cartridge {
    data: Vec<u8>,
}

impl Cartridge {
    pub fn new(path: &str) -> Self {
        let read_result = read(path);

        let data = match read_result {
            Ok(bytes) => bytes,
            Err(e) =>  panic!("{:?}", e)//{println!("{:?}", e); Vec::new()}
        };
        println!("{:#04X?}", &data[0x0104..=0x0133]);

        Cartridge{data: data}
    }
    pub fn data(&self) -> &Vec<u8> {
        &self.data
    }
    // pub fn 
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
    fn one() {
        let cart = Cartridge::new("./roms/tetris.gb");
        assert_ne!(cart.data, Vec::new());
    }

    #[test]
    #[should_panic]
    fn dne() {
        let _ = Cartridge::new("./roms/dne.gb");
    }
}
