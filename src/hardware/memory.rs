use std::{
    error::Error,
    fmt::{self, Display},
};

use crate::emulator::EmuError;

#[derive(Debug, PartialEq)]
pub enum MemoryError {
    InitError,
    OutOfBounds { idx: usize, size: usize },
}
impl Error for MemoryError {}

impl From<MemoryError> for EmuError {
    fn from(e: MemoryError) -> Self {
        EmuError::MemoryError(e)
    }
}
impl Display for MemoryError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MemoryError::InitError => write!(f, "Memory failed to initialize"),
            MemoryError::OutOfBounds { idx, size } => {
                write!(f, "Out of bounds: index {idx} with size {size}")
            }
        }
    }
}
pub struct Memory {
    data: Vec<u8>,
}

impl Memory {
    /// # Errors
    ///
    /// Will return Err if fails to allocate memory
    pub fn new() -> Result<Self, MemoryError> {
        let data = vec![0; 0x10000];
        Ok(Memory { data })
    }
    /// # Errors
    ///
    /// Will return Err if the index is out of range
    pub fn read(&self, idx: usize) -> Result<u8, MemoryError> {
        self.data.get(idx).copied().ok_or(MemoryError::OutOfBounds {
            idx,
            size: self.data.len(),
        })
    }
    /// # Errors
    ///
    /// Will return Err if the index is out of range
    pub fn write(&mut self, value: u8, idx: usize) -> Result<(), MemoryError> {
        if idx >= self.data.len() {
            return Err(MemoryError::OutOfBounds {
                idx,
                size: self.data.len(),
            });
        }
        self.data[idx] = value;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{Memory, MemoryError};

    #[test]
    fn memory_size() {
        let mem = Memory::new().unwrap();
        assert_eq!(mem.data.len(), 0x10000);
    }

    #[test]
    fn read_oob() {
        let mem = Memory::new().unwrap();
        let result = mem.read(0x10000);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            MemoryError::OutOfBounds {
                idx: 0x10000,
                size: 0x10000
            }
        );
    }

    #[test]
    fn read_success() {
        let mut mem = Memory::new().unwrap();
        mem.data[0] = 100;
        let result = mem.read(0x0);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 100);
    }
}
