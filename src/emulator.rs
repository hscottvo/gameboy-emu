use std::error::Error;
use std::fmt;
use std::sync::Arc;
use tokio::sync::RwLock;
use tokio_util::sync::CancellationToken;

use crate::hardware::{cpu, emu_display::EmuDisplay, memory};

pub enum EmuError {
    MiniFBError(minifb::Error),
    TryFromIntError(std::num::TryFromIntError),
    IOError(std::io::Error),
    CPUError(cpu::CPUError),
    MemoryError(memory::MemoryError),
}

impl Error for EmuError {}
impl fmt::Display for EmuError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EmuError::MiniFBError(err) => write!(f, "MiniFB error: {err}"),
            EmuError::TryFromIntError(err) => write!(f, "TryFromIntError: {err}"),
            EmuError::IOError(err) => write!(f, "IO error: {err}"),
            EmuError::CPUError(err) => write!(f, "CPU error: {err}"),
            EmuError::MemoryError(err) => write!(f, "Memory error: {err}"),
        }
    }
}
impl fmt::Debug for EmuError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl From<minifb::Error> for EmuError {
    fn from(e: minifb::Error) -> Self {
        EmuError::MiniFBError(e)
    }
}

impl From<std::num::TryFromIntError> for EmuError {
    fn from(e: std::num::TryFromIntError) -> Self {
        EmuError::TryFromIntError(e)
    }
}

impl From<std::io::Error> for EmuError {
    fn from(e: std::io::Error) -> Self {
        EmuError::IOError(e)
    }
}

pub struct Emulator {
    runtime: tokio::runtime::Runtime,
    display: EmuDisplay,
    cpu: cpu::Cpu,
}

impl Emulator {
    /// # Errors
    ///
    /// Will return 'Err' if the runtime fails to start
    // pub fn new() -> std::io::Result<Self> {
    pub fn new() -> Result<Self, EmuError> {
        let runtime = tokio::runtime::Runtime::new()?;
        let counter = Arc::new(RwLock::new(0));
        let display = EmuDisplay::new(Arc::clone(&counter))?;
        let memory = Arc::new(RwLock::new(memory::Memory::new()?));
        let cpu = cpu::Cpu::new(Arc::clone(&counter), Arc::clone(&memory))?;
        Ok(Self {
            runtime,
            display,
            cpu,
        })
    }

    pub fn run(&mut self) {
        let token = CancellationToken::new();
        let cloned_token = token.clone();
        let _ = self
            .runtime
            .block_on(async { tokio::join!(self.cpu.run(cloned_token), self.display.run(token)) });
    }
}
