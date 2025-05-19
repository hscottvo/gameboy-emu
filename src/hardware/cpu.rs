use std::{
    error::Error,
    fmt::{self, Display},
    sync::Arc,
    time::Duration,
};

use tokio::{sync::RwLock, time};
use tokio_util::sync::CancellationToken;

use crate::emulator::EmuError;

use super::memory::Memory;

#[derive(Debug)]
pub struct CPUError;
impl Error for CPUError {}

impl From<CPUError> for EmuError {
    fn from(e: CPUError) -> Self {
        EmuError::CPUError(e)
    }
}
impl Display for CPUError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "CPU error: TODO")
    }
}
pub struct Cpu {
    counter: Arc<RwLock<u32>>,
    memory: Arc<RwLock<Memory>>,
}

impl Cpu {
    /// # Errors
    ///
    /// Will return Err if the CPU cannot be initialized
    pub fn new(counter: Arc<RwLock<u32>>, memory: Arc<RwLock<Memory>>) -> Result<Self, CPUError> {
        Ok(Cpu { counter, memory })
    }
    async fn increment(&mut self) {
        let mut counter = self.counter.write().await;
        *counter += counter.wrapping_add(1);
    }
    pub async fn run(&mut self, token: CancellationToken) {
        let mut interval = time::interval(Duration::from_millis(17));
        loop {
            if token.is_cancelled() {
                println!("token cancelled");
                return;
            }
            interval.tick().await;
            self.increment().await;
            let mut mem_write = self.memory.write().await;
            let read_counter = self.counter.read().await;
            mem_write
                .write((*read_counter & 0b1111_1111) as u8, 0)
                .unwrap();
        }
    }
}
