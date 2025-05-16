use std::{
    error::Error,
    fmt::{self, Display},
    sync::Arc,
    time::Duration,
};

use tokio::{sync::RwLock, time};
use tokio_util::sync::CancellationToken;

use crate::emulator::EmuError;

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
}

impl Cpu {
    /// # Errors
    ///
    /// Will return Err if the CPU cannot be initialized
    pub fn new(counter: Arc<RwLock<u32>>) -> Result<Self, CPUError> {
        Ok(Cpu { counter })
    }
    async fn increment(&mut self) {
        let mut counter = self.counter.write().await;
        *counter += 1;
        println!("counter is now {{*counter}}");
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
        }
    }
}
