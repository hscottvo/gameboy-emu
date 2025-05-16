use std::sync::Arc;
use tokio::{
    sync::RwLock,
    time::{self, Duration},
};
use tokio_util::sync::CancellationToken;

use minifb::{Key, Window, WindowOptions};

const WIDTH: usize = 640;
const HEIGHT: usize = 360;

pub struct EmuDisplay {
    counter: Arc<RwLock<u32>>,
    buffer: Vec<u32>,
    window: Window,
}

pub enum EmuError {
    MiniFBError(minifb::Error),
    TryFromIntError(std::num::TryFromIntError),
    IOError(std::io::Error),
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

impl EmuDisplay {
    fn new(counter: Arc<RwLock<u32>>) -> Result<Self, minifb::Error> {
        let buffer: Vec<u32> = vec![0; WIDTH * HEIGHT];
        let window = Window::new(
            "Test - ESC to exit",
            WIDTH,
            HEIGHT,
            WindowOptions::default(),
        )?;
        // window.set_target_fps(10);
        Ok(EmuDisplay {
            counter,
            buffer,
            window,
        })
    }
    /// # Errors
    ///
    /// Will return 'Err' if the index written to is too big
    pub async fn run(&mut self, token: CancellationToken) -> Result<(), EmuError> {
        let mut x = 0;
        let mut interval = time::interval(Duration::from_millis(17));
        while self.window.is_open() && !self.window.is_key_down(Key::Escape) {
            interval.tick().await;
            x += 1;
            println!("frame {x}");
            let mut idx = usize::try_from(*self.counter.read().await)? + WIDTH * 10;
            idx %= WIDTH * HEIGHT;
            println!("writing {{u32::MAX}} to index {idx}");
            self.buffer.fill(0);
            self.buffer[idx] = u32::MAX;

            self.window
                .update_with_buffer(&self.buffer, WIDTH, HEIGHT)?;
        }
        Self::cancel_token(&token);
        Ok(())
    }
    fn cancel_token(token: &CancellationToken) {
        token.cancel();
    }
}

pub struct Cpu {
    counter: Arc<RwLock<u32>>,
}

impl Cpu {
    async fn increment(&mut self) {
        let mut counter = self.counter.write().await;
        *counter += 1;
        println!("counter is now {{*counter}}");
    }
    async fn run(&mut self, token: CancellationToken) {
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

pub struct Emulator {
    runtime: tokio::runtime::Runtime,
    display: EmuDisplay,
    cpu: Cpu,
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
        let cpu = Cpu {
            counter: Arc::clone(&counter),
        };
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
