use std::{sync::Arc, time::Duration};

use minifb::{Key, Window, WindowOptions};
use tokio::{sync::RwLock, time};
use tokio_util::sync::CancellationToken;

use crate::emulator::EmuError;

const WIDTH: usize = 640;
const HEIGHT: usize = 360;

pub struct EmuDisplay {
    counter: Arc<RwLock<u32>>,
    buffer: Vec<u32>,
    window: Window,
}

impl EmuDisplay {
    pub fn new(counter: Arc<RwLock<u32>>) -> Result<Self, minifb::Error> {
        let buffer: Vec<u32> = vec![0; WIDTH * HEIGHT];
        let window = Window::new(
            "Test - ESC to exit",
            WIDTH,
            HEIGHT,
            WindowOptions::default(),
        )?;
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
