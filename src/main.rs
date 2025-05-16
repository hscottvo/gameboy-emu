use gameboy::emulator;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let mut emulator = emulator::Emulator::new()?;
    emulator.run();
    Ok(())
}
