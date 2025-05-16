use gameboy::emulator;

fn main() {
    let mut emulator = match emulator::Emulator::new() {
        Ok(emu) => emu,
        Err(e) => {
            panic!("tootoo");
        }
    };
    emulator.run();
}
