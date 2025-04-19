extern crate sdl3;
use gameboy_hw::hardware::gameboy::Gameboy;

fn main() {
    let mut gb = Gameboy::new(5);

    gb.run();
}
