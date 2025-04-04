use gameboy_hw::hardware::cpu::Cpu;
use gameboy_hw::hardware::gameboy::Gameboy;
use std::time::Duration;
use winit::event_loop::ControlFlow;
use winit::event_loop::EventLoop;

const SCALE: u32 = 8;
const HEIGHT: u32 = 160;
const WIDTH: u32 = 144;

fn main() {
    let event_loop = EventLoop::new().unwrap();
    event_loop.set_control_flow(ControlFlow::Poll);

    event_loop.set_control_flow(ControlFlow::Wait);

    let mut gameboy = Gameboy::new(HEIGHT, WIDTH, SCALE);
    let _ = event_loop.run_app(&mut gameboy);
    env_logger::init();

    let mut i = 0;
    let mut cpu = Cpu::new_with_cart("./roms/tetris.gb");

    loop {
        cpu.step();
        i = (i + 1) % 255;
        ::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
    }
}
