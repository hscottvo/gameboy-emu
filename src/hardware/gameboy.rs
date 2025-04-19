extern crate sdl3;
use super::{cpu::Cpu, memory::Memory};
use sdl3::event::Event;
use sdl3::keyboard::Keycode;
use sdl3::pixels::Color;
use std::time::Duration;

static DISPLAY_WIDTH: u32 = 160;
static DISPLAY_HEIGHT: u32 = 144;

pub struct Gameboy {
    scale: u32,
    mem: Memory,
    cpu: Cpu,
}
impl Gameboy {
    pub fn new(scale: u32) -> Gameboy {
        Gameboy {
            scale,
            mem: Memory::new(),
            cpu: Cpu::new(),
        }
    }
}

impl Gameboy {
    pub fn run(&mut self) {
        let sdl_context = sdl3::init().unwrap();
        let video_subsystem = sdl_context.video().unwrap();

        let window = video_subsystem
            .window(
                "Gameboy",
                DISPLAY_WIDTH * self.scale,
                DISPLAY_HEIGHT * self.scale,
            )
            .position_centered()
            .build()
            .unwrap();

        let mut canvas = window.into_canvas();

        canvas.set_draw_color(Color::RGB(0, 255, 255));
        canvas.clear();
        canvas.present();

        let mut event_pump = sdl_context.event_pump().unwrap();

        let mut i = 0;

        self.cpu.step(&mut self.mem);

        'running: loop {
            i = (i + 1) % 255;
            canvas.set_draw_color(Color::RGB(i, 64, 255 - i));
            canvas.clear();
            for event in event_pump.poll_iter() {
                match event {
                    Event::Quit { .. }
                    | Event::KeyDown {
                        keycode: Some(Keycode::Escape),
                        ..
                    } => break 'running,
                    _ => {}
                }
            }
            canvas.present();
            ::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
        }
    }
}
