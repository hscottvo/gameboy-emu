use winit::application::ApplicationHandler;
use winit::dpi::LogicalSize;
use winit::event::WindowEvent;
use winit::event_loop::{ActiveEventLoop, ControlFlow, EventLoop};
use winit::window::{Window, WindowId};

#[derive(Default)]
pub struct Gameboy {
    window: Option<Window>,
    height: u32,
    width: u32,
    scale: u32,
}
impl Gameboy {
    pub fn new(height: u32, width: u32, scale: u32) -> Gameboy {
        Gameboy {
            window: None,
            height,
            width,
            scale,
        }
    }
}

impl ApplicationHandler for Gameboy {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        self.window = Some(
            event_loop
                .create_window(
                    Window::default_attributes()
                        .with_inner_size(LogicalSize::new(
                            self.width * self.scale,
                            self.height * self.scale,
                        ))
                        .with_min_inner_size(LogicalSize::new(self.width, self.height))
                        .with_resizable(true)
                        .with_title("Gameboy")
                        .with_decorations(true)
                        .with_active(true),
                )
                .unwrap(),
        );
    }
    fn window_event(&mut self, event_loop: &ActiveEventLoop, id: WindowId, event: WindowEvent) {
        match event {
            WindowEvent::CloseRequested => {
                println!("Closing window");
                event_loop.exit();
            }
            WindowEvent::RedrawRequested => {
                self.window.as_ref().unwrap().request_redraw();
            }
            _ => (),
        }
    }
}
