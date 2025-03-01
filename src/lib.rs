use wasm_bindgen::prelude::*;
use wasm_bindgen::Clamped;
use wasm_bindgen::JsCast;
use web_sys::{CanvasRenderingContext2d, HtmlCanvasElement, ImageData};

pub mod hardware;

// #[wasm_bindgen]
// extern "C" {
//     pub fn alert(s: &str);
// }
//
// #[wasm_bindgen]
// pub fn greet(name: &str) {
//     alert(&format!("Hello, {}!", name));
// }

#[wasm_bindgen(start)]
pub fn start() -> Result<(), JsValue> {
    let document = web_sys::window().unwrap().document().unwrap();
    let canvas = document
        .get_element_by_id("demo-canvas")
        .unwrap()
        .dyn_into::<HtmlCanvasElement>()
        .unwrap();
    let context = canvas
        .get_context("2d")?
        .unwrap()
        .dyn_into::<CanvasRenderingContext2d>()
        .unwrap();

    // let width = canvas.width() as usize;
    // let height = canvas.height() as usize;
    let width = 144;
    let height = 160;

    let mut backbuffer = vec![0u8; width * height * 4];

    for y in 0..height {
        for x in 0..width {
            let offset = (y * width + x) * 4;
            backbuffer[offset] = (x % 256) as u8;
            backbuffer[offset + 1] = (y % 256) as u8;
            backbuffer[offset + 2] = 128 as u8;
            backbuffer[offset + 3] = 255 as u8;
        }
    }

    let image_data = ImageData::new_with_u8_clamped_array_and_sh(
        Clamped(&backbuffer), // Wrap the slice with Clamped
        width as u32,
        height as u32,
    )?;
    context.put_image_data(&image_data, 0.0, 0.0)?;
    Ok(())
}
