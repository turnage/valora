#[cfg(target_arch = "wasm32")]
fn main() {
    use wasm_bindgen::JsCast;

    eframe::WebLogger::init(log::LevelFilter::Info).ok();
    wasm_bindgen_futures::spawn_local(async {
        let canvas = web_sys::window()
            .and_then(|window| window.document())
            .and_then(|document| document.get_element_by_id("valora"))
            .and_then(|element| element.dyn_into::<web_sys::HtmlCanvasElement>().ok())
            .expect("the page to have a canvas with id `valora`");

        let result = eframe::WebRunner::new()
            .start(
                canvas,
                eframe::WebOptions::default(),
                Box::new(|cc| Ok(Box::new(valora_web::App::new(cc)))),
            )
            .await;
        if let Err(e) = result {
            log::error!("Failed to start valora: {:?}", e);
        }
    });
}

#[cfg(not(target_arch = "wasm32"))]
fn main() {
    eprintln!("valora_web runs in the browser; build it with ./build.sh");
}
