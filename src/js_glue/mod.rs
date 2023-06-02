use std::sync::{
    atomic::{AtomicBool, Ordering},
    Arc,
};
use wasm_bindgen::prelude::*;
use web_sys::console;

// A function imitating `std::thread::spawn`.
pub fn spawn(f: impl FnOnce() + Send + 'static) {
    console::log_1(&JsValue::from("new thread spawned"));

    let worker = web_sys::Worker::new_with_options(
        "./worker_threading.js",
        web_sys::WorkerOptions::new().type_(web_sys::WorkerType::Module),
    ).unwrap();
    // Double-boxing because `dyn FnOnce` is unsized and so `Box<dyn FnOnce()>` is a fat pointer.
    // But `Box<Box<dyn FnOnce()>>` is just a plain pointer, and since wasm has 32-bit pointers,
    // we can cast it to a `u32` and back.
    let ptr = Box::into_raw(Box::new(Box::new(f) as Box<dyn FnOnce()>));
    let msg = js_sys::Array::new();
    // Send the worker a reference to our memory chunk, so it can initialize a wasm module
    // using the same memory.
    msg.push(&wasm_bindgen::memory());

    // Also send the worker the address of the closure we want to execute.
    msg.push(&JsValue::from(ptr as u32));
    console::log_1(&worker.post_message(&msg).err().unwrap_or_default());
}

pub fn park(parked: Arc<AtomicBool>) {
    parked.store(true, Ordering::SeqCst);
    while parked.load(Ordering::SeqCst) {}
}

pub fn unpark(parked: Arc<AtomicBool>) {
    parked.store(false, Ordering::SeqCst);
}

#[wasm_bindgen]
// This function is here for `worker.js` to call.
pub fn worker_entry_point(addr: u32) {
    #[cfg(feature = "wasm")]
    // Interpret the address we were given as a pointer to a closure to call.
    let closure = unsafe { Box::from_raw(addr as *mut Box<dyn FnOnce()>) };
    (*closure)();
}
