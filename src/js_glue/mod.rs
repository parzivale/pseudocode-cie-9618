pub struct UnPark;

// A function imitating `std::thread::spawn`.
pub fn spawn(f: impl FnOnce() + Send + 'static) -> Result<web_sys::Worker, JsValue> {
    let worker = web_sys::Worker::new("./worker.js")?;
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
    worker.post_message(&msg);
}


pub fn park(rec: Reciever<UnPark>) {
    while rec.recv().recv_timeout(Duration::from_millis(10)) != Ok(UnPark) {}
}

pub fn unpark(send: Sender<Unpark>) {
    sender.send(UnPark).unwrap();
}

#[wasm_bindgen]
// This function is here for `worker.js` to call.
pub fn worker_entry_point(addr: u32) {
  // Interpret the address we were given as a pointer to a closure to call.
  let closure = unsafe { Box::from_raw(ptr as *mut Box<dyn FnOnce()>) };
  (*closure)();
}