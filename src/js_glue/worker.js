importScripts("./wasm_bindgen/module.js")
self.onmessage = async event => {
  // event.data[0] should be the Memory object, and event.data[1] is the value to pass into child_entry_point
  const { child_entry_point } = await wasm_bindgen(
    "./wasm_bindgen/module_bg.wasm",
    event.data[0]
  )
  child_entry_point(Number(event.data[1]))
}