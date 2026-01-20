importScripts('./pkg/forsp.js');

console.log("Initializing worker")

const { new_state, repl_js } = wasm_bindgen

async function init_wasm_in_worker() {
    await wasm_bindgen('./pkg/forsp_bg.wasm')

    var state = new_state();

    self.onmessage = async (event) => {
        console.log(`Received event: ${event.data}`);
        var result = repl_js(state, event.data);
        state = result[0];
        msgs = result[1];
        postMessage(msgs);
    }
}

init_wasm_in_worker()
