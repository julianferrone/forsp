importScripts('./pkg/forsp.js');

console.log("Initializing worker")

const { new_state, repl_js } = wasm_bindgen

async function init_wasm_in_worker() {
    await wasm_bindgen('./pkg/forsp_bg.wasm')

    var state = new_state();

    self.onmessage = async (event) => {
        console.log(`Received event: ${event.data}`);
        var result = repl_js(state, event.data);
        console.log(`Calculated new state: ${result}`);
        state = result[0];
        msgs = result[1];
        errors = result[2];
        console.log(JSON.stringify(state, null, 2));
        console.log(JSON.stringify(msgs, null, 2));
        console.log(JSON.stringify(errors, null, 2));
        postMessage([msgs, errors]);
    }
}

init_wasm_in_worker()