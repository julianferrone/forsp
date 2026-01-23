importScripts('./pkg/forsp.js');

console.log("Initializing worker")

// const { new_state, repl_js } = wasm_bindgen
const { new_vm_repl_state, vm_loop_js, vm_repl_js} = wasm_bindgen

// async function init_wasm_in_worker() {
//     await wasm_bindgen('./pkg/forsp_bg.wasm')
// 
//     var state = new_state();
// 
//     self.onmessage = async (event) => {
//         console.log(`Received event: ${event.data}`);
//         var result = repl_js(state, event.data);
//         state = result[0];
//         msgs = result[1];
//         postMessage(msgs);
//     }
// }

function log(label, data = {}) {
    console.log(`${label}`, data);
}

function get_messages(repl_state) {
    const messages = repl_state.messages;
    repl_state.messages = [];
    return messages
}

function flush_messages(repl_state) {
    const messages = get_messages(repl_state);
    log('Logging messages', messages);
    postMessage(messages);
}

async function init_vm() {
    await wasm_bindgen('./pkg/forsp_bg.wasm')
    
    var repl_state = new_vm_repl_state();
    const display = JSON.stringify(repl_state, null, 4);
    console.log(`Starting VM with repl state: ${display}`)

    self.onmessage = async (event) => {
        log('Received event', event.data);
        var new_state = vm_repl_js(repl_state, event.data);
        log('Current repl state', new_state);
        
        // Invalid state => don't overwrite repl_state
        if (new_state.status.hasOwnProperty('Invalid')) {
            flush_messages(new_state);
        };
        
        // Print messages while suspended, then keep looping
        while (new_state.status === 'Suspend') {
            log('Continuing suspended repl', new_state);
            flush_messages(new_state);
            new_state = vm_loop_js(new_state);
        };

        // If we've finished processing, we can safely snapshot the new VM
        if (new_state.status === 'RequestInput') {
            flush_messages(new_state);
            repl_state = new_state;
            log('Saved new repl state', new_state);
        }
    }
}

// init_wasm_in_worker()
init_vm()
