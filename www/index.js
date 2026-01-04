const INPUT_COLUMNS = 73; // 80 - 7 from "forsp> "
const input = document.getElementById("terminal-input");
const rendered = document.getElementById("rendered-input");
const cursor = document.getElementById("cursor");
const output = document.getElementById("output");

function appendText(text) {
    if (text.length <= 80) {
        appendLine(text)
    } else {
        const length = 80; // 80 characters in the terminal
        var split = [];
        for (var i = 0; i < text.length; i += length) {
            split.push(text.substr(i, length));
        }
        split.forEach(line => appendLine(line))
    }
}

function appendLine(text) {
    const line = document.createElement("p");
    line.textContent = text;
    output.appendChild(line);
    output.scrollTop = output.scrollHeight;
}

[
    "    __________  ____  _____ ____ ",
    "   / ____/ __ \\/ __ \\/ ___// __ \\",
    "  / /_  / / / / /_/ /\\__ \\/ /_/ /",
    " / __/ / /_/ / _, _/___/ / ____/ ",
    "/_/    \\____/_/ |_|/____/_/      ",
    "                                 ",
    "Welcome to Forsp!"
].forEach(line => appendLine(line))

function render() {
    const value = input.value;
    const pos = input.selectionStart ?? 0;
    const focused = document.activeElement === input;

    const before = value.slice(0, pos);
    const at = value[pos];
    const after = value.slice(pos + 1);

    rendered.innerHTML = "";
    rendered.append(document.createTextNode(before));

    const cursor = document.createElement("span");

    if (focused && at) {
        cursor.className = "cursor-block";
        cursor.textContent = at;
    } else if (focused && !at) {
        cursor.className = "cursor-eol";
        cursor.textContent = " ";
    } else if (!focused && at) {
        cursor.className = "cursor-unfocused";
        cursor.textContent = at;
    } else if (!focused && !at) {
        cursor.className = "cursor-unfocused";
        cursor.textContent = " ";
    }

    rendered.append(cursor);
    rendered.append(document.createTextNode(at ? after : ""));

    // --- horizontal scrolling ---
    const scrollCols = Math.max(0, pos - INPUT_COLUMNS + 1);
    rendered.style.transform = `translateX(${-scrollCols}ch)`;
}

async function run_wasm() {
    // Load the wasm file by awaiting the Promise returned by `wasm_bindgen`
    // `wasm_bindgen` was imported in `index.html`
    await wasm_bindgen('./pkg/forsp_bg.wasm')

    console.log('index.js loaded')
}

run_wasm();
var worker = new Worker("./worker.js");

[
    "input",
    "keyup",
    "click",
    "focus",
    "blur"
].forEach(evt => input.addEventListener(evt, render));

// Typing updates cursor
input.addEventListener("keydown", (e) => {
    if (e.key === "Enter") {
        const value = input.value;
        if (value.trim()) {
            appendText(`forsp> ${value}`);
        }
        worker.postMessage(value)
        input.value = "";
    }
    render();
});

worker.onmessage = (e) => {
    const worker_result = e.data;
    console.log(`Message received from worker: ${worker_result}`);
    const msgs = worker_result[0];
    const err_msgs = worker_result[1];

    msgs.forEach(line => appendText(line));
    err_msgs.forEach(line => appendText(`ERR: ${line}`));
}

// Focus terminal on click
document.getElementById("terminal").addEventListener("mousedown", () => {
    console.log("Clicked terminal");
    input.focus();
});

// initial render
render();