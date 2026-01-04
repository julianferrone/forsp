# Forsp

A Forsp interpreter, written in Rust and compiled to WASM.

## What is Forsp?

Forsp is a Forth+Lisp hybrid lambda calculus language with a call-by-push-value evaluation model. I'll let [Anthony Bonkoski, who came up with it, explain it himself](https://xorvoid.com/forsp.html).

## Running

To build this repo, you'll need wasm-pack installed. Build the WASM binaries with:

```shell
wasm-pack build --out-dir www/pkg --target no-modules
```

Which will produce compilation output under the **/www/pkg** folder.

You can then run this by running a local HTTP server from the **/www** folder, such as by using Python's built-in server:

```shell
cd ./www
python3 -m http.server --bind 127.0.0.1
```

## Todo

- [ ] Make CSS look better
- [ ] More efficient Sexpr representation
  - Right now, I'm using Serde's Serialize/Deserialize because enum variants with associated data aren't supported with `#[wasm_bindgen]`
  - It would be nice to have a more efficient Sexpr representation, probably using `Vec`s, so that 1. I can use wasm_bindgen 2. I can make the WASM binary smaller
- [ ] Add ability to save and load system states as files
- [ ] Create pipeline to inline all the files into one HTML file
  - I'd like for people to be able to just download one file that they can run on their local computers