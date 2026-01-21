use wasm_bindgen::prelude::*;

pub mod env;
pub mod interpreter;
pub mod parser;
pub mod sexpr;

use crate::interpreter::{MessageType, Message, State, Value};
use crate::parser::{read, scan};
use crate::sexpr::Sexpr;

#[wasm_bindgen]
pub fn new_state() -> JsValue {
    let state = State::new();
    serde_wasm_bindgen::to_value(&state).expect("Should be able to serialize new state via serde")
}

pub fn repl(state: State, user_input: &str) -> (State, Vec<String>) {
    let parsed = read(scan(user_input));
    match parsed {
        Ok(atoms) => {
            let exprs: Sexpr<Value> = atoms.into();
            return state.compute(exprs).flush_messages();
        }
        Err(err) => {
            let err = Message {
                typ: MessageType::Error,
                msg: err.into()
            }.to_string();
            return (state, vec![err])
        }
    }
}

#[wasm_bindgen]
pub fn repl_js(state: JsValue, user_input: JsValue) -> Result<JsValue, JsValue> {
    let state: State = serde_wasm_bindgen::from_value(state)?;
    let user_input: String = serde_wasm_bindgen::from_value(user_input)?;
    let result = repl(state, &user_input);
    let result_js = serde_wasm_bindgen::to_value(&result)?;
    Ok(result_js)
}
