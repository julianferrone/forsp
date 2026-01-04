use wasm_bindgen::prelude::*;

mod interpreter;
mod parser;
mod sexpr;

use crate::interpreter::{State, Value};
use crate::parser::{read, scan};
use crate::sexpr::Sexpr;

#[wasm_bindgen]
pub fn new_state() -> JsValue {
    let state = State::new();
    serde_wasm_bindgen::to_value(&state).expect("Should be able to serialize new state via serde")
}

pub fn repl(state: State, user_input: &str) -> (State, Sexpr<String>, Sexpr<String>) {
    let parsed = read(scan(user_input));
    match parsed {
        Ok(atoms) => {
            let exprs: Sexpr<Value> = atoms.into();
            return state.compute(exprs).flush_messages();
        }
        Err(err) => {
            return (
                state,
                Sexpr::Nil,
                Sexpr::cons(Sexpr::Single(err), Sexpr::Nil),
            )
        }
    }
}

#[wasm_bindgen]
pub fn repl_js(state: JsValue, user_input: JsValue) -> Result<JsValue, JsValue> {
    let state: State = serde_wasm_bindgen::from_value(state)?;
    let user_input: String = serde_wasm_bindgen::from_value(user_input)?;
    let (new_state, msgs, error_msgs) = repl(state, &user_input);
    let msgs: Vec<String> = msgs.into();
    let error_msgs: Vec<String> = error_msgs.into();
    let result = (new_state, msgs, error_msgs);
    let result_js = serde_wasm_bindgen::to_value(&result)?;
    Ok(result_js)
}
