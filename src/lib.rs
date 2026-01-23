use wasm_bindgen::prelude::*;

pub mod env;
pub mod interpreter;
pub mod message;
pub mod nonempty;
pub mod parser;
pub mod primitive;
pub mod sexpr;
pub mod vm;

use crate::message::Message;
use crate::parser::{read, scan, parse};
use crate::sexpr::Sexpr;

use serde::{Deserialize, Serialize};

////// Interpreter //////

#[wasm_bindgen]
pub fn new_state() -> JsValue {
    let state = interpreter::State::new();
    serde_wasm_bindgen::to_value(&state).expect("Should be able to serialize new state via serde")
}

pub fn interpreter_repl(state: interpreter::State, user_input: &str) -> (interpreter::State, Vec<String>) {
    let parsed = read(scan(user_input));
    match parsed {
        Ok(atoms) => {
            let exprs: Sexpr<interpreter::Value> = atoms.into();
            return state.compute(exprs).flush_messages();
        }
        Err(err) => {
            let err = Message::msg_error(err).to_string();
            return (state, vec![err]);
        }
    }
}

#[wasm_bindgen]
pub fn interpreter_repl_js(state: JsValue, user_input: JsValue) -> Result<JsValue, JsValue> {
    let state: interpreter::State = serde_wasm_bindgen::from_value(state)?;
    let user_input: String = serde_wasm_bindgen::from_value(user_input)?;
    let result = interpreter_repl(state, &user_input);
    let result_js = serde_wasm_bindgen::to_value(&result)?;
    Ok(result_js)
}

////// VM //////


#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
struct VMReplState {
    vm: vm::VM,
    status: vm::VMStatus,
    messages: Vec<String>,
}

#[wasm_bindgen]
pub fn new_vm_repl_state() -> JsValue {
    let repl_state = VMReplState {
        vm: vm::VM::new(),
        status: vm::VMStatus::RequestInput,
        messages: vec![]
    };
    serde_wasm_bindgen::to_value(&repl_state).expect("Should be able to serialize new VM via serde")
}

pub fn vm_loop(repl_state: &mut VMReplState) {
    let status = repl_state.vm.step_until_yield();
    repl_state.status = status;
    repl_state.messages = repl_state
        .vm
        .flush_messages()
        .into_iter()
        .map(|msg: Message| msg.to_string())
        .collect();
}

pub fn vm_repl(repl_state: &mut VMReplState, user_input: &str) {
    let parsed = read(scan(user_input)).and_then(|atoms| parse(atoms));
    match parsed {
        Ok(instructions) => {
            repl_state.vm.set_instructions(instructions);
            vm_loop(repl_state);
        },
        Err(err) => {
            repl_state.status = vm::VMStatus::Invalid(err.clone());
            repl_state.messages = vec![Message::msg_error(err).to_string()]
        }
    }; 
}

// JS

#[wasm_bindgen]
pub fn vm_loop_js(repl_state: JsValue) -> JsValue {
    let mut repl_state: VMReplState = serde_wasm_bindgen::from_value(repl_state)
        .expect("Should be able to convert VMReplState into JSValue");
    vm_loop(&mut repl_state);
    serde_wasm_bindgen::to_value(&repl_state)
        .expect("Should be able to convert JSValue into VMReplState")
}

#[wasm_bindgen]
pub fn vm_repl_js(repl_state: JsValue, user_input: &str) -> JsValue {
    let mut repl_state: VMReplState = serde_wasm_bindgen::from_value(repl_state)
        .expect("Should be able to convert VMReplState into JSValue");
    vm_repl(&mut repl_state, user_input);
    serde_wasm_bindgen::to_value(&repl_state)
        .expect("Should be able to convert JSValue into VMReplState")
}
