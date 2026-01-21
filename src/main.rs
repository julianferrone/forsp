use std::io::prelude::*;

use forsp::interpreter::{State, Value};
use forsp::parser::{read, scan};
use forsp::sexpr::Sexpr;

fn get_user_input(prompt: &str) -> String {
    let mut buf = String::new();
    print!("{}", prompt);
    std::io::stdout().flush().unwrap();
    let stdin = std::io::stdin();
    let _line = stdin.read_line(&mut buf).unwrap();
    return buf;
}

fn main() {
    let mut state = State::new();
    loop {
        let user_input = get_user_input("forsp> ");
        let expr: Sexpr<Value> = read(scan(&user_input)).unwrap().into();
        match expr {
            Sexpr::List(_) => {
                state = state.compute(expr);
            }
            _ => state = state.eval(expr.into()),
        }
        state = state.flush_messages_to_stdout();
    }
}
