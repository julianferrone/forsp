use std::io::prelude::*;

use crate::interpreter::Object;
mod interpreter;
mod parser;
mod sexpr;

fn get_user_input(prompt: &str) -> String {
    let mut buf = String::new();
    print!("{}", prompt);
    std::io::stdout().flush().unwrap();
    let stdin = std::io::stdin();
    let _line = stdin.read_line(&mut buf).unwrap();
    return buf;
}

fn main() {
    let mut state = interpreter::State::new();
    loop {
        let user_input = get_user_input("forsp> ");
        let expr: interpreter::Object = parser::read(parser::scan(&user_input)).unwrap().into();
        match expr {
            Object::Pair(_, _) => {
                state = state.compute(expr);
            }
            _ => state = state.eval(expr),
        }
        println!("stack: {}", state.stack);
        println!("")
    }
}
