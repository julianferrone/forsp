use std::io::prelude::*;

mod interpreter;
mod nonempty;
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
        let expr: sexpr::Sexpr<interpreter::Value> =
            parser::read(parser::scan(&user_input)).unwrap().into();
        match expr {
            sexpr::Sexpr::List(_) => {
                state = state.compute(expr);
            }
            _ => state = state.eval(expr.into()),
        }
        state = state.flush_messages_to_stdout();
    }
}
