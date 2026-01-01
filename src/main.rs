use std::io::prelude::*;
mod interpreter;
mod parser;

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
        println!("     | parsed line: {expr:?}");
        let env = state.env.clone();
        state = state.eval(expr, env);
        println!("     | evaluated: {state}");
    }
}
