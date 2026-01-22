use serde::{Deserialize, Serialize};
use std::fmt::Display;

use crate::env::Env;
use crate::message::{Message, MessageType};
use crate::primitive::{Primitive, ApplyPrimitive};
use crate::sexpr::{Atom, Sexpr};

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum Instruction {
    AddValue(Value),
    Call(String),
    Primitive(Primitive),
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Closure {
    instructions: Box<Value>,
    env: Env<Value>
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum Value {
    Atom(Atom),
    Closure(Closure),
    Primitive(Primitive),
    Sexpr(Box<Sexpr<Value>>),
}

impl Value {
    fn is_atom(&self) -> bool {
        match self {
            Value::Atom(_) => true,
            _ => false,
        }
    }

    pub fn make_name(name: impl Into<String>) -> Value {
        Atom::Name(name.into()).into()
    }

    fn make_closure(body: Value, env: Env<Value>) -> Value {
        Value::Closure(Closure {
            instructions: Box::new(body),
            env: env
        })
    }

    fn cons(car: Value, cdr: Value) -> Value {
        let car: Sexpr<Value> = car.into();
        let cdr: Sexpr<Value> = cdr.into();
        let obj: Value = Sexpr::cons(car, cdr).into();
        obj
    }

    fn nil() -> Value {
        Sexpr::nil().into()
    }

    fn car(obj: Value) -> Result<Value, String> {
        match obj {
            Value::Sexpr(sexpr) => Ok(Sexpr::car(&*sexpr)?.into()),
            _ => Err("car expects Object::Sexpr".into()),
        }
    }

    fn cdr(obj: Value) -> Result<Value, String> {
        match obj {
            Value::Sexpr(sexpr) => Ok(Sexpr::cdr(&*sexpr)?.into()),
            _ => Err("cdr expects Object::Sexpr".into()),
        }
    }
}

impl From<Atom> for Value {
    fn from(value: Atom) -> Self {
        Value::Atom(value)
    }
}

impl From<Primitive> for Value {
    fn from(value: Primitive) -> Self {
        Value::Primitive(value)
    }
}

impl From<Sexpr<Value>> for Value {
    fn from(value: Sexpr<Value>) -> Self {
        match value {
            Sexpr::Single(obj) => obj,
            _ => Value::Sexpr(Box::new(value)),
        }
    }
}

impl From<Value> for Sexpr<Value> {
    fn from(value: Value) -> Self {
        match value {
            Value::Sexpr(sexpr) => *sexpr,
            _ => Sexpr::Single(value),
        }
    }
}

impl From<Sexpr<Atom>> for Sexpr<Value> {
    fn from(value: Sexpr<Atom>) -> Self {
        match value {
            Sexpr::Single(atom) => Sexpr::Single(atom.into()),
            // Sexpr::Pair(car, cdr) => Sexpr::cons((*car).into(), (*cdr).into()),
            Sexpr::List(atoms) => {
                let values: Vec<Box<Sexpr<Value>>> = atoms.iter()
                    .map(|boxed| {
                        let atom: Sexpr<Atom> = *boxed.clone();
                        let sexpr = atom.into();
                        // let sexpr = <Box<Sexpr<Atom>> as Into<Sexpr<Value>>>::into(boxed.clone());
                        Box::new(sexpr)
                    })
                    .collect();
                Sexpr::List(values)
            }
        }
    }
}

impl From<Env<Value>> for Value {
    fn from(value: Env<Value>) -> Value {
        let sexprs: Vec<Box<Sexpr<Value>>> = value.0.into_iter()
            .map(|(key, value): (String, Value)| {
                let key = Box::new(Sexpr::Single(Value::make_name(key)));
                let value = Box::new(Sexpr::Single(value.clone()));
                let pair = Sexpr::List(vec![key, value]);
                Box::new(pair)
            })
            .collect();
        Value::Sexpr(Box::new(Sexpr::List(sexprs)))
    }
}

enum Task<'a> {
    PrintObject(&'a Value),
    PrintStr(&'static str),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut stack: Vec<Task> = Vec::new();
        stack.push(Task::PrintObject(self));

        while let Some(task) = stack.pop() {
            match task {
                Task::PrintObject(obj) => match obj {
                    Value::Atom(name) => write!(f, "{name}")?,
                    Value::Closure(Closure {
                        instructions: body, 
                        env: _env
                    }) => {
                        stack.push(Task::PrintStr(">"));
                        stack.push(Task::PrintObject(body));
                        stack.push(Task::PrintStr("CLOSURE<"));
                    }
                    Value::Primitive(_func) => {
                        stack.push(Task::PrintStr("PRIM"));
                    }
                    Value::Sexpr(sexpr) => write!(f, "{sexpr}")?,
                },
                Task::PrintStr(s) => write!(f, "{s}")?,
            }
        }
        Ok(())
    }
}

////////////////////////////////////////////////////////////
//                          State                         //
////////////////////////////////////////////////////////////

//////////               Environment              //////////

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct State {
    pub stack: Vec<Value>,
    pub env: Env<Value>,
    pub messages: Vec<Message>,
}

impl Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let stack: Sexpr<Value> = Sexpr::from_vec(self.stack.clone());
        let env: Value = self.env.clone().into();
        write!(f, "STATE<{stack} {env}>")
    }
}

impl State {
    pub fn new() -> State {
        // Core primitives
        State {
            stack: vec![],
            env: Env::new(),
            messages: vec![],
        }
    }

    ////// Messages

    pub fn flush_messages_to_stdout(self) -> State {
        self.messages.into_iter().for_each(|msg| println!("{msg}"));
        State {
            messages: vec![], 
            ..self
        }
    }

    pub fn flush_messages(self) -> (State, Vec<String>) {
        let state = State {
            messages: vec![],
            ..self
        };
        let messages: Vec<String> = self.messages
            .into_iter()
            .map(|msg| msg.to_string())
            .collect();
        (state, messages)
    }

    pub fn print(self, msg: impl Into<String>) -> State {
        let mut messages = self.messages.clone();
        let msg = Message {
            typ: MessageType::Output,
            msg: msg.into(),
        };
        messages.push(msg);
        State {
            messages: messages,
            ..self
        }
    }

    pub fn eprint(self, msg: impl Into<String>) -> State {
        let mut messages = self.messages.clone();
        let msg = Message {
            typ: MessageType::Error,
            msg: msg.into(),
        };
        messages.push(msg);
        State {
            messages: messages,
            ..self
        }
    }

    //////////         Value Stack Operations         //////////

    fn push(self, object: Value) -> State {
        let mut stack = self.stack.clone();
        stack.push(object);
        State {
            stack: stack,
            ..self
        }
    }

    fn pop(self) -> Result<(Value, State), String> {
        let mut stack = self.stack.clone();
        match stack.pop() {
            Some(value) => {
                let state = State {stack: stack, ..self};
                Ok((value, state))
            },
            None => Err("pop expects stack to be non-empty".into())
        }
    }

    fn pop2(self) -> Result<((Value, Value), State), String> {
        let (a, state) = self.pop()?;
        let (b, state) = state.pop()?;
        Ok(((a, b), state))
    }

    //////////               Environment              //////////

    fn with_env(self, env: Env<Value>) -> State {
        State { env: env, ..self }
    }

    //////////                  Eval                  //////////

    pub fn compute(self, program: Sexpr<Value>) -> State {
        match program {
            Sexpr::Single(obj) => return self.eval(obj),
            Sexpr::List(sexprs) => {
                let mut state = self;
                let mut comp = sexprs;
                loop { 
                    match comp.pop().to_owned() {
                        Some(cmd_box) => {
                            let cmd_value: Value = (*cmd_box.clone()).into();
                            match cmd_value {
                                Value::Atom(Atom::Name(name)) if name == "quote" => {
                                    match comp.pop().to_owned() {
                                        Some(to_quote) => {
                                            let quoted: Value = (*to_quote).into();
                                            state = state.push(quoted);
                                            continue;
                                        }
                                        None => {
                                            state = state.eprint("quote expects an argument");
                                            continue;
                                        }
                                    }
                                },
                                other => {
                                    state = state.eval(other);
                                }
                            }
                        },
                        None => return state // No args to apply
                    }
                }
            }
        }
    }

    pub fn eval(self, expr: Value) -> State {
        match expr {
            Value::Atom(ref atom) => match atom {
                Atom::Name(name) => {
                    let value: Result<Value, String> = self.env
                        .find(&name)
                        .cloned();

                    match value {
                        Ok(Value::Primitive(func)) => self.apply_primitive(func),

                        Ok(Value::Closure(Closure {
                            instructions: body, 
                            env: closure_env
                        })) => {
                            let saved_env = self.env.clone();

                            let state = self.with_env(closure_env)
                                .compute((*body).into());

                            state.with_env(saved_env)
                        }
                        Ok(object) => self.push(object),
                        Err(err) => self.eprint(err),
                    }
                }
                Atom::Num(_) => self.push(expr),
            },

            Value::Sexpr(_) => {
                let closure = Value::make_closure(expr, self.env.clone());
                self.push(closure)
            }

            other => self.push(other),
        }
    }
}

impl ApplyPrimitive for State {
    fn apply_primitive(self, primitive: Primitive) -> State {
        let env = self.env.clone();
        let func = get_primitive_function(primitive);
        match func(self.clone(), env) {
            Ok(state) => state,
            Err(err) => {
                let state = self.eprint(format!("applying primitive: {err}"));
                state
            }
        }
    }
}

////////////////////////////////////////////////////////////
//                   Primitive Functions                  //
////////////////////////////////////////////////////////////

fn get_primitive_function(primitive: Primitive) -> fn(State, Env<Value>) -> Result<State, String> {
    match primitive {
        Primitive::Push => prim_push,
        Primitive::Pop => prim_pop,
        Primitive::Equals => prim_eq,
        Primitive::Cons => prim_cons,
        Primitive::Car => prim_car,
        Primitive::Cdr => prim_cdr,
        Primitive::Cswap => prim_cswap,
        Primitive::Print => prim_print,
        Primitive::Help => prim_help,
        Primitive::Stack => prim_stack,
        Primitive::Env => prim_env,
        Primitive::Add => prim_add,
        Primitive::Sub => prim_sub,
        Primitive::Mul => prim_mul,
        Primitive::Div => prim_div,
    }
}

//////////             Core Primitives            //////////

fn prim_push(state: State, env: Env<Value>) -> Result<State, String> {
    let (key, state) = state.pop()?;
    if let Value::Atom(Atom::Name(name)) = key {
        let value = env.find(&name)?;
        Ok(state.push(value.clone()))
    } else {
        Err("push expects the top of the stack to be a Value::Atom(Atom::Name)".into())
    }
}

fn prim_pop(state: State, env: Env<Value>) -> Result<State, String> {
    let ((key, value), state) = state.pop2()?;
    if let Value::Atom(Atom::Name(name)) = key {
        let env = env.define(name, value);
        Ok(State {env: env, ..state })
    } else {
        let msg = format!("prim_pop expects the top of the stack to be Value::Atom(Atom::Name), got {key:?}");
        Err(msg)
    }
}

fn prim_eq(state: State, _env: Env<Value>) -> Result<State, String> {
    let ((a, b), state) = state.pop2()?;
    let result = if a == b {
        Value::make_name("t")
    } else {
        Value::nil()
    };
    Ok(state.push(result))
}

fn prim_cons(state: State, _env: Env<Value>) -> Result<State, String> {
    let ((a, b), state) = state.pop2()?;
    let pair = Value::cons(a, b);
    Ok(state.push(pair))
}

fn prim_car(state: State, _env: Env<Value>) -> Result<State, String> {
    let (x, state) = state.pop()?;
    let car = Value::car(x)?;
    Ok(state.push(car))
}

fn prim_cdr(state: State, _env: Env<Value>) -> Result<State, String> {
    let (x, state) = state.pop()?;
    let cdr = Value::cdr(x)?;
    Ok(state.push(cdr))
}

fn prim_cswap(state: State, _env: Env<Value>) -> Result<State, String> {
    let (top, state) = state.pop()?;
    let new_state = if top == Value::make_name("t") {
        let ((a, b), state) = state.pop2()?;
        state.push(a).push(b)
    } else {
        state
    };
    Ok(new_state)
}

fn prim_print(state: State, _env: Env<Value>) -> Result<State, String> {
    let (top, state) = state.pop()?;
    let mut messages = state.messages;
    messages.push(Message {
        typ: MessageType::Output,
        msg: top.to_string()
    });
    
    Ok(State {
        messages: messages,
        ..state
    })
}

const HELP: &str = include_str!("help.txt");

fn prim_help(state: State, _env: Env<Value>) -> Result<State, String> {
    let help_messages = HELP
        .lines()
        .map(|line| {
            Message {
                typ: MessageType::Output,
                msg: line.to_owned()
            }
        })
        .collect::<Vec<Message>>();
    let mut messages = state.messages;
    messages.extend(help_messages);
    Ok(State {
        messages: messages, 
        ..state
    })
}

//////////            Extra Primitives            //////////.

fn prim_stack(state: State, _env: Env<Value>) -> Result<State, String> {
    let stack = Sexpr::from_vec(state.stack.clone()).into();
    Ok(state.push(stack))
}

fn prim_env(state: State, env: Env<Value>) -> Result<State, String> {
  Ok(state.push(env.into()))
}

fn binary_num_op(a: Value, b: Value, func: fn(usize, usize) -> usize) -> Result<Value, String> {
    match (&a, &b) {
        (Value::Atom(Atom::Num(num_a)), Value::Atom(Atom::Num(num_b))) => {
            Ok(Value::Atom(Atom::Num(func(*num_a, *num_b))))
        }
        (_, _) => Err(format!(
            "Expected topmost two args to be Object::Atom(Atom::Num, a)re {}, {}",
            a, b
        )),
    }
}

fn prim_add(state: State, _env: Env<Value>) -> Result<State, String> {
    let ((a, b), state) = state.pop2()?;
    let result = binary_num_op(a, b, |a, b| a + b)?;
    Ok(state.push(result))
}

fn prim_sub(state: State, _env: Env<Value>) -> Result<State, String> {
    let ((a, b), state) = state.pop2()?;
    let result = binary_num_op(a, b, |a, b| b - a)?;
    Ok(state.push(result))
}

fn prim_mul(state: State, _env: Env<Value>) -> Result<State, String> {
    let ((a, b), state) = state.pop2()?;
    let result = binary_num_op(a, b, |a, b| a * b)?;
    Ok(state.push(result))
}

fn prim_div(state: State, _env: Env<Value>) -> Result<State, String> {
    let ((a, b), state) = state.pop2()?;
    let result = binary_num_op(a, b, |a, b| b / a)?;
    Ok(state.push(result))
}

////////////////////////////////////////////////////////////
//                          Tests                         //
////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::*;

    fn interpret_from_new(input: &str) -> State {
        let state = State::new();
        let cmd: Sexpr<Value> = read(scan(input))
            .expect("Test input should be well-formed")
            .into();
        state.compute(cmd)
    }

    #[test]
    fn run_put_num() {
        let state = interpret_from_new("1");
        let (result, _state) = state.pop().expect("Should be Ok");
        assert_eq!(result, Value::Atom(Atom::Num(1)))
    }

    #[test]
    fn run_put_quoted_name() {
        let state = interpret_from_new("'a");
        let (result, _state) = state.pop().expect("Should be Ok");
        assert_eq!(result, Value::Atom(Atom::Name("a".into())))
    }

    #[test]
    fn run_pop2() {
        let state = interpret_from_new("1 2");
        let (result, _state) = state.pop2().expect("Should be Ok");
        let expected = (
            Value::Atom(Atom::Num(2)),
            Value::Atom(Atom::Num(1))
        );
        assert_eq!(result, expected) 
    }

    #[test]
    fn run_swap() {
        let state = interpret_from_new("($x $y ^x ^y) $swap 1 2 swap stack");
        let (result, _state) = state.pop().expect("Should be Ok");
        let expected = Value::cons(
            Value::Atom(Atom::Num(1)),
            Value::cons(Value::Atom(Atom::Num(2)), Value::nil()),
        );
        assert_eq!(result, expected)
    }

}
