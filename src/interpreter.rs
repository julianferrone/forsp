use serde::{Deserialize, Serialize};
use std::fmt::Display;

use crate::sexpr;
use crate::sexpr::{Atom, Sexpr};

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum Value {
    Atom(Atom),
    Closure(Box<Value>, Box<Value>),
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

    fn make_name(name: &str) -> Value {
        Atom::Name(name.into()).into()
    }

    fn make_closure(body: Value, env: Value) -> Value {
        Value::Closure(Box::new(body), Box::new(env))
    }

    fn default_env() -> Value {
        let env: Value = Value::Sexpr(Box::new(Sexpr::Nil));
        let env = env_define_prim(env, "push", Primitive::Push);
        let env = env_define_prim(env, "pop", Primitive::Pop);
        let env = env_define_prim(env, "cons", Primitive::Cons);
        let env = env_define_prim(env, "car", Primitive::Car);
        let env = env_define_prim(env, "cdr", Primitive::Cdr);
        let env = env_define_prim(env, "eq", Primitive::Eq);
        let env = env_define_prim(env, "cswap", Primitive::Cswap);
        let env = env_define_prim(env, "print", Primitive::Print);

        // Extra primitives
        let env = env_define_prim(env, "stack", Primitive::Stack);
        let env = env_define_prim(env, "env", Primitive::Env);
        let env = env_define_prim(env, "+", Primitive::Add);
        let env = env_define_prim(env, "-", Primitive::Sub);
        let env = env_define_prim(env, "*", Primitive::Mul);
        let env = env_define_prim(env, "/", Primitive::Div);
        let env = env_define_prim(env, "help", Primitive::Help);
        env
    }

    fn cons(car: Value, cdr: Value) -> Value {
        let car: Sexpr<Value> = car.into();
        let cdr: Sexpr<Value> = cdr.into();
        let obj: Value = Sexpr::cons(car, cdr).into();
        obj
    }

    fn nil() -> Value {
        Sexpr::Nil.into()
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
            Sexpr::Nil => Sexpr::Nil,
            Sexpr::Single(atom) => Sexpr::Single(atom.into()),
            Sexpr::Pair(car, cdr) => Sexpr::cons((*car).into(), (*cdr).into()),
        }
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
                    Value::Closure(body, _env) => {
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

fn env_find(env: &Value, key: &Value) -> Result<Value, String> {
    if !key.is_atom() {
        return Err("Expected key to be Object::Atom".into());
    }
    let mut kvs = env.clone();
    loop {
        match kvs {
            Value::Sexpr(sexpr) => match *sexpr {
                Sexpr::Nil => return Err(format!("Failed to find key {key} in environment {env}")),
                Sexpr::Single(_) => todo!(),
                Sexpr::Pair(kv, rest) => match &*kv {
                    Sexpr::Pair(ref k, ref v) => {
                        if **k == Sexpr::Single(key.clone()) {
                            return Ok((**v).clone().into());
                            // return Ok(Into::<Object>::into(**v));
                        };
                        kvs = (*rest).into();
                    }
                    _ => return Err("Expected kv to be a pair".into()),
                },
            },
            // Object::Nil => return Err(format!("Failed to find key {key} in environment {env}")),
            _ => return Err("Expected env to be an Object::Sexpr".into()),
        }
    }
}

fn env_define(env: Value, key: Value, value: Value) -> Value {
    Value::cons(Value::cons(key, value), env)
}

fn env_define_prim(env: Value, name: &str, prim: Primitive) -> Value {
    let key = Value::Atom(Atom::Name(name.into()));
    env_define(env, key, Value::Primitive(prim))
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct State {
    pub stack: Value,
    pub env: Value,
    pub messages: Sexpr<String>,
    pub err_messages: Sexpr<String>,
}

impl Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "STATE<{} {}>", self.stack, self.env)
    }
}

impl State {
    pub fn new() -> State {
        // Core primitives
        State {
            stack: Value::nil(),
            env: Value::default_env(),
            messages: Sexpr::Nil,
            err_messages: Sexpr::Nil,
        }
    }

    ////// Messages

    pub fn flush_messages_to_stdout(self) -> State {
        self.messages.into_iter().for_each(|msg| println!("{msg}"));
        self.err_messages
            .into_iter()
            .for_each(|msg| eprintln!("ERROR: {msg}"));
        State {
            messages: Sexpr::Nil,
            err_messages: Sexpr::Nil,
            ..self
        }
    }

    pub fn flush_messages(self) -> (State, Sexpr<String>, Sexpr<String>) {
        let state = State {
            messages: Sexpr::Nil,
            err_messages: Sexpr::Nil,
            ..self
        };
        (state, self.messages, self.err_messages)
    }

    pub fn print(self, msg: impl Into<String>) -> State {
        let msg = Sexpr::Single(msg.into());
        State {
            messages: Sexpr::cons(msg, self.messages),
            ..self
        }
    }

    pub fn eprint(self, msg: impl Into<String>) -> State {
        let msg = Sexpr::Single(msg.into());
        State {
            err_messages: Sexpr::cons(msg, self.err_messages),
            ..self
        }
    }

    //////////         Value Stack Operations         //////////

    fn push(self, object: Value) -> State {
        State {
            stack: Value::cons(object, self.stack),
            ..self
        }
    }

    fn pop(self) -> Result<(Value, State), String> {
        match self.stack {
            Value::Sexpr(sexpr) => match *sexpr {
                Sexpr::Nil => Err("Stack is Nil".into()),
                Sexpr::Single(obj) => {
                    let state = State {
                        stack: Value::nil(),
                        ..self
                    };
                    Ok((obj, state))
                }
                Sexpr::Pair(car, cdr) => {
                    let top = (*car).into();
                    let state = State {
                        stack: (*cdr).into(),
                        ..self
                    };
                    Ok((top, state))
                }
            },
            _ => Err("Stack should be an Object::Sexpr".into()),
        }
    }

    fn pop2(self) -> Result<((Value, Value), State), String> {
        let (a, state) = self.pop()?;
        let (b, state) = state.pop()?;
        Ok(((a, b), state))
    }

    //////////               Environment              //////////

    fn with_env(self, env: Value) -> State {
        State { env: env, ..self }
    }

    //////////                  Eval                  //////////

    fn apply_primitive(self, primitive: Primitive) -> State {
        let env = self.env.clone();
        let func = get_primitive_function(primitive);
        match func(self.clone(), env) {
            Ok(state) => state,
            Err(err) => {
                eprintln!("ERROR: applying primitive: {err}");
                self
            }
        }
    }

    pub fn compute(self, program: Sexpr<Value>) -> State {
        let mut state = self;
        let mut comp = program;

        loop {
            match comp {
                Sexpr::Nil => return state,
                Sexpr::Single(obj) => return state.eval(obj),
                Sexpr::Pair(ref cmd_box, ref rest_box) => {
                    let rest = *rest_box.clone();
                    let cmd_value: Value = (*cmd_box.clone()).into();

                    if let Value::Atom(Atom::Name(ref name)) = cmd_value {
                        if name == "quote" {
                            match rest {
                                Sexpr::Pair(quoted, tail) => {
                                    let quoted: Value = (*quoted).into();
                                    state = state.push(quoted);
                                    comp = *tail;
                                    continue;
                                }
                                _ => {
                                    state = state.eprint("quote expects an argument");
                                    continue;
                                }
                            }
                        }
                    }
                    state = state.eval(cmd_value);
                    comp = rest;
                }
            }
        }
    }

    pub fn eval(self, expr: Value) -> State {
        match expr {
            Value::Atom(ref atom) => match atom {
                Atom::Name(_) => {
                    let value = env_find(&self.env, &expr);

                    match value {
                        Ok(Value::Primitive(func)) => self.apply_primitive(func),

                        Ok(Value::Closure(body, closure_env)) => {
                            let saved_env = self.env.clone();

                            let state = self.with_env(*closure_env).compute((*body).into());

                            state.with_env(saved_env)
                        }
                        Ok(object) => self.push(object),
                        Err(err) => self.eprint(err),
                    }
                }
                Atom::Num(_) => self.push(expr),
            },

            Value::Sexpr(ref _sexpr) => {
                let closure = Value::make_closure(expr, self.env.clone());
                self.push(closure)
            }

            other => self.push(other),
        }
    }
}

////////////////////////////////////////////////////////////
//                   Primitive Functions                  //
////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum Primitive {
    Push,
    Pop,
    Eq,
    Cons,
    Car,
    Cdr,
    Cswap,
    Print,
    Help,
    Stack,
    Env,
    Add,
    Sub,
    Mul,
    Div,
}

fn get_primitive_function(primitive: Primitive) -> fn(State, Value) -> Result<State, String> {
    match primitive {
        Primitive::Push => prim_push,
        Primitive::Pop => prim_pop,
        Primitive::Eq => prim_eq,
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

fn prim_push(state: State, env: Value) -> Result<State, String> {
    let (key, state) = state.pop()?;
    let value = env_find(&env, &key)?;
    Ok(state.push(value))
}

fn prim_pop(state: State, env: Value) -> Result<State, String> {
    let ((key, value), state) = state.pop2()?;
    let env = env_define(env, key, value);
    Ok(State { env: env, ..state })
}

fn prim_eq(state: State, _env: Value) -> Result<State, String> {
    let ((a, b), state) = state.pop2()?;
    let result = if a == b {
        Value::make_name("t")
    } else {
        Value::nil()
    };
    Ok(state.push(result))
}

fn prim_cons(state: State, _env: Value) -> Result<State, String> {
    let ((a, b), state) = state.pop2()?;
    let pair = Value::cons(a, b);
    Ok(state.push(pair))
}

fn prim_car(state: State, _env: Value) -> Result<State, String> {
    let (x, state) = state.pop()?;
    let car = Value::car(x)?;
    Ok(state.push(car))
}

fn prim_cdr(state: State, _env: Value) -> Result<State, String> {
    let (x, state) = state.pop()?;
    let cdr = Value::cdr(x)?;
    Ok(state.push(cdr))
}

fn prim_cswap(state: State, _env: Value) -> Result<State, String> {
    let (top, state) = state.pop()?;
    let new_state = if top == Value::make_name("t") {
        let ((a, b), state) = state.pop2()?;
        state.push(a).push(b)
    } else {
        state
    };
    Ok(new_state)
}

fn prim_print(state: State, _env: Value) -> Result<State, String> {
    let (top, state) = state.pop()?;
    let messages = Sexpr::cons(Sexpr::Single(format!("{}", top)), state.messages);

    Ok(State {
        messages: messages,
        ..state
    })
}

fn prim_help(state: State, _env: Value) -> Result<State, String> {
    let help_messages: Sexpr<String> = sexpr!([
        "Welcome to Forsp: A Forth+Lisp Hybrid Lambda Calculus Language!".into(),
        "".into(),
        "Please find below the list of primitives defined in Forsp:".into(),
        "".into(),
        "SPECIAL FORMS: ".into(),
        "".into(),
        "Syntax | Parsed as      | Semantics                                                      ".into(),
        "-------+----------------+----------------------------------------------------------------".into(),
        "'foo   | quote foo      | The quoted literal (foo) is pushed to the stack".into(),
        "$foo   | quote foo pop  | A value will be popped from the stack and bound to \"foo\" in  ".into(),
        "       |                | the environment".into(),
        "^foo   | quote foo push | The name \"foo\" will be resolved in current environment and   ".into(),
        "       |                | pushed to the stack".into(),
        "".into(),
        "CORE: Primitives needed to self-implement".into(),
        "".into(),
        "primitive [args]          |  description                                      | example usage".into(),
        "--------------------------|---------------------------------------------------|--------------".into(),
        "push  [$name]             |  resolve \"name\" in environment and push           | 'foo push".into(),
        "pop   [$name $val]        |  bind \"val\" to \"name\" in environment              | 'foo pop".into(),
        "eq    [$a $b]             |  if \"a\" and \"b\" are equal, then \"t\", else \"()\"    | 'a 'b eq".into(),
        "cons  [$fst $snd]         |  construct a pair from \"fst\" and \"snd\"            | '(2 3) 1 cons".into(),
        "car   [$pair]             |  extract the first element of a pair              | '(1 2 3) car".into(),
        "cdr   [$pair]             |  extract the second element of a pair             | '(1 2 3) cdr".into(),
        "cswap [$cond $a $b]       |  if cond is \"t\" then perform a swap               | 1 2 't cswap".into(),
        "tag   [$obj]              |  query the type-tag of any object                 | ^tag tag".into(),
        "read  []                  |  read an s-expression from input data             | read".into(),
        "print [$obj]              |  print an object as an s-expression               | '(foo bar) print".into(),
        "".into(),
        "EXTRA: Additional primitives that are not strictly needed, but useful to have".into(),
        "".into(),
        "primitive [args]          |  description                                      | example usage".into(),
        "--------------------------|---------------------------------------------------|--------------".into(),
        "stack                     |  push the \"stack\" onto the stack: cons'ing self   | stack".into(),
        "env                       |  push the \"env\" onto the stack                    | env".into(),
        "+    [$b $a]              |  push the result of \"a+b\" (addition)              | 3 2 +".into(),
        "-    [$b $a]              |  push the result of \"a-b\" (subtraction)           | 3 2 -".into(),
        "*    [$b $a]              |  push the result of \"a*b\" (multiplication)        | 3 2 *".into(),
        "/    [$b $a]              |  push the result of \"a/b\" (division)              | 3 2 /".into(),
        "help                      |  print this table of built-in primitives          | print".into(),
        "".into()
    ]);
    Ok(State {
        messages: Sexpr::extend(state.messages, help_messages),
        ..state
    })
}

//////////            Extra Primitives            //////////.

fn prim_stack(state: State, _env: Value) -> Result<State, String> {
    let stack = state.stack.clone();
    Ok(state.push(stack))
}

fn prim_env(state: State, env: Value) -> Result<State, String> {
    Ok(state.push(env))
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

fn prim_add(state: State, _env: Value) -> Result<State, String> {
    let ((a, b), state) = state.pop2()?;
    let result = binary_num_op(a, b, |a, b| a + b)?;
    Ok(state.push(result))
}

fn prim_sub(state: State, _env: Value) -> Result<State, String> {
    let ((a, b), state) = state.pop2()?;
    let result = binary_num_op(a, b, |a, b| b - a)?;
    Ok(state.push(result))
}

fn prim_mul(state: State, _env: Value) -> Result<State, String> {
    let ((a, b), state) = state.pop2()?;
    let result = binary_num_op(a, b, |a, b| a * b)?;
    Ok(state.push(result))
}

fn prim_div(state: State, _env: Value) -> Result<State, String> {
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
    fn test_interpret_put() {
        let state = interpret_from_new("1");
        let (result, _state) = state.pop().expect("Should be Ok");
        assert_eq!(result, Value::Atom(Atom::Num(1)))
    }

    #[test]
    fn test_compute_swap() {
        let state = interpret_from_new("($x $y ^x ^y) $swap 1 2 swap stack");
        let (result, _state) = state.pop().expect("Should be Ok");
        let expected = Value::cons(
            Value::Atom(Atom::Num(1)),
            Value::cons(Value::Atom(Atom::Num(2)), Value::nil()),
        );
        assert_eq!(result, expected)
    }
}
