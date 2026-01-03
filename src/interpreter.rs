use std::fmt::Display;

use crate::sexpr::{Atom, Sexpr};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Atom(Atom),
    Closure(Box<Value>, Box<Value>),
    Primitive(fn(State, Value) -> Result<State, String>),
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
        let env = env_define_prim(env, "push", prim_push);
        let env = env_define_prim(env, "pop", prim_pop);
        let env = env_define_prim(env, "cons", prim_cons);
        let env = env_define_prim(env, "car", prim_car);
        let env = env_define_prim(env, "cdr", prim_cdr);
        let env = env_define_prim(env, "eq", prim_eq);
        let env = env_define_prim(env, "cswap", prim_cswap);
        let env = env_define_prim(env, "print", prim_print);

        // Extra primitives
        let env = env_define_prim(env, "stack", prim_stack);
        let env = env_define_prim(env, "env", prim_env);
        let env = env_define_prim(env, "+", prim_add);
        let env = env_define_prim(env, "-", prim_sub);
        let env = env_define_prim(env, "*", prim_mul);
        let env = env_define_prim(env, "/", prim_div);
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

fn env_define_prim(
    env: Value,
    name: &str,
    func: fn(State, Value) -> Result<State, String>,
) -> Value {
    let key = Value::Atom(Atom::Name(name.into()));
    env_define(env, key, Value::Primitive(func))
}

#[derive(Debug, Clone, PartialEq)]
pub struct State {
    pub stack: Value,
    pub env: Value,
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
        }
    }

    //////////         Value Stack Operations         //////////

    fn push(self, object: Value) -> State {
        State {
            stack: Value::cons(object, self.stack),
            env: self.env,
        }
    }

    fn peek(self) -> Result<Value, String> {
        match self.stack {
            Value::Sexpr(sexpr) => match *sexpr {
                Sexpr::Nil => Err("Stack is Nil".into()),
                Sexpr::Single(obj) => Ok(obj),
                Sexpr::Pair(car, _cdr) => Ok((*car).into()),
            },
            _ => Err("Stack should be an Object::Sexpr".into()),
        }
    }

    fn pop(self) -> Result<(Value, State), String> {
        match self.stack {
            Value::Sexpr(sexpr) => match *sexpr {
                Sexpr::Nil => Err("Stack is Nil".into()),
                Sexpr::Single(obj) => {
                    let state = State {
                        stack: Value::nil(),
                        env: self.env,
                    };
                    Ok((obj, state))
                }
                Sexpr::Pair(car, cdr) => {
                    let top = (*car).into();
                    let state = State {
                        stack: (*cdr).into(),
                        env: self.env,
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
        State {
            stack: self.stack,
            env: env,
        }
    }

    //////////                  Eval                  //////////

    fn apply_primitive(self, primitive: fn(State, Value) -> Result<State, String>) -> State {
        let env = self.env.clone();
        match primitive(self.clone(), env) {
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
                Sexpr::Pair(cmd_box, rest_box) => {
                    let rest = *rest_box;
                    let cmd_value: Value = (*cmd_box).into();

                    if let Value::Atom(Atom::Name(ref name)) = cmd_value {
                        if name == "quote" {
                            match rest {
                                Sexpr::Pair(quoted, tail) => {
                                    let quoted: Value = (*quoted).into();
                                    state = state.push(quoted);
                                    comp = *tail;
                                    continue;
                                }
                                _ => panic!("quote expects an argument"),
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
                        Err(err) => {
                            eprintln!("ERROR: evaluating atom: {}", err);
                            self
                        }
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

//////////             Core Primitives            //////////

fn prim_push(state: State, env: Value) -> Result<State, String> {
    let (key, state) = state.pop()?;
    let value = env_find(&env, &key)?;
    Ok(state.push(value))
}

fn prim_pop(state: State, env: Value) -> Result<State, String> {
    let ((key, value), state) = state.pop2()?;
    let env = env_define(env, key, value);
    Ok(State {
        stack: state.stack,
        env: env,
    })
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
    println!("PRINT: {top}");
    Ok(state)
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

mod tests {
    use super::*;
    use crate::parser::*;

    fn interpret_from_new(input: &str) -> State {
        let mut state = State::new();
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
