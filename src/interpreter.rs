use std::fmt::{format, Display};

use crate::sexpr::{Atom, Sexpr};

// TODO: Pull out Nil / Pair of SExpr into generic type SExpr<T>
// TODO: Pull out Atom as separate enum of Num(usize) | Name(String)
// then parser SExpr = SExpr<Atom>
// TODO: Pull out interpreter Object => SExpr<Atom | Closure | Primitive>

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Atom(Atom),
    Closure(Box<Object>, Box<Object>),
    Primitive(fn(State, Object) -> Result<State, String>),
    Sexpr(Box<Sexpr<Object>>),
}

impl Object {
    fn is_atom(&self) -> bool {
        match self {
            Object::Atom(_) => true,
            _ => false,
        }
    }

    fn make_name(name: &str) -> Object {
        Atom::Name(name.into()).into()
    }

    fn make_closure(body: Object, env: Object) -> Object {
        Object::Closure(Box::new(body), Box::new(env))
    }

    fn default_env() -> Object {
        let env: Object = Object::Sexpr(Box::new(Sexpr::Nil));
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

    fn cons(car: Object, cdr: Object) -> Object {
        let car: Sexpr<Object> = car.into();
        let cdr: Sexpr<Object> = cdr.into();
        let obj: Object = Sexpr::cons(car, cdr).into();
        obj
    }

    fn nil() -> Object {
        Sexpr::Nil.into()
    }

    fn car(obj: Object) -> Result<Object, String> {
        match obj {
            Object::Sexpr(sexpr) => Ok(Sexpr::car(&*sexpr)?.into()),
            _ => Err("car expects Object::Sexpr".into()),
        }
    }

    fn cdr(obj: Object) -> Result<Object, String> {
        match obj {
            Object::Sexpr(sexpr) => Ok(Sexpr::cdr(&*sexpr)?.into()),
            _ => Err("cdr expects Object::Sexpr".into()),
        }
    }
}

impl From<Atom> for Object {
    fn from(value: Atom) -> Self {
        Object::Atom(value)
    }
}

impl From<Sexpr<Object>> for Object {
    fn from(value: Sexpr<Object>) -> Self {
        match value {
            Sexpr::Single(obj) => obj,
            _ => Object::Sexpr(Box::new(value)),
        }
    }
}

impl From<Object> for Sexpr<Object> {
    fn from(value: Object) -> Self {
        match value {
            Object::Sexpr(sexpr) => *sexpr,
            _ => Sexpr::Single(value),
        }
    }
}

impl From<Sexpr<Atom>> for Sexpr<Object> {
    fn from(value: Sexpr<Atom>) -> Self {
        match value {
            Sexpr::Nil => Sexpr::Nil,
            Sexpr::Single(atom) => Sexpr::Single(atom.into()),
            Sexpr::Pair(car, cdr) => Sexpr::cons((*car).into(), (*cdr).into()),
        }
    }
}

enum Task<'a> {
    PrintObject(&'a Object),
    PrintStr(&'static str),
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut stack: Vec<Task> = Vec::new();
        stack.push(Task::PrintObject(self));

        while let Some(task) = stack.pop() {
            match task {
                Task::PrintObject(obj) => match obj {
                    Object::Atom(name) => write!(f, "{name}")?,
                    Object::Closure(body, _env) => {
                        stack.push(Task::PrintStr(">"));
                        stack.push(Task::PrintObject(body));
                        stack.push(Task::PrintStr("CLOSURE<"));
                    }
                    Object::Primitive(_func) => {
                        stack.push(Task::PrintStr("PRIM"));
                    }
                    Object::Sexpr(sexpr) => write!(f, "{sexpr}")?,
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

fn env_find(env: &Object, key: &Object) -> Result<Object, String> {
    if !key.is_atom() {
        return Err("Expected key to be Object::Atom".into());
    }
    let mut kvs = env.clone();
    loop {
        match kvs {
            Object::Sexpr(sexpr) => match *sexpr {
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

fn env_define(env: Object, key: Object, value: Object) -> Object {
    Object::cons(Object::cons(key, value), env)
}

fn env_define_prim(
    env: Object,
    name: &str,
    func: fn(State, Object) -> Result<State, String>,
) -> Object {
    let key = Object::Atom(Atom::Name(name.into()));
    env_define(env, key, Object::Primitive(func))
}

#[derive(Debug, Clone, PartialEq)]
pub struct State {
    pub stack: Object,
    pub env: Object,
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
            stack: Object::nil(),
            env: Object::default_env(),
        }
    }

    //////////         Value Stack Operations         //////////

    fn push(self, object: Object) -> State {
        State {
            stack: Object::cons(object, self.stack),
            env: self.env,
        }
    }

    fn peek(self) -> Result<Object, String> {
        match self.stack {
            Object::Sexpr(sexpr) => match *sexpr {
                Sexpr::Nil => Err("Stack is Nil".into()),
                Sexpr::Single(obj) => Ok(obj),
                Sexpr::Pair(car, cdr) => Ok((*car).into()),
            },
            _ => Err("Stack should be an Object::Sexpr".into()),
        }
    }

    fn pop(self) -> Result<(Object, State), String> {
        match self.stack {
            Object::Sexpr(sexpr) => match *sexpr {
                Sexpr::Nil => Err("Stack is Nil".into()),
                Sexpr::Single(obj) => {
                    let state = State {
                        stack: Object::nil(),
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

    fn pop2(self) -> Result<((Object, Object), State), String> {
        let (a, state) = self.pop()?;
        let (b, state) = state.pop()?;
        Ok(((a, b), state))
    }

    //////////               Environment              //////////

    fn with_env(self, env: Object) -> State {
        State {
            stack: self.stack,
            env: env,
        }
    }

    //////////                  Eval                  //////////

    fn apply_primitive(self, primitive: fn(State, Object) -> Result<State, String>) -> State {
        let env = self.env.clone();
        match primitive(self.clone(), env) {
            Ok(state) => state,
            Err(err) => {
                eprintln!("ERROR: applying primitive: {err}");
                self
            }
        }
    }
    pub fn compute(self, program: Sexpr<Object>) -> State {
        let mut state = self;
        let mut comp = program;

        loop {
            match comp {
                Sexpr::Nil => return state,

                Sexpr::Pair(cmd_box, rest_box) => {
                    let rest = *rest_box;
                    let cmd_value: Object = (*cmd_box).into();

                    if let Object::Atom(Atom::Name(ref name)) = cmd_value {
                        if name == "quote" {
                            match rest {
                                Sexpr::Pair(quoted, tail) => {
                                    let quoted: Object = (*quoted).into();
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
                _ => panic!("compute expects a list"),
            }
        }
    }

    pub fn eval(self, expr: Object) -> State {
        match expr {
            Object::Atom(ref atom) => match atom {
                Atom::Name(_) => {
                    let value = env_find(&self.env, &expr);

                    match value {
                        Ok(Object::Primitive(func)) => self.apply_primitive(func),

                        Ok(Object::Closure(body, closure_env)) => {
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

            Object::Sexpr(ref _sexpr) => {
                let closure = Object::make_closure(expr, self.env.clone());
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

fn prim_push(state: State, env: Object) -> Result<State, String> {
    let (key, state) = state.pop()?;
    let value = env_find(&env, &key)?;
    Ok(state.push(value))
}

fn prim_pop(state: State, env: Object) -> Result<State, String> {
    let ((key, value), state) = state.pop2()?;
    let env = env_define(env, key, value);
    Ok(State {
        stack: state.stack,
        env: env,
    })
}

fn prim_eq(state: State, _env: Object) -> Result<State, String> {
    let ((a, b), state) = state.pop2()?;
    let result = if a == b {
        Object::make_name("t")
    } else {
        Object::nil()
    };
    Ok(state.push(result))
}

fn prim_cons(state: State, _env: Object) -> Result<State, String> {
    let ((a, b), state) = state.pop2()?;
    let pair = Object::cons(a, b);
    Ok(state.push(pair))
}

fn prim_car(state: State, _env: Object) -> Result<State, String> {
    let (x, state) = state.pop()?;
    let car = Object::car(x)?;
    Ok(state.push(car))
}

fn prim_cdr(state: State, _env: Object) -> Result<State, String> {
    let (x, state) = state.pop()?;
    let cdr = Object::cdr(x)?;
    Ok(state.push(cdr))
}

fn prim_cswap(state: State, _env: Object) -> Result<State, String> {
    let (top, state) = state.pop()?;
    let new_state = if top == Object::make_name("t") {
        let ((a, b), state) = state.pop2()?;
        state.push(a).push(b)
    } else {
        state
    };
    Ok(new_state)
}

fn prim_print(state: State, _env: Object) -> Result<State, String> {
    let (top, state) = state.pop()?;
    println!("PRINT: {top}");
    Ok(state)
}

//////////            Extra Primitives            //////////.

fn prim_stack(state: State, _env: Object) -> Result<State, String> {
    let stack = state.stack.clone();
    Ok(state.push(stack))
}

fn prim_env(state: State, env: Object) -> Result<State, String> {
    Ok(state.push(env))
}

fn binary_num_op(a: Object, b: Object, func: fn(usize, usize) -> usize) -> Result<Object, String> {
    match (&a, &b) {
        (Object::Atom(Atom::Num(num_a)), Object::Atom(Atom::Num(num_b))) => {
            Ok(Object::Atom(Atom::Num(func(*num_a, *num_b))))
        }
        (_, _) => Err(format!(
            "Expected topmost two args to be Object::Atom(Atom::Num, a)re {}, {}",
            a, b
        )),
    }
}

fn prim_add(state: State, _env: Object) -> Result<State, String> {
    let ((a, b), state) = state.pop2()?;
    let result = binary_num_op(a, b, |a, b| a + b)?;
    Ok(state.push(result))
}

fn prim_sub(state: State, _env: Object) -> Result<State, String> {
    let ((a, b), state) = state.pop2()?;
    let result = binary_num_op(a, b, |a, b| b - a)?;
    Ok(state.push(result))
}

fn prim_mul(state: State, _env: Object) -> Result<State, String> {
    let ((a, b), state) = state.pop2()?;
    let result = binary_num_op(a, b, |a, b| a * b)?;
    Ok(state.push(result))
}

fn prim_div(state: State, _env: Object) -> Result<State, String> {
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

    #[test]
    fn test_display_pair() {
        let read: Sexpr<Object> = read(scan("(x y z)")).unwrap().into();
        assert_eq!(format!("{read}"), "(x y z)")
    }

    #[test]
    fn test_display_force() {
        let read: Sexpr<Object> = read(scan("($x x)")).unwrap().into();
        assert_eq!(format!("{read}"), "(quote x pop x)")
    }

    #[test]
    fn test_display_dup() {
        let read: Sexpr<Object> = read(scan("(($x ^x ^x) $dup)")).unwrap().into();
        assert_eq!(
            format!("{read}"),
            "((quote x pop quote x push quote x push) quote dup pop)"
        )
    }

    #[test]
    fn test_display_assoc() {
        let read: Sexpr<Object> = read(scan("((a b) (c d))")).unwrap().into();
        assert_eq!(format!("{read}"), "((a b) (c d))")
    }

    fn compute_new(input: &str) -> State {
        let mut state = State::new();
        let cmd: Sexpr<Object> = read(scan(input))
            .expect("Test input should be well-formed")
            .into();
        state.compute(cmd)
    }

    #[test]
    fn test_compute_put() {
        let state = compute_new("(1)");
        let (result, _state) = state.pop().expect("Should be Ok");
        assert_eq!(result, Object::Atom(Atom::Num(1)))
    }

    #[test]
    fn test_compute_swap() {
        let state = compute_new("(($x $y ^x ^y) $swap 1 2 swap stack)");
        let (result, _state) = state.pop().expect("Should be Ok");
        let expected = Object::cons(
            Object::Atom(Atom::Num(1)),
            Object::cons(Object::Atom(Atom::Num(2)), Object::nil()),
        );
        assert_eq!(result, expected)
    }
}
