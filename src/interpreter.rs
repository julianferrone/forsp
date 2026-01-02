use std::fmt::{format, Display};

use crate::sexpr::Sexpr;

// TODO: Pull out Nil / Pair of SExpr into generic type SExpr<T>
// TODO: Pull out Atom as separate enum of Num(usize) | Name(String)
// then parser SExpr = SExpr<Atom>
// TODO: Pull out interpreter Object => SExpr<Atom | Closure | Primitive>

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Nil,
    Atom(String),
    Num(usize),
    Pair(Box<Object>, Box<Object>),
    Closure(Box<Object>, Box<Object>),
    Primitive(fn(State, Object) -> Result<State, String>),
}

impl Object {
    fn is_atom(&self) -> bool {
        match self {
            Object::Atom(_) => true,
            _ => false,
        }
    }

    fn is_pair(&self) -> bool {
        match self {
            Object::Pair(_, _) => true,
            _ => false,
        }
    }

    fn car(&self) -> Result<Object, String> {
        match self {
            Object::Pair(car, _) => Ok(*car.clone()),
            _ => Err("Expected pair to apply car() function".into()),
        }
    }

    fn cdr(&self) -> Result<Object, String> {
        match self {
            Object::Pair(_, cdr) => Ok(*cdr.clone()),
            _ => Err("Expected pair to apply cdr() function".into()),
        }
    }

    fn cons(self, car: Object) -> Object {
        Object::Pair(Box::new(car), Box::new(self))
    }

    fn make_closure(body: Object, env: Object) -> Object {
        Object::Closure(Box::new(body), Box::new(env))
    }

    fn default_env() -> Object {
        let env = Object::Nil;
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
}

impl From<Sexpr> for Object {
    fn from(value: Sexpr) -> Self {
        match value {
            Sexpr::Nil => Object::Nil,
            Sexpr::Atom(name) => Object::Atom(name),
            Sexpr::Num(num) => Object::Num(num),
            Sexpr::Pair(car, cdr) => Object::Pair(Box::new((*car).into()), Box::new((*cdr).into())),
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
                    Object::Nil => write!(f, "()")?,
                    Object::Atom(name) => write!(f, "{name}")?,
                    Object::Num(num) => write!(f, "{num}")?,
                    Object::Pair(_car, _cdr) => {
                        stack.push(Task::PrintStr(")"));

                        let mut cur = obj;
                        let mut elems: Vec<&Object> = Vec::new();

                        loop {
                            match cur {
                                Object::Pair(car, cdr) => {
                                    elems.push(car);
                                    cur = cdr;
                                }
                                Object::Nil => break,
                                tail => {
                                    stack.push(Task::PrintObject(tail));
                                    stack.push(Task::PrintStr(" . "));
                                    break;
                                }
                            }
                        }

                        for (i, e) in elems.iter().rev().enumerate() {
                            if i > 0 {
                                stack.push(Task::PrintStr(" "));
                            }
                            stack.push(Task::PrintObject(e));
                        }

                        stack.push(Task::PrintStr("("));
                    }
                    Object::Closure(body, env) => {
                        stack.push(Task::PrintStr(">"));
                        stack.push(Task::PrintObject(body));
                        stack.push(Task::PrintStr("CLOSURE<"));
                    }
                    Object::Primitive(_func) => {
                        stack.push(Task::PrintStr("PRIM"));
                    }
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
            Object::Pair(kv, rest) => match &*kv {
                Object::Pair(k, v) => {
                    if **k == *key {
                        return Ok(*v.to_owned());
                    };
                    kvs = *rest;
                }
                _ => return Err("Expected kv to be a pair".into()),
            },
            Object::Nil => return Err(format!("Failed to find key {key} in environment {env}")),
            _ => return Err("Expected env to be a pair".into()),
        }
    }
}

fn env_define(env: Object, key: Object, value: Object) -> Object {
    env.cons(value.cons(key))
}

fn env_define_prim(
    env: Object,
    name: &str,
    func: fn(State, Object) -> Result<State, String>,
) -> Object {
    env_define(env, Object::Atom(name.into()), Object::Primitive(func))
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
            stack: Object::Nil,
            env: Object::default_env(),
        }
    }

    //////////         Value Stack Operations         //////////

    fn push(self, object: Object) -> State {
        State {
            stack: self.stack.cons(object),
            env: self.env,
        }
    }

    fn pop(self) -> Result<(Object, State), String> {
        match self.stack {
            Object::Pair(car, cdr) => {
                let state = State {
                    stack: *cdr,
                    env: self.env,
                };
                Ok((*car, state))
            }
            Object::Nil => Err("Stack is Nil".into()),
            _ => Err("Stack should only be a Pair or Nil".into()),
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
    pub fn compute(self, program: Object) -> State {
        let mut state = self;
        let mut comp = program;

        loop {
            match comp {
                Object::Nil => return state,

                Object::Pair(cmd, rest) => match *cmd {
                    Object::Atom(ref a) if a == "quote" => match *rest {
                        Object::Pair(quoted, tail) => {
                            state = state.push(*quoted);
                            comp = *tail;
                        }
                        _ => panic!("quote expects one argument"),
                    },

                    _ => {
                        state = state.eval(*cmd);
                        comp = *rest;
                    }
                },

                _ => panic!("compute expects a list"),
            }
        }
    }

    pub fn eval(self, expr: Object) -> State {
        match expr {
            Object::Atom(_) => {
                let value = env_find(&self.env, &expr);

                match value {
                    Ok(Object::Primitive(func)) => self.apply_primitive(func),

                    Ok(Object::Closure(body, closure_env)) => {
                        let saved_env = self.env.clone();

                        let state = self.with_env(*closure_env).compute(*body);

                        state.with_env(saved_env)
                    }
                    Ok(object) => self.push(object),
                    Err(err) => {
                        eprintln!("ERROR: evaluating atom: {}", err);
                        self
                    }
                }
            }

            Object::Nil | Object::Pair(_, _) => {
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
        Object::Atom("t".into())
    } else {
        Object::Nil
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
    let car = x.car()?;
    Ok(state.push(car))
}

fn prim_cdr(state: State, _env: Object) -> Result<State, String> {
    let (x, state) = state.pop()?;
    let cdr = x.cdr()?;
    Ok(state.push(cdr))
}

fn prim_cswap(state: State, _env: Object) -> Result<State, String> {
    let (top, state) = state.pop()?;
    let new_state = if top == Object::Atom("t".into()) {
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
        (Object::Num(num_a), Object::Num(num_b)) => Ok(Object::Num(func(*num_a, *num_b))),
        (_, _) => Err(format!(
            "Expected topmost two args to be Object::Num, are {}, {}",
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
        let read: Object = read(scan("(x y z)")).unwrap().into();
        assert_eq!(format!("{read}"), "(x y z)")
    }

    #[test]
    fn test_display_force() {
        let read: Object = read(scan("($x x)")).unwrap().into();
        assert_eq!(format!("{read}"), "(quote x pop x)")
    }

    #[test]
    fn test_display_dup() {
        let read: Object = read(scan("(($x ^x ^x) $dup)")).unwrap().into();
        assert_eq!(
            format!("{read}"),
            "((quote x pop quote x push quote x push) quote dup pop)"
        )
    }

    #[test]
    fn test_display_assoc() {
        let read: Object = read(scan("((a b) (c d))")).unwrap().into();
        assert_eq!(format!("{read}"), "((a b) (c d))")
    }

    fn compute_new(input: &str) -> State {
        let mut state = State::new();
        let cmd: Object = read(scan(input))
            .expect("Test input should be well-formed")
            .into();
        state.compute(cmd)
    }

    #[test]
    fn test_compute_put() {
        let state = compute_new("(1)");
        let (result, _state) = state.pop().expect("Should be Ok");
        assert_eq!(result, Object::Num(1))
    }

    #[test]
    fn test_compute_swap() {
        let state = compute_new("(($x $y ^x ^y) $swap 1 2 swap stack)");
        let (result, _state) = state.pop().expect("Should be Ok");
        assert_eq!(
            result,
            Object::Nil.cons(Object::Num(2)).cons(Object::Num(1))
        )
    }
}
