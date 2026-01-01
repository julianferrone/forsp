use crate::parser::Sexpr;

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
                                    stack.push(Task::PrintStr(" . "));
                                    stack.push(Task::PrintObject(tail));
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
                    Object::Closure(body, _env) => {
                        stack.push(Task::PrintStr("CLOSURE<"));
                        stack.push(Task::PrintObject(body));
                        stack.push(Task::PrintStr(">"));
                    }
                    Object::Primitive(_) => todo!(),
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

fn env_find(env: &Object, key: Object) -> Result<Object, String> {
    if !key.is_atom() {
        return Err("Expected key to be Object::Atom".into());
    }
    let mut kvs = env.clone();
    loop {
        match kvs {
            Object::Pair(kv, rest) => match &*kv {
                Object::Pair(k, v) => {
                    if **k == key {
                        return Ok(*v.to_owned());
                    };
                    kvs = *rest;
                }
                _ => return Err("Expected kv to be a pair".into()),
            },
            Object::Nil => return Err(format!("Failed to find key {key} in environment")),
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
    interned_atoms: Object,
    stack: Object,
    env: Object,
}

impl State {
    fn new() -> State {
        State {
            interned_atoms: Object::Nil,
            stack: Object::Nil,
            env: Object::Nil,
        }
    }

    //////////         Value Stack Operations         //////////

    fn push(self, object: Object) -> State {
        State {
            interned_atoms: self.interned_atoms,
            stack: self.stack.cons(object),
            env: self.env,
        }
    }

    fn pop(self) -> Result<(Object, State), String> {
        match self.stack {
            Object::Pair(car, cdr) => {
                let state = State {
                    interned_atoms: self.interned_atoms,
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

    //////////                  Eval                  //////////

    fn apply(self, primitive: fn(State, Object) -> Result<State, String>, env: Object) -> State {
        match primitive(self.clone(), env) {
            Ok(state) => state,
            Err(err) => {
                println!("Operation failed: {err}");
                self
            }
        }
    }

    fn compute(self, comp: Object, env: Object) -> State {
        let mut comp = comp.clone();
        let mut state = self.clone();
        loop {
            match comp {
                Object::Nil => return state,
                Object::Pair(cmd, cdr) => {
                    comp = *cdr;
                    if *cmd == Object::Atom("quote".into()) {
                        match comp {
                            Object::Pair(comp_car, comp_cdr) => {
                                state = state.push(*comp_car);
                                comp = *comp_cdr;
                            }
                            _ => unreachable!("Expected data following a quote form"),
                        }
                    } else {
                        state = state.eval(*cmd, env.clone());
                    }
                }
                _ => unreachable!("Closure bodies should only be Nil or Pair"),
            }
        }
    }

    fn eval(self, expr: Object, env: Object) -> State {
        match expr {
            Object::Nil | Object::Pair(_, _) => {
                let env = self.env.clone();
                let closure = Object::make_closure(expr.into(), env);
                self.push(closure)
            }
            Object::Atom(_) => {
                let atom = expr.into();
                let value = env_find(&env, atom);
                match value {
                    Ok(Object::Primitive(func)) => self.apply(func, env),
                    Ok(Object::Closure(body, env)) => self.compute(*body, *env),
                    Ok(object) => self.push(object),
                    Err(_) => self,
                }
            }
            object => self.push(object),
        }
    }
}

////////////////////////////////////////////////////////////
//                   Primitive Functions                  //
////////////////////////////////////////////////////////////

//////////             Core Primitives            //////////

fn prim_push(state: State, env: Object) -> Result<State, String> {
    let (key, state) = state.pop()?;
    let value = env_find(&env, key)?;
    Ok(state.push(value))
}

fn prim_pop(state: State, env: Object) -> Result<State, String> {
    let ((key, value), state) = state.pop2()?;
    let env = env_define(env, key, value);
    Ok(State {
        interned_atoms: state.interned_atoms,
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

//////////            Extra Primitives            //////////.
