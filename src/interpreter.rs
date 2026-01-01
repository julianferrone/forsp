use crate::parser::Sexpr;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Nil,
    Atom(String),
    Num(usize),
    Pair(Box<Object>, Box<Object>),
    Closure(Box<Object>, Box<Object>),
    Primitive(fn(State) -> State),
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

fn env_define_prim(env: Object, name: &str, func: fn(State) -> State) -> Object {
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

    fn pop(self) -> (Option<Object>, State) {
        match self.stack {
            Object::Pair(car, cdr) => {
                let top = Some(*car);
                let state = State {
                    interned_atoms: self.interned_atoms,
                    stack: *cdr,
                    env: self.env,
                };
                (top, state)
            }
            Object::Nil => (None, self),
            _ => panic!("Stack should only be a Pair or Nil"),
        }
    }

    //////////                  Eval                  //////////
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
                        state = state.eval(*cmd);
                    }
                }
                _ => unreachable!("Closure bodies should only be Nil or Pair"),
            }
        }
    }

    fn eval(self, expr: Object) -> State {
        match expr {
            Object::Nil | Object::Pair(_, _) => {
                let env = self.env.clone();
                self.push(Object::make_closure(expr, env))
            }
            Object::Atom(_) => {
                let value = env_find(&self.env, expr);
                match value {
                    Ok(_) => todo!(),
                    Err(_) => self,
                }
            }
            Object::Num(_) => self.push(expr),
            Object::Closure(body, env) => self.compute(*body, *env),
            Object::Primitive(func) => func(self),
        }
    }
}
