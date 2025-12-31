use crate::parser::Sexpr;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Nil,
    Atom(String),
    Num(usize),
    Pair(Box<Object>, Box<Object>),
    Closure(Box<Object>, Box<Object>),
    Primitive(fn(Object) -> Object),
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

struct State {
    interned_atoms: Object,
    stack: Object,
    env: Object,
}

//////////               Environment              //////////

impl State {
    fn env_find(&self, key: Object) -> Result<Object, String> {
        if !key.is_atom() {
            return Err("Expected key to be Object::Atom".into());
        }
        let mut kvs = self.env.clone();
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

    fn env_define(self, key: Object, value: Object) -> State {
        let env = self.env.cons(value.cons(key));
        State {
            interned_atoms: self.interned_atoms,
            stack: self.stack,
            env: env,
        }
    }

    fn env_define_prim(self, name: &str, func: fn(Object) -> Object) -> State {
        return self.env_define(Object::Atom(name.into()), Object::Primitive(func));
    }
}
