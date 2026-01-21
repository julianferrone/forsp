use std::collections::HashMap;
use serde::{Deserialize, Serialize};

use crate::interpreter::Value;
use crate::primitive::Primitive;
use crate::sexpr::{Sexpr, Atom};

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Env(HashMap<String, Value>);

impl Env {
    pub fn new() -> Env {
        let mut env: Env = Env(HashMap::new());
        
        env.define_prim_mut("push", Primitive::Push);
        env.define_prim_mut("pop", Primitive::Pop);
        env.define_prim_mut("cons", Primitive::Cons);
        env.define_prim_mut("car", Primitive::Car);
        env.define_prim_mut("cdr", Primitive::Cdr);
        env.define_prim_mut("eq", Primitive::Equals);
        env.define_prim_mut("cswap", Primitive::Cswap);
        env.define_prim_mut("print", Primitive::Print);
        env.define_prim_mut("stack", Primitive::Stack);
        env.define_prim_mut("env", Primitive::Env);
        env.define_prim_mut("+", Primitive::Add);
        env.define_prim_mut("-", Primitive::Sub);
        env.define_prim_mut("*", Primitive::Mul);
        env.define_prim_mut("/", Primitive::Div);
        env.define_prim_mut("help", Primitive::Help);
        
        env
    }

    pub fn find(self: &Env, key: &Value) -> Result<Value, String> {
        match key {
            Value::Atom(Atom::Name(name)) => {
                self.0
                    .get(name)
                    .ok_or("Could not find value for key".to_owned())
                    .cloned()
            },
            _other => Err("Expected Key to be Value::Atom(Atom::Name)".into())
        }
    }

    pub fn define_mut(self: &mut Env, key: impl Into<String>, value: Value) {
        self.0.insert(key.into(), value);
    }

    pub fn define(self: Env, key: impl Into<String>, value: Value) -> Env {
        let mut env = self;
        env.define_mut(key, value);
        env
    }

    pub fn define_prim_mut(
        self: &mut Env, 
        name: impl Into<String>, 
        prim: Primitive
    ) {
        self.define_mut(name.into(), Value::Primitive(prim))
    }

    pub fn define_prim(self: Env, name: impl Into<String>, prim: Primitive) -> Env {
        self.define(name.into(), Value::Primitive(prim))
    }

    pub fn to_value(self: &Env) -> Value {
        let sexprs: Vec<Box<Sexpr<Value>>> = self.0.iter()
            .map(|(key, value)| {
                let key = Box::new(Sexpr::Single(Value::make_name(key)));
                let value = Box::new(Sexpr::Single(value.clone()));
                let pair = Sexpr::List(vec![key, value]);
                Box::new(pair)
            })
            .collect();
        Value::Sexpr(Box::new(Sexpr::List(sexprs)))
    }
}
