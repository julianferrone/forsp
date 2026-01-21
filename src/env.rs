use std::collections::HashMap;
use serde::{Deserialize, Serialize};

use crate::primitive::Primitive;
use crate::sexpr::{Sexpr, Atom};

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Env<T>(pub HashMap<String, T>);

impl<T: std::convert::From<Primitive>> Env<T> {
    pub fn new() -> Env<T> {
        let mut env = Env(HashMap::new());
        
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

    pub fn find(self: &Env<T>, key: &str) -> Result<&T, String> {
        self.0
            .get(key)
            .ok_or("Could not find value for key".to_owned())
    }

    pub fn define_mut(self: &mut Env<T>, key: impl Into<String>, value: T) {
        self.0.insert(key.into(), value);
    }

    pub fn define(self: Env<T>, key: impl Into<String>, value: T) -> Env<T> {
        let mut env = self;
        env.define_mut(key, value);
        env
    }

    pub fn define_prim_mut(
        self: &mut Env<T>, 
        name: impl Into<String>, 
        prim: impl Into<T> 
    ) {
        self.define_mut(name.into(), prim.into())
    }

    pub fn define_prim(
        self: Env<T>, 
        name: impl Into<String>, 
        prim: impl Into<T>
    ) -> Env<T> {
        self.define(name.into(), prim.into())
    }
}

