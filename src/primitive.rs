use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum Primitive {
    Push,
    Pop,
    Equals,
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

pub trait ApplyPrimitive {
    fn apply_primitive(self, primitive: Primitive) -> Self;
}

pub trait ApplyPrimitiveMut {
    fn apply_primitive(&mut self, primitive: Primitive);
}
