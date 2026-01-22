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
    fn apply_primitive(&mut self, primitive: &Primitive) -> Result<(), String>;
}

impl std::fmt::Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Primitive::Push => write!(f, "push"),
            Primitive::Pop => write!(f, "pop"),
            Primitive::Cons => write!(f, "cons"),
            Primitive::Car => write!(f, "car"),
            Primitive::Cdr => write!(f, "cdr"),
            Primitive::Equals => write!(f, "eq"),
            Primitive::Cswap => write!(f, "cswap"),
            Primitive::Print => write!(f, "print"),
            Primitive::Stack => write!(f, "stack"),
            Primitive::Env => write!(f, "env"),
            Primitive::Add => write!(f, "+"),
            Primitive::Sub => write!(f, "-"),
            Primitive::Mul => write!(f, "*"),
            Primitive::Div => write!(f, "/"),
            Primitive::Help => write!(f, "help"),
        }
    }
}

pub fn try_parse(s: &str) -> Option<Primitive> {
    match s {
            "push" => Some(Primitive::Push),
            "pop" => Some(Primitive::Pop),
            "cons" => Some(Primitive::Cons),
            "car" => Some(Primitive::Car),
            "cdr" => Some(Primitive::Cdr),
            "eq" => Some(Primitive::Equals),
            "cswap" => Some(Primitive::Cswap),
            "print" => Some(Primitive::Print),
            "stack" => Some(Primitive::Stack),
            "env" => Some(Primitive::Env),
            "+" => Some(Primitive::Add),
            "-" => Some(Primitive::Sub),
            "*" => Some(Primitive::Mul),
            "/" => Some(Primitive::Div),
            "help" => Some(Primitive::Help),
            _ => None
    }
}
