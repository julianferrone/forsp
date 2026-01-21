use std::collections::{HashMap, VecDeque};
use serde::{Deserialize, Serialize};

use crate::env::Env;
use crate::message::{Message, MessageType};
use crate::nonempty::NonEmpty;
use crate::primitive::{Primitive, ApplyPrimitiveMut};
use crate::sexpr::{Atom, Sexpr};

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum Value {
    Atom(Atom),
    Sexpr(Box<Sexpr<Value>>),
    Closure(Closure)
}

impl Value {
    fn make_name(name: impl Into<String>) -> Value {
        Value::Atom(Atom::Name(name.into()))
    }

    fn cons(car: Value, cdr: Value) -> Value {
        let car: Sexpr<Value> = car.into();
        let cdr: Sexpr<Value> = cdr.into();
        let obj: Value = Sexpr::cons(car, cdr).into();
        obj
    }

    fn nil() -> Value {
        Sexpr::nil().into()
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

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Atom(atom) => write!(f, "{atom}"),
            Value::Sexpr(sexpr) => write!(f, "{sexpr}"),
            Value::Closure(closure) => write!(f, "{closure}")
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

impl From<Env<Value>> for Sexpr<Value> {
    fn from(value: Env<Value>) -> Sexpr<Value> {
        let sexprs: Vec<Box<Sexpr<Value>>> = value.0.into_iter()
            .map(|(key, value): (String, Value)| {
                let key = Box::new(Sexpr::Single(Value::make_name(key)));
                let value = Box::new(Sexpr::Single(value));
                let pair = Sexpr::List(vec![key, value]);
                Box::new(pair)
            })
            .collect();
        Sexpr::List(sexprs)
    }
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct Closure {
    body: VecDeque<Instruction>,
    env: Env<Value>
}

impl std::fmt::Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let body: Sexpr<Instruction> = Sexpr::from_vec(self.body.clone().into());
        let env: Sexpr<Value> = self.env.clone().into();
        write!(f, "CLOSURE<{body} {env}>")
    }
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum Instruction {
    ApplyPrimitive(Primitive),
    Call(String),
    PushValue(Value)
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::ApplyPrimitive(primitive) => write!(f, "APPLY<{primitive}>"),
            Instruction::Call(name) => write!(f, "CALL<{name}>"),
            Instruction::PushValue(value) => write!(f, "PUSH<{value}>")
        }
    }
}

//////////             Core Primitives            //////////

fn prim_push(vm: &mut VM) -> Result<(), String> {
    let key = vm.pop_value()?;
    if let Value::Atom(Atom::Name(key)) = key {
        let value = vm.env().find(&key).cloned()?;
        vm.push_value(value);
        Ok(())
    } else {
        Err("Push expects key to be Value::Atom(Atom::Name)".into())
    }
}

fn prim_pop(vm: &mut VM) -> Result<(), String> {
    let (key, value) = vm.pop2_values()?;
    if let Value::Atom(Atom::Name(key)) = key {
        vm.env_mut().define_mut(key, value);
        Ok(())
    } else {
        Err("Pop expects key to be Value::Atom(Atom::Name)".into())
    }
}

fn prim_eq(vm: &mut VM) -> Result<(), String> {
    let (a, b) = vm.pop2_values()?;
    let result = if a == b {
        Value::make_name("t")
    } else {
        Value::nil()
    };
    vm.push_value(result);
    Ok(())
}

fn prim_cons(vm: &mut VM) -> Result<(), String> {
    let (a, b) = vm.pop2_values()?;
    let pair = Value::cons(a, b);
    vm.push_value(pair);
    Ok(())
}

fn prim_car(vm: &mut VM) -> Result<(), String> {
    let x = vm.pop_value()?;
    let car = Value::car(x)?;
    vm.push_value(car);
    Ok(())
}

fn prim_cdr(vm: &mut VM) -> Result<(), String> {
    let x = vm.pop_value()?;
    let cdr = Value::cdr(x)?;
    vm.push_value(cdr);
    Ok(())
}

fn prim_cswap(vm: &mut VM) -> Result<(), String> {
    let top = vm.pop_value()?;
    if top == Value::make_name("t") {
        let (a, b) = vm.pop2_values()?;
        vm.push_value(a);
        vm.push_value(b);
    };
    Ok(())
}

fn prim_print(vm: &mut VM) -> Result<(), String> {
    let top = vm.pop_value()?;
    vm.messages.push(Message {
        typ: MessageType::Output,
        msg: top.to_string()
    });
    Ok(())
}

const HELP: &str = include_str!("help.txt");


fn prim_help(vm: &mut VM) -> Result<(), String> {
    let help_messages = HELP
        .lines()
        .map(|line| {
            Message {
                typ: MessageType::Output,
                msg: line.to_owned()
            }
        })
        .rev()
        .collect::<Vec<Message>>();
    vm.messages.extend(help_messages);
    Ok(())
}

//////////            Extra Primitives            //////////.

fn prim_stack(vm: &mut VM) -> Result<(), String> {
    let stack = vm.data.clone();
    vm.push_value(Value::Sexpr(Box::new(stack.into())));
    Ok(())
}

fn prim_env(vm: &mut VM) -> Result<(), String> {
    let env: Sexpr<Value> = vm.env().clone().into();
    vm.push_value(env.into());
    Ok(())
}

fn binary_num_op(a: Value, b: Value, func: fn(usize, usize) -> usize) -> Result<Value, String> {
    match (&a, &b) {
        (Value::Atom(Atom::Num(num_a)), Value::Atom(Atom::Num(num_b))) => {
            Ok(Value::Atom(Atom::Num(func(*num_a, *num_b))))
        }
        (_, _) => Err(format!(
            "Expected topmost two args to be Object::Atom(Atom::Num, are {a}, {b}",
        )),
    }
}

fn prim_add(vm: &mut VM) -> Result<(), String> {
    let (a, b) = vm.pop2_values()?;
    let result = binary_num_op(a, b, |a, b| a + b)?;
    vm.push_value(result);
    Ok(())
}

fn prim_sub(vm: &mut VM) -> Result<(), String> {
    let (a, b) = vm.pop2_values()?;
    let result = binary_num_op(a, b, |a, b| b - a)?;
    vm.push_value(result);
    Ok(())
}

fn prim_mul(vm: &mut VM) -> Result<(), String> {
    let (a, b) = vm.pop2_values()?;
    let result = binary_num_op(a, b, |a, b| a * b)?;
    vm.push_value(result);
    Ok(())
}

fn prim_div(vm: &mut VM) -> Result<(), String> {
    let (a, b) = vm.pop2_values()?;
    let result = binary_num_op(a, b, |a, b| b / a)?;
    vm.push_value(result);
    Ok(())
}


fn get_primitive_function(primitive: Primitive) -> fn(&mut VM) -> Result<(), String> {
    match primitive {
        Primitive::Push => prim_push,
        Primitive::Pop => prim_pop,
        Primitive::Equals => prim_eq,
        Primitive::Cons => prim_cons,
        Primitive::Car => prim_car,
        Primitive::Cdr => prim_cdr,
        Primitive::Cswap => prim_cswap,
        Primitive::Print => prim_print,
        Primitive::Help => prim_help,
        Primitive::Stack => prim_stack,
        Primitive::Env => prim_env,
        Primitive::Add => prim_add,
        Primitive::Sub => prim_sub,
        Primitive::Mul => prim_mul,
        Primitive::Div => prim_div,
    }
}

// VM

pub struct VM {
    call_stack: NonEmpty<Closure>,
    data: Vec<Value>,
    messages: Vec<Message>
}

impl VM {
    fn env(&self) -> &Env<Value> {
        &self.call_stack.last().env
    }

    fn env_mut(&mut self) -> &mut Env<Value> {
        &mut self.call_stack.last_mut().env
    }

    fn push_value(&mut self, value: Value) {
        self.data.push(value);
    }

    fn push_message(&mut self, message: Message) {
        self.messages.push(message);
    }

    fn pop_value(&mut self) -> Result<Value, String> {
        self.data
            .pop()
            .ok_or("pop_value expects data to be non-empty".into())
    }

    fn pop2_values(&mut self) -> Result<(Value, Value), String> {
        let a = self.pop_value()?;
        let b = self.pop_value()?;
        Ok((a, b))
    }
}

impl ApplyPrimitiveMut for VM {
    fn apply_primitive(&mut self, primitive: Primitive) -> Result<(), String> {
        let func = get_primitive_function(primitive);
        func(self)
    }
}

#[cfg(tests)]
mod tests {
    #[test]
    fn run_add_value_instruction() {
        let expected = Value::Atom(Atom::Num(1));
        let instruction = Instruction::AddValue(expected.clone());
        let vm = VM::new().eval_instruction(instruction);
        let (result, _vm) = vm.pop().expect("Should be Ok");
        assert_eq!(result, expected)
    }

    #[test]
    fn run_primitive_instruction_mul() {
        let two = Value::Atom(Atom::Num(2));
        let three = Value::Atom(Atom::Num(3));
        let six = Value::Atom(Atom::Num(6));
        let (result, _vm) = VM::new()
            .eval_instruction(Instruction::AddValue(two))
            .eval_instruction(Instruction::AddValue(three))
            .eval_instruction(Instruction::Primitive(Primitive::Mul))
            .pop()
            .expect("Should be Ok");
        assert_eq!(result, six)
    }

    #[test]
    fn run_call_instruction() {
        let two = Value::Atom(Atom::Num(2));
        let three = Value::Atom(Atom::Num(3));
        let five = Value::Atom(Atom::Num(5));
        let (result, _vm) = VM::new()
            .eval_instruction(Instruction::AddValue(two))
            .eval_instruction(Instruction::AddValue(three))
            .eval_instruction(Instruction::Call("+".to_owned()))
            .pop()
            .expect("Should be Ok");
        assert_eq!(result, five)
    }
}
