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

impl Closure {
    fn new(body: VecDeque<Instruction>, env: Env<Value>) -> Closure {
        Closure {
            body: body,
            env: env
        }
    }

    fn current_instruction(&self) -> Option<&Instruction> {
        self.body.front()
    }

    fn pop_instruction(&mut self) -> Option<Instruction> {
        self.body.pop_front()
    }
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


fn get_primitive_function(primitive: &Primitive) -> fn(&mut VM) -> Result<(), String> {
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

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub struct VM {
    call_stack: NonEmpty<Closure>,
    data: Vec<Value>,
    messages: Vec<Message>
}

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum VMStatus {
    // VM can continue 
    Continue,
    // Waiting for web worker to print message
    Suspend,       
    // Waiting for instructions from user
    RequestInput,  
    // User input invalid instructions -- reset to previous state
    InvalidInput, 
}

impl VM {
    fn new() -> VM {
        let base_closure = Closure::new(VecDeque::new(), Env::empty());
        VM {
            call_stack: NonEmpty::singleton(base_closure),
            data: vec![],
            messages: vec![]
        }
    }
    
    ////// Operations on data stack //////
    
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

    // First item = top of stack
    // Second item = 2nd item of stack
    fn pop2_values(&mut self) -> Result<(Value, Value), String> {
        let a = self.pop_value()?;
        let b = self.pop_value()?;
        Ok((a, b))
    }

    ////// Operations on current environment //////

    fn env(&self) -> &Env<Value> {
        &self.call_stack.last().env
    }

    fn env_mut(&mut self) -> &mut Env<Value> {
        &mut self.call_stack.last_mut().env
    }

    fn make_closure(&self, instructions: VecDeque<Instruction>) -> Closure {
        let env = self.env().clone();
        Closure::new(instructions, env)
    }

    fn current_closure(&self) -> &Closure {
        self.call_stack.last()
    }

    fn current_closure_mut(&mut self) -> &mut Closure {
        self.call_stack.last_mut()
    }

    fn push_closure(&mut self, closure: Closure) {
        self.call_stack.push(closure)
    }

    fn current_instruction(&self) -> Option<&Instruction> {
        self.current_closure().current_instruction()
    }

    fn pop_instruction(&mut self) -> Option<Instruction> {
        self.current_closure_mut().pop_instruction()
    }

    ////// Evaluation //////

    fn eval_instruction(&mut self, instruction: Instruction) -> Result<(), String> {
        match instruction {
            Instruction::ApplyPrimitive(prim) => self.apply_primitive(&prim),
            Instruction::Call(name) => {
                let value: Value = self.env().find(&name)?.clone();
                match value {
                    Value::Atom(_) | Value::Sexpr(_) => {
                        self.push_value(value);
                        Ok(())
                    },
                    Value::Closure(closure) => {
                        self.push_closure(closure);
                        Ok(())
                    }
                }
            },
            Instruction::PushValue(value) => {
                self.push_value(value);
                Ok(())
            },
        }
    }

    fn step(&mut self) -> Result<(), String> {
        let instruction: Instruction = self.pop_instruction()
            .ok_or("No more instructions to step through".to_owned())?;
        self.eval_instruction(instruction)
    }
}

impl ApplyPrimitiveMut for VM {
    fn apply_primitive(&mut self, primitive: &Primitive) -> Result<(), String> {
        let func = get_primitive_function(primitive);
        func(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn run_add_value_instruction() {
        let expected = Value::Atom(Atom::Num(1));
        let mut vm = VM::new();
        let _ = vm.eval_instruction(Instruction::PushValue(expected.clone()));
        let result = vm.pop_value().expect("Should be Ok");
        assert_eq!(result, expected)
    }

    #[test]
    fn run_primitive_instruction_mul() {
        let two = Value::Atom(Atom::Num(2));
        let three = Value::Atom(Atom::Num(3));
        let six = Value::Atom(Atom::Num(6));
        let mut vm = VM::new();
        let instructions = vec![
            Instruction::PushValue(two),
            Instruction::PushValue(three),
            Instruction::ApplyPrimitive(Primitive::Mul)
        ];
        for instruction in instructions {
            let _ = vm.eval_instruction(instruction);
        }
        let result = vm.pop_value().expect("Should be Ok");
        assert_eq!(result, six)
    }

    // #[test]
    // fn run_call_instruction() {
    //     let two = Value::Atom(Atom::Num(2));
    //     let three = Value::Atom(Atom::Num(3));
    //     let five = Value::Atom(Atom::Num(5));
    //     let mut vm = VM::new();
    //     let _ = vm.eval_instruction(Instruction::PushValue(two));
    //     let _ = vm.eval_instruction(Instruction::PushValue(three));
    //     let _ = vm.eval_instruction(Instruction::Call("+".to_owned()));
    //     let result = vm.pop_value().expect("Should be Ok");
    //     assert_eq!(result, five)
    // }
}
