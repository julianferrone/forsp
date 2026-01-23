use serde::{Deserialize, Serialize};
use std::collections::VecDeque;
use std::mem;

use crate::env::Env;
use crate::message::{Message, MessageType};
use crate::nonempty::NonEmpty;
use crate::primitive::Primitive;
use crate::sexpr::{Atom, Sexpr};

#[derive(Debug, Clone, PartialEq, Deserialize, Serialize)]
pub enum Value {
    Atom(Atom),
    Sexpr(Box<Sexpr<Value>>),
    Closure(Closure),
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
            Value::Closure(closure) => write!(f, "{closure}"),
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
        let sexprs: Vec<Box<Sexpr<Value>>> = value
            .0
            .into_iter()
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
    env: Env<Value>,
}

impl Closure {
    fn new(body: VecDeque<Instruction>, env: Env<Value>) -> Closure {
        Closure {
            body: body,
            env: env,
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
    MakeClosure(VecDeque<Instruction>),
    PushValue(Value),
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::ApplyPrimitive(primitive) => write!(f, "APPLY<{primitive}>"),
            Instruction::Call(name) => write!(f, "CALL<{name}>"),
            Instruction::MakeClosure(instructions) => {
                let instructions: Sexpr<Instruction> = Vec::from(instructions.clone()).into();
                write!(f, "MAKE_CLOSURE<{instructions}>")
            }
            Instruction::PushValue(value) => write!(f, "PUSH<{value}>"),
        }
    }
}

//////////             Core Primitives            //////////

fn prim_push(vm: &mut VM) -> VMStatus {
    (|| {
        let key = vm.pop_value()?;
        if let Value::Atom(Atom::Name(key)) = key {
            let value = vm.env().find(&key).cloned()?;
            vm.push_value(value);
            Ok(VMStatus::Continue)
        } else {
            Err("Push expects key to be Value::Atom(Atom::Name)".into())
        }
    })()
    .unwrap_or_else(|err: String| VMStatus::Invalid(format!("cons failed: {err}")))
}

fn prim_pop(vm: &mut VM) -> VMStatus {
    (|| {
        let (key, value) = vm.pop2_values()?;
        if let Value::Atom(Atom::Name(key)) = key {
            vm.env_mut().define_mut(key, value);
            Ok(VMStatus::Continue)
        } else {
            Err("Pop expects key to be Value::Atom(Atom::Name)".into())
        }
    })()
    .unwrap_or_else(|err: String| VMStatus::Invalid(format!("pop failed: {err}")))
}

fn prim_eq(vm: &mut VM) -> VMStatus {
    (|| {
        let (a, b) = vm.pop2_values()?;
        let result = if a == b {
            Value::make_name("t")
        } else {
            Value::nil()
        };
        vm.push_value(result);
        Ok(VMStatus::Continue)
    })()
    .unwrap_or_else(|err: String| VMStatus::Invalid(format!("eq failed: {err}")))
}

fn prim_cons(vm: &mut VM) -> VMStatus {
    (|| {
        let (a, b) = vm.pop2_values()?;
        let pair = Value::cons(a, b);
        vm.push_value(pair);
        Ok(VMStatus::Continue)
    })()
    .unwrap_or_else(|err: String| VMStatus::Invalid(format!("cons failed: {err}")))
}

fn prim_car(vm: &mut VM) -> VMStatus {
    (|| {
        let x = vm.pop_value()?;
        let car = Value::car(x)?;
        vm.push_value(car);
        Ok(VMStatus::Continue)
    })()
    .unwrap_or_else(|err: String| VMStatus::Invalid(format!("car failed: {err}")))
}

fn prim_cdr(vm: &mut VM) -> VMStatus {
    (|| {
        let x = vm.pop_value()?;
        let cdr = Value::cdr(x)?;
        vm.push_value(cdr);
        Ok(VMStatus::Continue)
    })()
    .unwrap_or_else(|err: String| VMStatus::Invalid(format!("cdr failed: {err}")))
}

fn prim_cswap(vm: &mut VM) -> VMStatus {
    (|| {
        let top = vm.pop_value()?;
        if top == Value::make_name("t") {
            let (a, b) = vm.pop2_values()?;
            vm.push_value(a);
            vm.push_value(b);
        };
        Ok(VMStatus::Continue)
    })()
    .unwrap_or_else(|err: String| VMStatus::Invalid(format!("cswap failed: {err}")))
}

fn prim_print(vm: &mut VM) -> VMStatus {
    match vm.pop_value() {
        Ok(value) => {
            vm.push_message(Message {
                typ: MessageType::Output,
                msg: value.to_string(),
            });
            VMStatus::Suspend
        }
        Err(err) => VMStatus::Invalid(format!("print failed: {err}")),
    }
}

const HELP: &str = include_str!("help.txt");

fn prim_help(vm: &mut VM) -> VMStatus {
    let help_messages = HELP
        .lines()
        .map(|line| Message {
            typ: MessageType::Output,
            msg: line.to_owned(),
        })
        .collect::<Vec<Message>>();
    vm.messages.extend(help_messages);
    VMStatus::Continue
}

//////////            Extra Primitives            //////////.

fn prim_stack(vm: &mut VM) -> VMStatus {
    let stack = vm.data.clone();
    vm.push_value(Value::Sexpr(Box::new(stack.into())));
    VMStatus::Continue
}

fn prim_env(vm: &mut VM) -> VMStatus {
    let env: Sexpr<Value> = vm.env().clone().into();
    vm.push_value(env.into());
    VMStatus::Continue
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

fn prim_add(vm: &mut VM) -> VMStatus {
    let result: Result<Value, String> = vm
        .pop2_values()
        .and_then(|(a, b)| binary_num_op(a, b, |a, b| b + a));

    match result {
        Ok(value) => {
            vm.push_value(value);
            VMStatus::Continue
        }
        Err(err) => VMStatus::Invalid(err),
    }
}

fn prim_sub(vm: &mut VM) -> VMStatus {
    let result: Result<Value, String> = vm
        .pop2_values()
        .and_then(|(a, b)| binary_num_op(a, b, |a, b| b - a));

    match result {
        Ok(value) => {
            vm.push_value(value);
            VMStatus::Continue
        }
        Err(err) => VMStatus::Invalid(err),
    }
}

fn prim_mul(vm: &mut VM) -> VMStatus {
    let result: Result<Value, String> = vm
        .pop2_values()
        .and_then(|(a, b)| binary_num_op(a, b, |a, b| b * a));

    match result {
        Ok(value) => {
            vm.push_value(value);
            VMStatus::Continue
        }
        Err(err) => VMStatus::Invalid(err),
    }
}

fn prim_div(vm: &mut VM) -> VMStatus {
    let result: Result<Value, String> = vm
        .pop2_values()
        .and_then(|(a, b)| binary_num_op(a, b, |a, b| b / a));

    match result {
        Ok(value) => {
            vm.push_value(value);
            VMStatus::Continue
        }
        Err(err) => VMStatus::Invalid(err),
    }
}

fn get_primitive_function(primitive: &Primitive) -> fn(&mut VM) -> VMStatus {
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
    messages: Vec<Message>,
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
    Invalid(String),
}

impl VM {
    pub fn new() -> VM {
        let base_closure = Closure::new(VecDeque::new(), Env::empty());
        VM {
            call_stack: NonEmpty::singleton(base_closure),
            data: vec![],
            messages: vec![],
        }
    }

    ////// Operations on data stack //////

    fn push_value(&mut self, value: Value) {
        self.data.push(value);
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

    ////// Operations on messages //////

    fn push_message(&mut self, message: Message) {
        self.messages.push(message);
    }

    pub fn flush_messages(&mut self) -> Vec<Message> {
        mem::take(&mut self.messages)
    }

    fn print_messages(&mut self) {
        let messages = self.flush_messages();
        for message in messages {
            Message::print(&message);
        }
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

    fn pop_closure(&mut self) -> Option<Closure> {
        self.call_stack.pop()
    }

    fn current_instruction(&self) -> Option<&Instruction> {
        self.current_closure().current_instruction()
    }

    fn pop_instruction(&mut self) -> Option<Instruction> {
        let mut instruction = self.current_closure_mut().pop_instruction();
        while let None = instruction {
            match self.pop_closure() {
                Some(_) => {
                    instruction = self.current_closure_mut().pop_instruction();
                },
                None => return None,
            }
        }
        instruction
    }

    pub fn set_instructions(&mut self, instructions: VecDeque<Instruction>) {
        self.current_closure_mut().body = instructions;
    }

    ////// Evaluation //////

    fn apply_primitive(&mut self, primitive: &Primitive) -> VMStatus {
        let func = get_primitive_function(primitive);
        func(self)
    }

    fn eval_instruction(&mut self, instruction: Instruction) -> VMStatus {
        match instruction {
            Instruction::ApplyPrimitive(prim) => self.apply_primitive(&prim),
            Instruction::Call(name) => {
                let value = self.env().find(&name).cloned();
                match value {
                    Ok(value) => {
                        match value {
                            Value::Atom(_) | Value::Sexpr(_) => self.push_value(value),
                            Value::Closure(closure) => self.push_closure(closure),
                        };
                        VMStatus::Continue
                    }
                    Err(err) => VMStatus::Invalid(err),
                }
            }
            Instruction::MakeClosure(instructions) => {
                let closure = self.make_closure(instructions);
                self.push_value(Value::Closure(closure));
                VMStatus::Continue
            }
            Instruction::PushValue(value) => {
                self.push_value(value);
                VMStatus::Continue
            }
        }
    }

    fn step(&mut self) -> VMStatus {
        let instruction: Option<Instruction> = self.pop_instruction();
        match instruction {
            Some(instruction) => self.eval_instruction(instruction),
            None => VMStatus::RequestInput,
        }
    }

    pub fn step_until_yield(&mut self) -> VMStatus {
        let mut status = VMStatus::Continue;
        while let VMStatus::Continue = status {
            status = self.step();
        }
        if let VMStatus::Invalid(ref err_msg) = status {
            self.push_message(Message::msg_error(err_msg));
        };
        status
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn push_value() {
        let expected = Value::Atom(Atom::Num(1));
        let mut vm = VM::new();
        let _ = vm.eval_instruction(Instruction::PushValue(expected.clone()));
        let result = vm.pop_value().expect("Should be Ok");
        assert_eq!(result, expected)
    }

    #[test]
    fn primitive_instruction_mul() {
        let two = Value::Atom(Atom::Num(2));
        let three = Value::Atom(Atom::Num(3));
        let six = Value::Atom(Atom::Num(6));
        let mut vm = VM::new();
        let instructions = vec![
            Instruction::PushValue(two),
            Instruction::PushValue(three),
            Instruction::ApplyPrimitive(Primitive::Mul),
        ];
        for instruction in instructions {
            let _ = vm.eval_instruction(instruction);
        }
        let result = vm.pop_value().expect("Should be Ok");
        assert_eq!(result, six)
    }

    #[test]
    fn make_and_call_dup() {
        let mut vm = VM::new();

        let dup_instructions: VecDeque<Instruction> = VecDeque::from([
            Instruction::PushValue(Value::Atom(Atom::Name("x".into()))),
            Instruction::ApplyPrimitive(Primitive::Pop),
            Instruction::PushValue(Value::Atom(Atom::Name("x".into()))),
            Instruction::ApplyPrimitive(Primitive::Push),
            Instruction::PushValue(Value::Atom(Atom::Name("x".into()))),
            Instruction::ApplyPrimitive(Primitive::Push),
        ]);

        let one = Value::Atom(Atom::Num(1));
        let instructions: VecDeque<Instruction> = VecDeque::from([
            Instruction::MakeClosure(dup_instructions),
            Instruction::PushValue(Value::Atom(Atom::Name("dup".into()))),
            Instruction::ApplyPrimitive(Primitive::Pop),
            Instruction::PushValue(one.clone()),
            Instruction::Call("dup".to_owned()),
        ]);
        vm.set_instructions(instructions);
        vm.step_until_yield();
        let first = vm.pop_value().expect("Should be Ok");
        assert_eq!(first, one.clone());
        let second = vm.pop_value().expect("Should be Ok");
        assert_eq!(second, one.clone());
        assert!(vm.call_stack.rest_is_empty(), "Call stack should be empty after running all instructions");
    }
}
