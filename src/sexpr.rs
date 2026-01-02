////////////////////////////////////////////////////////////
//                          Atoms                         //
////////////////////////////////////////////////////////////

use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    Name(String),
    Num(usize),
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Name(name) => write!(f, "{}", name),
            Atom::Num(num) => write!(f, "{}", num),
        }
    }
}

////////////////////////////////////////////////////////////
//                      S-Expressions                     //
////////////////////////////////////////////////////////////

#[derive(Debug, PartialEq, Clone)]
pub enum Sexpr<T> {
    Nil,
    Single(T),
    Pair(Box<Sexpr<T>>, Box<Sexpr<T>>),
}

impl<T> Sexpr<T> {
    pub fn cons(car: Sexpr<T>, cdr: Sexpr<T>) -> Sexpr<T> {
        Sexpr::Pair(Box::new(car), Box::new(cdr))
    }

    pub fn reverse_list(self) -> Sexpr<T> {
        let mut list = self;
        let mut result = Sexpr::Nil;

        while let Sexpr::Pair(car, cdr) = list {
            result = Sexpr::cons(*car, result);
            list = *cdr;
        }
        result
    }
}

impl<T: Clone> Sexpr<T> {
    pub fn car(sexpr: &Sexpr<T>) -> Result<Sexpr<T>, String> {
        match sexpr {
            Sexpr::Pair(car, _cdr) => Ok((**car).clone()),
            _ => Err("car expects Sexpr::Pair".into()),
        }
    }

    pub fn cdr(sexpr: &Sexpr<T>) -> Result<Sexpr<T>, String> {
        match sexpr {
            Sexpr::Pair(_car, cdr) => Ok((**cdr).clone()),
            _ => Err("cdr expects Sexpr::Pair".into()),
        }
    }
}

enum Task<'a, T> {
    PrintSexpr(&'a Sexpr<T>),
    PrintStr(&'static str),
}

impl<T: Display> Display for Sexpr<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut stack: Vec<Task<T>> = Vec::new();
        stack.push(Task::PrintSexpr(self));

        while let Some(task) = stack.pop() {
            match task {
                Task::PrintSexpr(obj) => match obj {
                    Sexpr::Nil => write!(f, "()")?,
                    Sexpr::Single(inner) => write!(f, "{}", inner)?,
                    Sexpr::Pair(_car, _cdr) => {
                        stack.push(Task::PrintStr(")"));

                        let mut cur = obj;
                        let mut elems: Vec<&Sexpr<T>> = Vec::new();

                        loop {
                            match cur {
                                Sexpr::Pair(car, cdr) => {
                                    elems.push(car);
                                    cur = cdr;
                                }
                                Sexpr::Nil => break,
                                tail => {
                                    stack.push(Task::PrintStr(" . "));
                                    stack.push(Task::PrintSexpr(&tail));
                                    break;
                                }
                            }
                        }

                        for (i, e) in elems.iter().rev().enumerate() {
                            if i > 0 {
                                stack.push(Task::PrintStr(" "));
                            }
                            stack.push(Task::PrintSexpr(e));
                        }

                        stack.push(Task::PrintStr("("));
                    }
                },
                Task::PrintStr(s) => write!(f, "{s}")?,
            }
        }
        Ok(())
    }
}
