////////////////////////////////////////////////////////////
//                          Atoms                         //
////////////////////////////////////////////////////////////

use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    Name(String),
    Num(usize),
}

impl Atom {
    pub fn name(name: &str) -> Atom {
        Atom::Name(name.into())
    }
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
        if let Sexpr::Nil = self {
            return self;
        }
        if let Sexpr::Single(_) = self {
            return self
        }
        let mut list = self;
        let mut result = Sexpr::Nil;
        while let Sexpr::Pair(car, cdr) = list {
            result = Sexpr::cons(*car, result);
            list = *cdr;
        }
        result
    }

    pub fn extend(first: Sexpr<T>, second: Sexpr<T>) -> Sexpr<T> {
        let mut first = first.reverse_list();
        let mut result = second;
        loop {
            match first {
                Sexpr::Nil => break,
                Sexpr::Single(_) => {
                    result = Sexpr::cons(first, result);
                    break;
                }
                Sexpr::Pair(car, cdr) => {
                    result = Sexpr::Pair(car, Box::new(result));
                    first = *cdr;
                }
            }
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

mod tests {
    use super::*;

    #[test]
    fn extend_first_single_works() {
        let first: Sexpr<usize> = Sexpr::Single(1);
        let second: Sexpr<usize> =
            Sexpr::cons(Sexpr::Single(2), Sexpr::cons(Sexpr::Single(3), Sexpr::Nil));

        let expected: Sexpr<usize> = Sexpr::cons(
            Sexpr::Single(1),
            Sexpr::cons(Sexpr::Single(2), Sexpr::cons(Sexpr::Single(3), Sexpr::Nil)),
        );
        assert_eq!(Sexpr::extend(first, second), expected);
    }

    #[test]
    fn extend_second_nil_works() {
        let first: Sexpr<usize> = Sexpr::cons(Sexpr::Single(1), Sexpr::Nil);
        let second: Sexpr<usize> = Sexpr::Nil;
        assert_eq!(Sexpr::extend(first.clone(), second), first);
    }
}
