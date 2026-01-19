use serde::{Deserialize, Serialize};

////////////////////////////////////////////////////////////
//                          Atoms                         //
////////////////////////////////////////////////////////////

use std::fmt::Display;

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
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

#[derive(Debug, PartialEq, Clone, Deserialize, Serialize)]
pub enum Sexpr<T> {
    Single(T),
    List(Vec<Box<Sexpr<T>>>)
}

impl<T> Sexpr<T> {
    pub fn nil() -> Sexpr<T> {
        Sexpr::List(vec![])
    }

    pub fn cons(item: Sexpr<T>, sexpr: Sexpr<T>) -> Sexpr<T> {
        match sexpr {
            Sexpr::Single(cdr) => Sexpr::List(vec![Box::new(sexpr), Box::new(item)]),
            Sexpr::List(sexprs) => {
                let mut sexprs = sexprs.clone();
                sexprs.push(Box::new(item));
                Sexpr::List(sexprs)
            }
        }
    }

    pub fn to_list(sexpr: Sexpr<T>) -> Sexpr<T> {
        match sexpr {
            Sexpr::Single(_) => Sexpr::List(vec![Box::new(sexpr)]),
            Sexpr::List(_) => sexpr
        }
    }

    pub fn reverse_list(self) -> Sexpr<T> {
        match self {
            Sexpr::Single(_) => self,
            Sexpr::List(sexprs) => Sexpr::List(sexprs.into_iter().rev().collect())
        }
    }

    pub fn extend(first: Sexpr<T>, second: Sexpr<T>) -> Sexpr<T> {
        let mut result: Vec<Box<Sexpr<T>>> = match second {
            Sexpr::Single(_) => vec![Box::new(second)],
            Sexpr::List(sexprs) => sexprs
        };
        
        match first {
            Sexpr::Single(_) => result = Sexpr::cons(first, result),
            Sexpr::List(first_sexprs) => {
                result.extend(first_sexprs);
            }
        };
        result
    }
}

impl<T: Clone> Sexpr<T> {
    pub fn car(sexpr: &Sexpr<T>) -> Result<Sexpr<T>, String> {
        match sexpr {
            Sexpr::List(items) => {
                items
                    .split_first()
                    .map(|(car, _cdr): (&T, &[T])| car.to_owned())
                    .ok_or("car expects non-empty Sexpr::List")
            },
            _ => Err("car expects Sexpr::List".into()),
        }
    }

    pub fn cdr(sexpr: &Sexpr<T>) -> Result<Sexpr<T>, String> {
        match sexpr {
            Sexpr::List(items) => {
                items
                    .split_first()
                    .map(|(_car, cdr): (&T, &[T])| cdr.to_owned())
                    .ok_or("car expects non-empty Sexpr::List")
            },
            _ => Err("car expects Sexpr::List".into()),
        }
    }
}

impl<T: Display> Display for Sexpr<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Sexpr::Single(item) => write!(f, "{}", item),
            Sexpr::List(items) => {
                let items: String = items.iter().map(|item: Box<Sexpr<T>>| item.to_string()).join(" ");
                write!(f, "({})", items);
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::sexpr::Sexpr;

    #[test]
    fn extend_first_single_works() {
        let first: Sexpr<usize> = Sexpr::Single(1);
        let second: Sexpr<usize> =
            Sexpr::cons(Sexpr::Single(2), Sexpr::cons(Sexpr::Single(3), Sexpr::nil()));

        let expected: Sexpr<usize> = Sexpr::cons(
            Sexpr::Single(1),
            Sexpr::cons(Sexpr::Single(2), Sexpr::cons(Sexpr::Single(3), Sexpr::nil())),
        );
        assert_eq!(Sexpr::extend(first, second), expected);
    }

    #[test]
    fn extend_second_nil_works() {
        let first: Sexpr<usize> = Sexpr::cons(Sexpr::Single(1), Sexpr::nil());
        let second: Sexpr<usize> = Sexpr::nil();
        assert_eq!(Sexpr::extend(first.clone(), second), first);
    }

//    #[test]
//    fn sexpr_into_vec_works() {
//        let sexpr: Sexpr<usize> = Sexpr::cons(
//            Sexpr::Single(1),
//            Sexpr::cons(Sexpr::Single(2), Sexpr::cons(Sexpr::Single(3), Sexpr::nil())),
//        );
//        let actual: Vec<usize> = sexpr.into();
//        let expected: Vec<usize> = vec![1, 2, 3];
//        assert_eq!(actual, expected);
//    }
//
//    #[test]
//    fn vec_into_sexpr_works() {
//        let vec: Vec<usize> = vec![1, 2, 3];
//        let actual: Sexpr<usize> = vec.into();
//        let expected: Sexpr<usize> = Sexpr::cons(
//            Sexpr::Single(1),
//            Sexpr::cons(Sexpr::Single(2), Sexpr::cons(Sexpr::Single(3), Sexpr::nil())),
//        );
//        assert_eq!(actual, expected);
//    }
//
//    #[test]
//    fn vec_into_sexpr_into_vec_works() {
//        let vec: Vec<usize> = vec![1, 2, 3];
//        let sexpr: Sexpr<usize> = vec.clone().into();
//        let vec2: Vec<usize> = sexpr.into();
//        assert_eq!(vec, vec2)
//    }
//
//    #[test]
//    fn sexpr_into_vec_into_sexpr_works() {
//        let sexpr: Sexpr<usize> = Sexpr::cons(
//            Sexpr::Single(1),
//            Sexpr::cons(Sexpr::Single(2), Sexpr::cons(Sexpr::Single(3), Sexpr::nil())),
//        );
//        let vec: Vec<usize> = sexpr.clone().into();
//        let sexpr2: Sexpr<usize> = vec.into();
//        assert_eq!(sexpr, sexpr2)
//    }
}
