use serde::{Deserialize, Serialize};

////////////////////////////////////////////////////////////
//                          Atoms                         //
////////////////////////////////////////////////////////////

use std::fmt::Display;

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Atom {
    Symbol(String),
    Num(i64),
}

impl Atom {
    pub fn symbol(symbol: &str) -> Atom {
        Atom::Symbol(symbol.into())
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Symbol(symbol) => write!(f, "{}", symbol),
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
    List(Vec<Box<Sexpr<T>>>),
}

impl<T> Sexpr<T> {
    pub fn nil() -> Sexpr<T> {
        Sexpr::List(vec![])
    }

    pub fn to_list(sexpr: Sexpr<T>) -> Sexpr<T> {
        match sexpr {
            Sexpr::Single(_) => Sexpr::List(vec![Box::new(sexpr)]),
            Sexpr::List(_) => sexpr,
        }
    }

    pub fn from_vec(vec: Vec<T>) -> Sexpr<T> {
        let items: Vec<Box<Sexpr<T>>> = vec
            .into_iter()
            .map(|item| Box::new(Sexpr::Single(item)))
            .collect();
        Sexpr::List(items)
    }

    pub fn reverse_list(self) -> Sexpr<T> {
        match self {
            Sexpr::Single(_) => self,
            Sexpr::List(sexprs) => Sexpr::List(sexprs.into_iter().rev().collect()),
        }
    }

    pub fn extend(first: Sexpr<T>, second: Sexpr<T>) -> Sexpr<T> {
        let mut result: Vec<Box<Sexpr<T>>> = match second {
            Sexpr::Single(_) => vec![Box::new(second)],
            Sexpr::List(sexprs) => sexprs,
        };

        match first {
            Sexpr::Single(_) => result.push(Box::new(first)),
            Sexpr::List(first_sexprs) => {
                result.extend(first_sexprs);
            }
        };
        Sexpr::List(result)
    }
}

impl<T: Clone> Sexpr<T> {
    pub fn cons(item: Sexpr<T>, sexpr: Sexpr<T>) -> Sexpr<T> {
        match sexpr {
            Sexpr::Single(_) => Sexpr::List(vec![Box::new(sexpr), Box::new(item)]),
            Sexpr::List(sexprs) => {
                let mut sexprs = sexprs.clone();
                sexprs.push(Box::new(item));
                Sexpr::List(sexprs)
            }
        }
    }

    pub fn car(sexpr: &Sexpr<T>) -> Result<Sexpr<T>, String> {
        match sexpr {
            Sexpr::List(items) => items
                .split_last()
                .map(|(car, _cdr)| *car.to_owned())
                .ok_or("car expects non-empty Sexpr::List".to_owned()),
            _ => Err("car expects Sexpr::List".into()),
        }
    }

    pub fn car_mut(sexpr: &mut Sexpr<T>) -> Result<Sexpr<T>, String> {
        match sexpr {
            Sexpr::List(items) => items
                .pop()
                .map(|boxed| *boxed)
                .ok_or("car_mut expects non-empty Sexpr::List".to_owned()),
            _ => Err("car_mut expects Sexpr::List".into()),
        }
    }

    pub fn cdr(sexpr: &Sexpr<T>) -> Result<Sexpr<T>, String> {
        match sexpr {
            Sexpr::List(items) => items
                .split_last()
                .map(|(_car, cdr)| {
                    let cdr: Vec<Box<Sexpr<T>>> = cdr.to_vec();
                    Sexpr::List(cdr)
                })
                .ok_or("car expects non-empty Sexpr::List".to_owned()),
            _ => Err("car expects Sexpr::List".into()),
        }
    }

    pub fn split(sexpr: &Sexpr<T>) -> Result<(Sexpr<T>, Sexpr<T>), String> {
        match sexpr {
            Sexpr::List(items) => items
                .split_last()
                .map(|(car, cdr)| {
                    let car: Sexpr<T> = *car.to_owned();
                    let cdr: Vec<Box<Sexpr<T>>> = cdr.to_vec();
                    (car, Sexpr::List(cdr))
                })
                .ok_or("split expects non-empty Sexpr::List".to_owned()),
            _ => Err("split expects Sexpr::List".into()),
        }
    }
}

impl<T: Display> Display for Sexpr<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Sexpr::Single(item) => write!(f, "{item}"),
            Sexpr::List(items) => {
                let items: String = items
                    .iter()
                    .rev()
                    .map(|item: &Box<Sexpr<T>>| item.to_string())
                    .collect::<Vec<String>>()
                    .join(" ");
                write!(f, "({items})")
            }
        }
    }
}

impl<T> From<Vec<T>> for Sexpr<T> {
    fn from(value: Vec<T>) -> Self {
        Sexpr::from_vec(value)
    }
}

#[cfg(test)]
mod tests {
    use crate::sexpr::Sexpr;

    #[test]
    fn extend_first_single() {
        let first: Sexpr<i64> = Sexpr::Single(1);
        let second: Sexpr<i64> = Sexpr::cons(
            Sexpr::Single(2),
            Sexpr::cons(Sexpr::Single(3), Sexpr::nil()),
        );

        let expected: Sexpr<i64> = Sexpr::cons(
            Sexpr::Single(1),
            Sexpr::cons(
                Sexpr::Single(2),
                Sexpr::cons(Sexpr::Single(3), Sexpr::nil()),
            ),
        );
        assert_eq!(Sexpr::extend(first, second), expected);
    }

    #[test]
    fn extend_second_nil() {
        let first: Sexpr<i64> = Sexpr::cons(Sexpr::Single(1), Sexpr::nil());
        let second: Sexpr<i64> = Sexpr::nil();
        assert_eq!(Sexpr::extend(first.clone(), second), first);
    }

    #[test]
    fn cons_then_car() {
        let first: Sexpr<i64> = Sexpr::Single(1);
        let rest: Sexpr<i64> = Sexpr::cons(
            Sexpr::Single(2),
            Sexpr::cons(Sexpr::Single(3), Sexpr::nil()),
        );
        let consed = Sexpr::cons(first.clone(), rest);
        assert_eq!(Sexpr::car(&consed).expect("Car should succeed"), first);
    }

    #[test]
    fn cons_then_cdr() {
        let first: Sexpr<i64> = Sexpr::Single(1);
        let rest: Sexpr<i64> = Sexpr::cons(
            Sexpr::Single(2),
            Sexpr::cons(Sexpr::Single(3), Sexpr::nil()),
        );
        let consed = Sexpr::cons(first, rest.clone());
        assert_eq!(Sexpr::cdr(&consed).expect("Car should succeed"), rest);
    }

    #[test]
    fn split_list() {
        let expected_first: Sexpr<i64> = Sexpr::Single(1);
        let expected_rest: Sexpr<i64> = Sexpr::cons(
            Sexpr::Single(2),
            Sexpr::cons(Sexpr::Single(3), Sexpr::nil()),
        );
        let list = Sexpr::cons(expected_first.clone(), expected_rest.clone());
        let (first, rest) = Sexpr::split(&list).expect("Should split");
        assert_eq!(first, expected_first);
        assert_eq!(rest, expected_rest);
    }

    #[test]
    fn vec_into_sexpr_works() {
        // We assume that the END of a Vec is like a Cons cell (since we use
        // push/pop on the vec when working with Sexpr::List).
        // As such we need to provide the vector in reverse
        // ...I guess this makes it more like a Snoc-tree than a Cons-list
        let vec: Vec<i64> = vec![3, 2, 1];
        let actual: Sexpr<i64> = vec.into();
        let expected: Sexpr<i64> = Sexpr::cons(
            Sexpr::Single(1),
            Sexpr::cons(
                Sexpr::Single(2),
                Sexpr::cons(Sexpr::Single(3), Sexpr::nil()),
            ),
        );
        assert_eq!(actual, expected);
    }
}
