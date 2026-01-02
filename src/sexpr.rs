#[derive(Debug, PartialEq, Clone)]
pub enum Sexpr {
    Nil,
    Atom(String),
    Num(usize),
    Pair(Box<Sexpr>, Box<Sexpr>),
}

impl Sexpr {
    pub fn cons(self, car: Sexpr) -> Sexpr {
        Sexpr::Pair(Box::new(car), Box::new(self))
    }

    pub fn reverse_list(self) -> Sexpr {
        let mut list = self;
        let mut result = Sexpr::Nil;

        while let Sexpr::Pair(car, cdr) = list {
            result = result.cons(*car);
            list = *cdr;
        }
        result
    }
}
