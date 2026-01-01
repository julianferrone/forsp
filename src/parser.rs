use std::collections::VecDeque;

////////////////////////////////////////////////////////////
//                         Lexing                         //
////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Quote,
    Caret,
    Dollar,
    Semicolon, // Skip line-comments
    BracketOpen,
    BracketClose,
    WhiteSpace,
    NewLine,
    Int(usize),
    Literal(String),
}

const SPECIAL_CHARS: &str = "\'^$;()\n";

fn is_special_char(c: char) -> bool {
    SPECIAL_CHARS.contains(c)
}

fn to_token(atom: String) -> Token {
    match atom.parse::<usize>() {
        Ok(int) => Token::Int(int),
        Err(_) => Token::Literal(atom),
    }
}

pub fn scan(input: &str) -> VecDeque<Token> {
    let mut tokens = VecDeque::new();
    let mut last_was_whitespace = false;
    let mut chars = input.chars();
    let mut next = chars.next();
    let mut atom = String::new();
    while let Some(char) = next {
        if !atom.is_empty() && (is_special_char(char) || char.is_whitespace()) {
            tokens.push_back(to_token(atom));
            atom = String::new()
        }
        if char != '\n' && char.is_whitespace() {
            last_was_whitespace = true;
        } else {
            if last_was_whitespace {
                last_was_whitespace = false;
                tokens.push_back(Token::WhiteSpace);
            }
            match char {
                '\'' => tokens.push_back(Token::Quote),
                '^' => tokens.push_back(Token::Caret),
                '$' => tokens.push_back(Token::Dollar),
                ';' => tokens.push_back(Token::Semicolon),
                '(' => tokens.push_back(Token::BracketOpen),
                ')' => tokens.push_back(Token::BracketClose),
                '\n' => tokens.push_back(Token::NewLine),
                _ => atom.push(char),
            }
        }
        next = chars.next()
    }
    if !atom.is_empty() {
        tokens.push_back(to_token(atom));
    }
    tokens
}

////////////////////////////////////////////////////////////
//                         Reading                        //
////////////////////////////////////////////////////////////

#[derive(Debug, PartialEq, Clone)]
pub enum Sexpr {
    Nil,
    Atom(String),
    Num(usize),
    Pair(Box<Sexpr>, Box<Sexpr>),
}

impl Sexpr {
    fn cons(self, car: Sexpr) -> Sexpr {
        Sexpr::Pair(Box::new(car), Box::new(self))
    }

    fn reverse_list(self) -> Sexpr {
        let mut list = self;
        let mut result = Sexpr::Nil;

        while let Sexpr::Pair(car, cdr) = list {
            result = result.cons(*car);
            list = *cdr;
        }
        result
    }
}

enum Task<'a> {
    PrintSexpr(&'a Sexpr),
    PrintStr(&'static str),
}

impl std::fmt::Display for Sexpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut stack: Vec<Task> = Vec::new();
        stack.push(Task::PrintSexpr(self));

        while let Some(task) = stack.pop() {
            match task {
                Task::PrintSexpr(obj) => match obj {
                    Sexpr::Nil => write!(f, "()")?,
                    Sexpr::Atom(name) => write!(f, "{name}")?,
                    Sexpr::Num(num) => write!(f, "{num}")?,
                    Sexpr::Pair(car, cdr) => {
                        stack.push(Task::PrintStr(")"));

                        let mut cur = obj;
                        let mut elems: Vec<&Sexpr> = Vec::new();

                        loop {
                            match cur {
                                Sexpr::Pair(car, cdr) => {
                                    elems.push(car);
                                    cur = cdr;
                                }
                                Sexpr::Nil => break,
                                tail => {
                                    stack.push(Task::PrintStr(" . "));
                                    stack.push(Task::PrintSexpr(tail));
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

#[derive(Debug, PartialEq)]
struct Frame {
    rev_items: Sexpr, // reversed list
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum SpecialForm {
    Quote,
    Bind,
    Resolve,
}

fn desugar_special_form(obj: Sexpr, special_form: &SpecialForm) -> VecDeque<Sexpr> {
    match special_form {
        SpecialForm::Quote => VecDeque::from([Sexpr::Atom("quote".to_owned()), obj]),
        SpecialForm::Bind => VecDeque::from([
            Sexpr::Atom("quote".to_owned()),
            obj,
            Sexpr::Atom("pop".to_owned()),
        ]),
        SpecialForm::Resolve => VecDeque::from([
            Sexpr::Atom("quote".to_owned()),
            obj,
            Sexpr::Atom("push".to_owned()),
        ]),
    }
}

pub fn read(mut tokens: VecDeque<Token>) -> Result<Sexpr, String> {
    let mut stack: Vec<Frame> = Vec::new();
    let mut result: Option<Sexpr> = None;
    let mut special_form: Option<SpecialForm> = None;

    let mut is_comment = false;
    while let Some(token) = tokens.pop_front() {
        // println!("Stack: {stack:?}, Token: {token:?}");
        match token {
            // Skip whitespace and comments
            Token::Semicolon => is_comment = true,
            Token::WhiteSpace => (),
            Token::NewLine => is_comment = false,
            _ if is_comment => (),
            // Special forms
            Token::Quote => {
                special_form = Some(SpecialForm::Quote);
            }
            Token::Dollar => special_form = Some(SpecialForm::Bind),
            Token::Caret => {
                special_form = Some(SpecialForm::Resolve);
            }
            // Atoms
            Token::Int(int) => {
                let obj = Sexpr::Num(int);
                match (special_form.clone(), stack.last_mut()) {
                    (None, None) => {
                        if result.is_some() {
                            return Err("Multiple top-level expressions".into());
                        }
                        result = Some(obj);
                    }
                    (None, Some(frame)) => {
                        frame.rev_items = frame.rev_items.clone().cons(obj);
                    }
                    (Some(special), None) => {
                        special_form = None;
                        let mut expansion = Sexpr::Nil;
                        let mut parts = desugar_special_form(obj, &special);
                        while let Some(part) = parts.pop_front() {
                            expansion = expansion.cons(part);
                        }
                        if result.is_some() {
                            return Err("Multiple top-level expressions".into());
                        }
                        result = Some(expansion);
                    }
                    (Some(special), Some(frame)) => {
                        special_form = None;
                        let mut parts = desugar_special_form(obj, &special);
                        while let Some(part) = parts.pop_front() {
                            frame.rev_items = frame.rev_items.clone().cons(part);
                        }
                    }
                }
            }
            Token::Literal(name) => {
                let obj = Sexpr::Atom(name);
                match (special_form.clone(), stack.last_mut()) {
                    (None, None) => {
                        if result.is_some() {
                            return Err("Multiple top-level expressions".into());
                        }
                        result = Some(obj);
                    }
                    (None, Some(frame)) => {
                        frame.rev_items = frame.rev_items.clone().cons(obj);
                    }
                    (Some(special), None) => {
                        special_form = None;
                        let mut expansion = Sexpr::Nil;
                        let mut parts = desugar_special_form(obj, &special);
                        while let Some(part) = parts.pop_front() {
                            expansion = expansion.cons(part);
                        }
                        if result.is_some() {
                            return Err("Multiple top-level expressions".into());
                        }
                        result = Some(expansion);
                    }
                    (Some(special), Some(frame)) => {
                        special_form = None;
                        let mut parts = desugar_special_form(obj, &special);
                        while let Some(part) = parts.pop_front() {
                            frame.rev_items = frame.rev_items.clone().cons(part);
                        }
                    }
                }
            }
            // List
            Token::BracketOpen => stack.push(Frame {
                rev_items: Sexpr::Nil,
            }),
            Token::BracketClose => {
                let frame = stack.pop().ok_or("Unexpected ')'")?;
                let list = frame.rev_items.reverse_list();
                if let Some(parent) = stack.last_mut() {
                    parent.rev_items = parent.rev_items.clone().cons(list);
                } else {
                    if result.is_some() {
                        return Err("Multiple top-level expressions".into());
                    }
                    result = Some(list);
                }
            }
        }
    }
    if !stack.is_empty() {
        return Err("Unclosed '('".into());
    }
    result.ok_or("Empty input".into())
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;

    #[test]
    fn test_scan_drop() {
        assert_eq!(
            scan("($x) $drop"),
            vec![
                Token::BracketOpen,
                Token::Dollar,
                Token::Literal("x".to_string()),
                Token::BracketClose,
                Token::WhiteSpace,
                Token::Dollar,
                Token::Literal("drop".to_string()),
            ]
        )
    }

    #[test]
    fn test_scan_force() {
        assert_eq!(
            scan("($x x) $force"),
            vec![
                Token::BracketOpen,
                Token::Dollar,
                Token::Literal("x".to_string()),
                Token::WhiteSpace,
                Token::Literal("x".to_string()),
                Token::BracketClose,
                Token::WhiteSpace,
                Token::Dollar,
                Token::Literal("force".to_string()),
            ]
        )
    }

    #[test]
    fn test_scan_dup() {
        assert_eq!(
            scan("($x ^x ^x) $dup"),
            vec![
                Token::BracketOpen,
                Token::Dollar,
                Token::Literal("x".to_string()),
                Token::WhiteSpace,
                Token::Caret,
                Token::Literal("x".to_string()),
                Token::WhiteSpace,
                Token::Caret,
                Token::Literal("x".to_string()),
                Token::BracketClose,
                Token::WhiteSpace,
                Token::Dollar,
                Token::Literal("dup".to_string()),
            ]
        )
    }

    #[test]
    fn test_scan_plus() {
        assert_eq!(
            scan("(0 swap - -) $+"),
            vec![
                Token::BracketOpen,
                Token::Int(0),
                Token::WhiteSpace,
                Token::Literal("swap".to_owned()),
                Token::WhiteSpace,
                Token::Literal("-".to_owned()),
                Token::WhiteSpace,
                Token::Literal("-".to_owned()),
                Token::BracketClose,
                Token::WhiteSpace,
                Token::Dollar,
                Token::Literal("+".to_owned())
            ]
        )
    }

    #[test]
    fn test_scan_nil() {
        assert_eq!(
            scan("('()) $nil"),
            vec![
                Token::BracketOpen,
                Token::Quote,
                Token::BracketOpen,
                Token::BracketClose,
                Token::BracketClose,
                Token::WhiteSpace,
                Token::Dollar,
                Token::Literal("nil".to_string())
            ]
        )
    }

    #[test]
    fn test_scan_newline() {
        assert_eq!(
            scan(
                "4 3
                print
                "
            ),
            vec![
                Token::Int(4),
                Token::WhiteSpace,
                Token::Int(3),
                Token::NewLine,
                Token::WhiteSpace,
                Token::Literal("print".to_string()),
                Token::NewLine
            ]
        )
    }

    #[test]
    fn test_reverse_list() {
        let list = Sexpr::Nil
            .cons(Sexpr::Num(1))
            .cons(Sexpr::Num(2))
            .cons(Sexpr::Num(3));

        let reversed = Sexpr::Nil
            .cons(Sexpr::Num(3))
            .cons(Sexpr::Num(2))
            .cons(Sexpr::Num(1));

        assert_eq!(list.reverse_list(), reversed);
    }

    #[test]
    fn test_reading_force() {
        let scanned = scan("($x x)");
        let read = read(scanned).unwrap();
        let expected = Sexpr::Nil
            .cons(Sexpr::Atom("x".into()))
            .cons(Sexpr::Atom("pop".into()))
            .cons(Sexpr::Atom("x".into()))
            .cons(Sexpr::Atom("quote".into()));
        assert_eq!(read, expected);
    }

    #[test]
    fn test_display_pair() {
        let read = read(scan("(x y z)")).unwrap();
        assert_eq!(format!("{read}"), "(x y z)")
    }

    #[test]
    fn test_display_force() {
        let read = read(scan("($x x)")).unwrap();
        assert_eq!(format!("{read}"), "(quote x pop x)")
    }

    #[test]
    fn test_display_dup() {
        let read = read(scan("(($x ^x ^x) $dup)")).unwrap();
        assert_eq!(
            format!("{read}"),
            "((quote x pop quote x push quote x push) quote dup pop)"
        )
    }

    #[test]
    fn test_display_assoc() {
        let read = read(scan("((a b) (c d))")).unwrap();
        assert_eq!(
            format!("{read}"),
            "((a b) (c d))"
        )
    }
}
