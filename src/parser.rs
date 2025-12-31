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
pub enum Object {
    Nil,
    Atom(String),
    Num(usize),
    Pair {
        car: Box<Object>,
        cdr: Box<Object>,
    },
    Closure {
        body: Box<Object>,
        environment: Box<Object>,
    },
    Primitive(fn(Object) -> Object),
}

impl Object {
    fn cons(self, car: Object) -> Object {
        Object::Pair {
            car: Box::new(car),
            cdr: Box::new(self),
        }
    }

    fn cons_mut(mut self, car: Object) {
        let old = self;
        self = Object::Pair {
            car: Box::new(car),
            cdr: Box::new(old),
        }
    }

    fn reverse_list(self) -> Object {
        let mut list = self;
        let mut result = Object::Nil;

        while let Object::Pair { car, cdr } = list {
            result = result.cons(*car);
            list = *cdr;
        }
        result
    }
}

enum Task<'a> {
    PrintObject(&'a Object),
    PrintStr(&'static str),
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut stack: Vec<Task> = Vec::new();
        stack.push(Task::PrintObject(self));

        while let Some(task) = stack.pop() {
            match task {
                Task::PrintObject(obj) => match obj {
                    Object::Nil => write!(f, "()")?,
                    Object::Atom(name) => write!(f, "{name}")?,
                    Object::Num(num) => write!(f, "{num}")?,
                    Object::Pair { car, cdr } => {
                        stack.push(Task::PrintStr(")"));

                        let mut cur = obj;
                        let mut first = true;
                        loop {
                            match cur {
                                Object::Pair { car, cdr } => {
                                    if !first {
                                        stack.push(Task::PrintStr(" "));
                                    }
                                    stack.push(Task::PrintObject(car));
                                    cur = cdr;
                                    first = false;
                                }
                                Object::Nil => break,
                                tail => {
                                    stack.push(Task::PrintStr(" . "));
                                    stack.push(Task::PrintObject(tail));
                                    break;
                                }
                            }
                        }

                        stack.push(Task::PrintStr("("));
                    }
                    Object::Closure {
                        body,
                        environment: _,
                    } => {
                        stack.push(Task::PrintStr(">"));
                        stack.push(Task::PrintObject(body));
                        stack.push(Task::PrintStr("CLOSURE<"))
                    }
                    Object::Primitive(_) => write!(f, "PRIMITIVE")?,
                },
                Task::PrintStr(s) => write!(f, "{s}")?,
            }
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
struct Frame {
    rev_items: Object, // reversed list
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum SpecialForm {
    Quote,
    Bind,
    Resolve,
}

fn desugar_special_form(obj: Object, special_form: &SpecialForm) -> VecDeque<Object> {
    match special_form {
        SpecialForm::Quote => VecDeque::from([Object::Atom("quote".to_owned()), obj]),
        SpecialForm::Bind => VecDeque::from([
            Object::Atom("quote".to_owned()),
            obj,
            Object::Atom("pop".to_owned()),
        ]),
        SpecialForm::Resolve => VecDeque::from([
            Object::Atom("quote".to_owned()),
            obj,
            Object::Atom("push".to_owned()),
        ]),
    }
}

pub fn read(mut tokens: VecDeque<Token>) -> Result<Object, String> {
    let mut stack: Vec<Frame> = Vec::new();
    let mut result: Option<Object> = None;
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
                let obj = Object::Num(int);
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
                        let mut expansion = Object::Nil;
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
                let obj = Object::Atom(name);
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
                        let mut expansion = Object::Nil;
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
                rev_items: Object::Nil,
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
        let list = Object::Nil
            .cons(Object::Num(1))
            .cons(Object::Num(2))
            .cons(Object::Num(3));

        let reversed = Object::Nil
            .cons(Object::Num(3))
            .cons(Object::Num(2))
            .cons(Object::Num(1));

        assert_eq!(list.reverse_list(), reversed);
    }

    #[test]
    fn test_reading_force() {
        let scanned = scan("($x x)");
        let read = read(scanned).unwrap();
        let expected = Object::Nil
            .cons(Object::Atom("x".into()))
            .cons(Object::Atom("pop".into()))
            .cons(Object::Atom("x".into()))
            .cons(Object::Atom("quote".into()));
        println!("{read}");
        assert_eq!(read, expected);
    }
}
