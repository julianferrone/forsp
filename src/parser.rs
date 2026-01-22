use crate::primitive::try_parse;
use crate::sexpr::{Atom, Sexpr};
use crate::vm;
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

#[derive(Debug, PartialEq)]
struct Frame {
    rev_items: Sexpr<Atom>, // reversed list
    pending_special: Option<SpecialForm>,
}

impl Frame {
    fn new() -> Frame {
        Frame {
            rev_items: Sexpr::nil(),
            pending_special: None,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum SpecialForm {
    Quote,
    Bind,
    Resolve,
}

fn quote(obj: Sexpr<Atom>) -> Sexpr<Atom> {
    Sexpr::cons(
        Sexpr::Single(Atom::name("quote")),
        Sexpr::cons(obj, Sexpr::nil()),
    )
}

fn bind(obj: Sexpr<Atom>) -> Sexpr<Atom> {
    Sexpr::cons(
        Sexpr::Single(Atom::name("quote")),
        Sexpr::cons(
            obj,
            Sexpr::cons(Sexpr::Single(Atom::name("pop")), Sexpr::nil()),
        ),
    )
}

fn resolve(obj: Sexpr<Atom>) -> Sexpr<Atom> {
    Sexpr::cons(
        Sexpr::Single(Atom::name("quote")),
        Sexpr::cons(
            obj,
            Sexpr::cons(Sexpr::Single(Atom::name("push")), Sexpr::nil()),
        ),
    )
}

fn emit(stack: &mut Vec<Frame>, obj: Sexpr<Atom>) -> Result<(), String> {
    let frame = stack
        .last_mut()
        .ok_or::<String>("No frames in stack".into())?;

    match frame.pending_special.take() {
        Some(special_form) => {
            let extension = match special_form {
                SpecialForm::Quote => quote(obj),
                SpecialForm::Bind => bind(obj),
                SpecialForm::Resolve => resolve(obj),
            }
            .reverse_list();
            frame.rev_items = Sexpr::extend(extension, frame.rev_items.clone())
        }
        None => frame.rev_items = Sexpr::cons(obj, frame.rev_items.clone()),
    }

    Ok(())
}

pub fn read(mut tokens: VecDeque<Token>) -> Result<Sexpr<Atom>, String> {
    let mut stack: Vec<Frame> = vec![Frame::new()];
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
                stack
                    .last_mut()
                    .ok_or("No stacks in frame".to_owned())?
                    .pending_special = Some(SpecialForm::Quote);
            }
            Token::Dollar => {
                stack
                    .last_mut()
                    .ok_or("No stacks in frame".to_owned())?
                    .pending_special = Some(SpecialForm::Bind);
            }
            Token::Caret => {
                stack
                    .last_mut()
                    .ok_or("No stacks in frame".to_owned())?
                    .pending_special = Some(SpecialForm::Resolve);
            }

            // Atoms
            Token::Int(int) => {
                let obj = Sexpr::Single(Atom::Num(int));
                emit(&mut stack, obj)?;
            }
            Token::Literal(name) => {
                let obj = Sexpr::Single(Atom::Name(name));
                emit(&mut stack, obj)?;
            }

            // List
            Token::BracketOpen => stack.push(Frame::new()),
            Token::BracketClose => {
                let frame = stack.pop().ok_or("Unexpected ')'")?;
                let list = frame.rev_items.reverse_list();
                emit(&mut stack, list)?
            }
        }
    }
    // println!("Stack: {stack:?}");
    if !stack.len() == 1 {
        return Err("Unclosed '('".to_owned());
    }
    let result = stack
        .pop()
        .ok_or("Empty input".to_owned())?
        .rev_items
        .reverse_list();
    Ok(result)
}

////////////////////////////////////////////////////////////
//                          Tests                         //
////////////////////////////////////////////////////////////

fn quoted(value: Sexpr<Atom>) -> vm::Value {
    match value {
        Sexpr::Single(atom) => vm::Value::Atom(atom),
        Sexpr::List(atoms) => {
            let values = atoms
                .into_iter()
                .map(|boxed: Box<Sexpr<Atom>>| Box::new(Sexpr::Single(quoted(*boxed))))
                .collect::<Vec<Box<Sexpr<vm::Value>>>>()
                .into();
            vm::Value::Sexpr(Box::new(Sexpr::List(values)))
        }
    }
}

pub fn parse(mut atoms: Sexpr<Atom>) -> Result<VecDeque<vm::Instruction>, String> {
    assert!(matches!(atoms, Sexpr::List(_)));
    let mut car = Sexpr::car_mut(&mut atoms);
    let mut do_quote = false;
    let mut instructions = VecDeque::new();
    while let Ok(atom) = car {
        if do_quote {
            let value = quoted(atom);
            let instruction = vm::Instruction::PushValue(value);
            instructions.push_back(instruction);
            do_quote = false;
        } else {
            let instruction = match atom {
                Sexpr::Single(Atom::Name(name)) if name == "quote" => {
                    do_quote = true;
                    None
                }
                Sexpr::Single(Atom::Name(name)) => {
                    let instruction = try_parse(&name)
                        .map_or_else(
                            || vm::Instruction::Call(name),
                            |prim| vm::Instruction::ApplyPrimitive(prim)
                        );
                    Some(instruction)
                }
                Sexpr::Single(num) => Some(vm::Instruction::PushValue(vm::Value::Atom(num))),
                mut list => {
                    let instructions = parse(list)?;
                    Some(vm::Instruction::MakeClosure(instructions))
                }
            };
            if let Some(instruction) = instruction {
                instructions.push_back(instruction);
            };
        }
        car = Sexpr::car_mut(&mut atoms);
    }
    Ok(instructions)
}

////////////////////////////////////////////////////////////
//                          Tests                         //
////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;
    use crate::primitive::Primitive;
    use crate::vm;

    //////////              Test Scanner              //////////

    #[test]
    fn scan_drop() {
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
    fn scan_force() {
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
    fn scan_dup() {
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
    fn scan_plus() {
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
    fn scan_nil() {
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
    fn scan_newline() {
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
    fn reverse_list() {
        let list = Sexpr::cons(
            Sexpr::Single(Atom::Num(3)),
            Sexpr::cons(
                Sexpr::Single(Atom::Num(2)),
                Sexpr::cons(Sexpr::Single(Atom::Num(1)), Sexpr::nil()),
            ),
        );

        let reversed = Sexpr::cons(
            Sexpr::Single(Atom::Num(1)),
            Sexpr::cons(
                Sexpr::Single(Atom::Num(2)),
                Sexpr::cons(Sexpr::Single(Atom::Num(3)), Sexpr::nil()),
            ),
        );

        assert_eq!(list.reverse_list(), reversed);
    }

    //////////               Test Reader              //////////

    #[test]
    fn read_num_list() {
        let program = read(scan("(1 2 3)")).unwrap();
        let line = Sexpr::car(&program).expect("Program should have at least one line");
        let expected = Sexpr::cons(
            Sexpr::Single(Atom::Num(1)),
            Sexpr::cons(
                Sexpr::Single(Atom::Num(2)),
                Sexpr::cons(Sexpr::Single(Atom::Num(3)), Sexpr::nil()),
            ),
        );
        assert_eq!(line, expected)
    }

    #[test]
    fn read_name_list() {
        let program = read(scan("(a b c)")).unwrap();
        let line = Sexpr::car(&program).expect("Program should have at least one line");
        let expected = Sexpr::cons(
            Sexpr::Single(Atom::Name("a".into())),
            Sexpr::cons(
                Sexpr::Single(Atom::Name("b".into())),
                Sexpr::cons(Sexpr::Single(Atom::Name("c".into())), Sexpr::nil()),
            ),
        );
        assert_eq!(line, expected)
    }
    #[test]
    fn reading_force() {
        let program = read(scan("($x x)")).unwrap();
        let line = Sexpr::car(&program).expect("Program should have at least one line");
        let expected = Sexpr::cons(
            Sexpr::Single(Atom::Name("quote".into())),
            Sexpr::cons(
                Sexpr::Single(Atom::Name("x".into())),
                Sexpr::cons(
                    Sexpr::Single(Atom::Name("pop".into())),
                    Sexpr::cons(Sexpr::Single(Atom::Name("x".into())), Sexpr::nil()),
                ),
            ),
        );
        assert_eq!(line, expected);
    }

    fn check_display(input: &str, expected: &str) {
        let read: Sexpr<Atom> = read(scan(input))
            .expect("Test input should be well-formed")
            .into();
        let formatted = format!("{}", &read);
        assert_eq!(formatted, expected)
    }

    #[test]
    fn display_pair() {
        check_display("x y z", "(x y z)");
    }

    #[test]
    fn display_force() {
        check_display("$x x", "(quote x pop x)");
    }

    #[test]
    fn display_dup() {
        check_display(
            "($x ^x ^x) $dup",
            "((quote x pop quote x push quote x push) quote dup pop)",
        );
    }

    #[test]
    fn display_swap() {
        check_display(
            "($x $y ^x ^y) $swap",
            "((quote x pop quote y pop quote x push quote y push) quote swap pop)",
        );
    }

    #[test]
    fn display_tree() {
        check_display("(a b) (c d)", "((a b) (c d))");
    }

    #[test]
    fn quote_foo() {
        check_display("'foo", "(quote foo)");
    }

    #[test]
    fn bind_bar() {
        check_display("$bar", "(quote bar pop)");
    }

    #[test]
    fn resolve_baz() {
        check_display("^baz", "(quote baz push)");
    }

    #[test]
    fn quote_list() {
        check_display("'(1 2 3)", "(quote (1 2 3))");
    }

    #[test]
    fn resolve_list() {
        check_display("$(1 2 3)", "(quote (1 2 3) pop)");
    }

    #[test]
    fn bind_list() {
        check_display("^(1 2 3)", "(quote (1 2 3) push)");
    }


    //////////               Test Parser              //////////

    fn parse_str(s: &str) -> VecDeque<vm::Instruction> {
        let scanned = scan(s);
        let readed = read(scanned).expect("Test input should read");
        let parsed = parse(readed).expect("Test input should be parseable");
        parsed
    }

    #[test]
    fn parse_num() {
        let input = "15";
        let parsed = parse_str(input);
        let expected = VecDeque::from([
            vm::Instruction::PushValue(vm::Value::Atom(Atom::Num(15)))
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn parse_call() {
        let input = "foo";
        let parsed = parse_str(input);
        let expected = VecDeque::from([
            vm::Instruction::Call("foo".to_owned())
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn parse_primitive_plus() {
        let input = "+";
        let parsed = parse_str(input);
        let expected = VecDeque::from([
            vm::Instruction::ApplyPrimitive(Primitive::Add)
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn parse_quote_atom() {
        let input = "'foo";
        let parsed = parse_str(input);
        let expected = VecDeque::from([
            vm::Instruction::PushValue(vm::Value::Atom(Atom::Name("foo".into())))
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn parse_bind_atom() {
        let input = "$foo";
        let parsed = parse_str(input);
        let expected = VecDeque::from([
            vm::Instruction::PushValue(vm::Value::Atom(Atom::Name("foo".into()))),
            vm::Instruction::ApplyPrimitive(Primitive::Pop),
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn parse_resolve_atom() {
        let input = "^foo";
        let parsed = parse_str(input);
        let expected = VecDeque::from([
            vm::Instruction::PushValue(vm::Value::Atom(Atom::Name("foo".into()))),
            vm::Instruction::ApplyPrimitive(Primitive::Push),
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn parse_quote_list() {
        let input = "'(a b c)";
        let parsed = parse_str(input);
        let list = Sexpr::from_vec(vec![
            vm::Value::Atom(Atom::Name("c".into())),
            vm::Value::Atom(Atom::Name("b".into())),
            vm::Value::Atom(Atom::Name("a".into())),
        ]);
        let expected = VecDeque::from([
            vm::Instruction::PushValue(vm::Value::Sexpr(Box::new(list)))
        ]);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn parse_define_dup() {
        let input = "($x ^x ^x) $dup";
        let parsed = parse_str(input);

        let dup_instructions = VecDeque::from([
            vm::Instruction::PushValue(vm::Value::Atom(Atom::Name("x".into()))),
            vm::Instruction::ApplyPrimitive(Primitive::Pop),
            vm::Instruction::PushValue(vm::Value::Atom(Atom::Name("x".into()))),
            vm::Instruction::ApplyPrimitive(Primitive::Push),
            vm::Instruction::PushValue(vm::Value::Atom(Atom::Name("x".into()))),
            vm::Instruction::ApplyPrimitive(Primitive::Push),
        ]);

        let expected = VecDeque::from([
            vm::Instruction::MakeClosure(dup_instructions),
            vm::Instruction::PushValue(vm::Value::Atom(Atom::Name("dup".into()))),
            vm::Instruction::ApplyPrimitive(Primitive::Pop),
        ]);

        assert_eq!(parsed, expected);
    }
}
