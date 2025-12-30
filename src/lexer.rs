#[derive(Debug)]
pub enum Token {
    Quote,
    Caret,
    Dollar,
    Semicolon, // Skip line-comments
    BracketOpen,
    BracketClose,
    Whitespace,
    Atom(String),
}

const SPECIAL_CHARS: &str = "\'^$;()";

fn is_special_char(c: char) -> bool {
    SPECIAL_CHARS.contains(c)
}

pub fn scan(input: &str) -> Vec<Token> {
    let mut tokens = vec![];
    let mut last_parsed = Token::Whitespace;
    let mut chars = input.chars();
    let mut next = chars.next();
    let mut atom = String::new();
    while let Some(char) = next {
        if is_special_char(char) && !atom.is_empty() {
            tokens.push(Token::Atom(atom));
            atom = String::new()
        }
        match char {
            '\'' => tokens.push(Token::Quote),
            '^' => tokens.push(Token::Caret),
            '$' => tokens.push(Token::Dollar),
            ';' => tokens.push(Token::Semicolon),
            '(' => tokens.push(Token::BracketOpen),
            ')' => tokens.push(Token::BracketClose),
            c if c.is_whitespace() => (),
            _ => atom.push(char),
        }
        next = chars.next()
    }
    if !atom.is_empty() {
        tokens.push(Token::Atom(atom));
    }
    tokens
}
