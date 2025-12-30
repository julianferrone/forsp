#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Quote,
    Caret,
    Dollar,
    Semicolon, // Skip line-comments
    BracketOpen,
    BracketClose,
    WhiteSpace,
    Int(usize),
    Literal(String),
}

const SPECIAL_CHARS: &str = "\'^$;()";

fn is_special_char(c: char) -> bool {
    SPECIAL_CHARS.contains(c)
}

fn to_token(atom: String) -> Token {
    match atom.parse::<usize>() {
        Ok(int) => Token::Int(int),
        Err(_) => Token::Literal(atom),
    }
}

pub fn scan(input: &str) -> Vec<Token> {
    let mut tokens = vec![];
    let mut last_was_whitespace = false;
    let mut chars = input.chars();
    let mut next = chars.next();
    let mut atom = String::new();
    while let Some(char) = next {
        if !atom.is_empty() && (is_special_char(char) || char.is_whitespace()) {
            tokens.push(to_token(atom));
            atom = String::new()
        }
        if char.is_whitespace() {
            last_was_whitespace = true;
        } else {
            if last_was_whitespace {
                last_was_whitespace = false;
                tokens.push(Token::WhiteSpace);
            }
            match char {
                '\'' => tokens.push(Token::Quote),
                '^' => tokens.push(Token::Caret),
                '$' => tokens.push(Token::Dollar),
                ';' => tokens.push(Token::Semicolon),
                '(' => tokens.push(Token::BracketOpen),
                ')' => tokens.push(Token::BracketClose),
                _ => atom.push(char),
            }
        }
        next = chars.next()
    }
    if !atom.is_empty() {
        tokens.push(to_token(atom));
    }
    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
