use crate::VecDeque;

#[derive(Debug, Clone)]
pub struct Variable(pub String);
#[derive(Debug, Clone)]
pub struct IntLiteral(pub i64);

#[derive(Debug, Clone)]
pub enum Token {
    LetDeclaration,
    If,
    Else,
    Elif,
    VarName(Variable),
    EqualSign,
    LeftParen,
    RightParen,
    LeftCurlyParen,
    RightCurlyParen,
    Exit,
    IntLit(IntLiteral),
    Semicolon,
    Exclamation,
    Asterix,
    Plus,
    Slash,
    Minus,
    Ampersand,
    VertBar,
    LessThan,
    GreaterThan,
    While,
}

pub struct Tokenizer;

impl Tokenizer {
    pub fn tokenize(src: &str) -> VecDeque<Token> {
        let mut tokens = VecDeque::new();
        let mut start = 0;
        let mut end = 0;

        for (i, c) in src.chars().enumerate() {
            if i != 0 {
                end += 1;
            }

            if !c.is_alphanumeric() {
                match &src[start..end] {
                    "exit" => {
                        tokens.push_back(Token::Exit)
                    },
                    "let" => {
                        tokens.push_back(Token::LetDeclaration)
                    },
                    "if" => {
                        tokens.push_back(Token::If)
                    },
                    "else" => {
                        tokens.push_back(Token::Else)
                    },
                    "elif" => {
                        tokens.push_back(Token::Elif)
                    },
                    "while" => {
                        tokens.push_back(Token::While)
                    },
                    _ => {
                        if src[start..start+1].to_string().chars().next().unwrap().is_alphabetic() {
                            tokens.push_back(Token::VarName(Variable(src[start..end].to_string())))
                        }
                    }
                }

                if let Ok(num) = src[start..end].parse::<i64>() {
                    tokens.push_back(Token::IntLit(IntLiteral(num)))
                }

                start = i;
                end = i;    
            }

            start += 1;
            match c {
                ' ' => {},
                '\n' => {},
                '(' => {tokens.push_back(Token::LeftParen)},
                ')' => {tokens.push_back(Token::RightParen)},
                '{' => {tokens.push_back(Token::LeftCurlyParen)},
                '}' => {tokens.push_back(Token::RightCurlyParen)},
                ';' => {tokens.push_back(Token::Semicolon)},
                '=' => {tokens.push_back(Token::EqualSign)},
                '+' => {tokens.push_back(Token::Plus)},
                '*' => {tokens.push_back(Token::Asterix)},
                '/' => {tokens.push_back(Token::Slash)},
                '-' => {tokens.push_back(Token::Minus)},
                '&' => {tokens.push_back(Token::Ampersand)},
                '!' => {tokens.push_back(Token::Exclamation)},
                '|' => {tokens.push_back(Token::VertBar)},
                '<' => {tokens.push_back(Token::LessThan)},
                '>' => {tokens.push_back(Token::GreaterThan)},
                _ => {
                    start -= 1;
                }
            }
        }

        tokens
    }
}
