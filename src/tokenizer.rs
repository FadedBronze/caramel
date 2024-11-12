use crate::VecDeque;

#[derive(Debug, Clone)]
pub struct Variable(pub String);
#[derive(Debug, Clone)]
pub struct IntLiteral(pub i64);

#[derive(Debug, Clone)]
pub enum Token {
    LetDeclaration,
    As,
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
    Colon,
    Dot,

    Int64,
    Uint64,
    Float64
}

pub struct Tokenizer;

impl Tokenizer {
    const KEYWORDS: &'static [(&'static str, Token)] = &[
        ("exit", Token::Exit),
        ("let", Token::LetDeclaration),
        ("if", Token::If),
        ("else", Token::Else),
        ("elif", Token::Elif),
        ("while", Token::While),

        ("as", Token::As),
        ("int64", Token::Int64),
        ("uint64", Token::Uint64),
        ("float64", Token::Float64),
    ];

    pub fn tokenize(src: &str) -> VecDeque<Token> {
        let mut tokens = VecDeque::new();
        let mut start = 0;
        let mut end = 0;

        for (i, c) in src.chars().enumerate() {
            if i != 0 {
                end += 1;
            }

            if !c.is_alphanumeric() {
                let mut matched = false;
                for (keyword, token) in Self::KEYWORDS {
                    if &src[start..end] == *keyword {
                        tokens.push_back(token.clone());
                        matched = true;
                        break;
                    }
                }
                if !matched && src[start..start+1].to_string().chars().next().unwrap().is_alphabetic() {
                    tokens.push_back(Token::VarName(Variable(src[start..end].to_string())))
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
                ':' => {tokens.push_back(Token::Colon)},
                '.' => {tokens.push_back(Token::Dot)},
                _ => {
                    start -= 1;
                }
            }
        }

        tokens
    }
}
