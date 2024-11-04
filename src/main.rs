use std::{collections::VecDeque, env, fs};

#[derive(Debug, Clone)]
struct Variable(String);
#[derive(Debug, Clone)]
struct IntLiteral(i64);

#[derive(Debug, Clone)]
enum Token {
    LetDeclaration,
    VarName(Variable),
    EqualSign,
    LeftParen,
    RightParen,
    Exit,
    IntLit(IntLiteral),
    Semicolon,
}

impl Token {}

struct Tokenizer;

impl Tokenizer {
    fn tokenize(src: &str) -> VecDeque<Token> {
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
                ';' => {tokens.push_back(Token::Semicolon)},
                '=' => {tokens.push_back(Token::EqualSign)},
                _ => {
                    start -= 1;
                }
            }
        }

        tokens
    }
}

#[derive(Debug)]
enum Value {
    Var(Variable),
    IntLit(IntLiteral)
}

#[derive(Debug)]
enum Stmt {
    ExitStmt(Value),
    DeclarationStmt(Variable, Value),
}

#[derive(Debug)]
struct AST {
    statements: Vec<Stmt>
}

// variable => String
// int_literal => i64
// let_declaration => let
// exit => exit
//
// stmt => {
//   let_declaration variable = {
//      variable, 
//      int_literal
//   }, 
//   exit left_paren int_literal right_paren
// }

struct Parser;

impl Parser {
    fn parse_value(tokens: &mut VecDeque<Token>) -> Option<Value> {
        match &tokens[0] {
            Token::IntLit(_) => {},
            Token::VarName(_) => {},
            _ => return None
        }

        match tokens.pop_front().unwrap() {
            Token::IntLit(lit) => {
                return Some(Value::IntLit(lit))
            },
            Token::VarName(name) => {
                return Some(Value::Var(name))
            },
            _ => { 
                println!("expected value: either literal or variable name");
                return None
            }
        }
    }

    fn parse_stmt(tokens: &mut VecDeque<Token>) -> Option<Stmt> {
        match tokens[0] {
            Token::Exit => {
                tokens.pop_front();
                let next = tokens.pop_front();

                let Some(Token::LeftParen) = next else { 
                    println!("expected ( after exit");
                    return None;
                };

                let result = Stmt::ExitStmt(Parser::parse_value(tokens)?);
                
                let next = tokens.pop_front();
                
                let Some(Token::RightParen) = next else { 
                    println!("expected )");
                    return None;
                };
                
                let next = tokens.pop_front();
                
                let Some(Token::Semicolon) = next else { 
                    println!("expected ;");
                    return None;
                };

                return Some(result);
            }
            Token::LetDeclaration => {
                tokens.pop_front();
                let next = tokens.pop_front();

                let Some(Token::VarName(dest)) = next else { 
                    println!("expected variable name");
                    return None;
                };

                let next = tokens.pop_front();
                
                let Some(Token::EqualSign) = next else { 
                    println!("expected =");
                    return None;
                };
                
                let result = Stmt::DeclarationStmt(dest, Parser::parse_value(tokens)?);

                let next = tokens.pop_front();
                
                let Some(Token::Semicolon) = next else { 
                    println!("expected ;");
                    return None;
                };

                return Some(result);
            }
            _ => {}
        }

        None
    }

    fn parse(mut tokens: VecDeque<Token>) -> Option<AST> {
        let mut statements = vec![];

        while tokens.len() != 0 {
            statements.push(Parser::parse_stmt(&mut tokens)?);
        }

        return Some(AST { statements });
    }
}

struct Codegen;

impl Codegen {
    fn generate(tokens: &Vec<Token>) -> String {
        let mut result = String::new();

        result += "global _start\n";
        result += "\n";
        result += "_start:\n";

        for i in 0..tokens.len()-4 {
            if let (
                Token::Exit, 
                Token::LeftParen, 
                Token::IntLit(IntLiteral(num)), 
                Token::RightParen, 
                Token::Semicolon
            ) = (
                tokens[i].clone(), 
                tokens[i+1].clone(), 
                tokens[i+2].clone(), 
                tokens[i+3].clone(), 
                tokens[i+4].clone()
            ) {
                 result += "    mov rax, 60\n";
                 result += format!("    mov rdi, {}\n", num).as_str();
                 result += "    syscall\n";
            }
        }

        result += "    ret\n";
        result
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("caramel /path/to/src.mel");
        return;
    }
    
    println!("{}", args[1].clone());

    let Ok(bytes) = fs::read_to_string(args[1].clone()) else {
        println!("cannot find source file at the destination specified");
        return;
    };

    let tokens = Tokenizer::tokenize(&bytes);
    let ast = Parser::parse(tokens);
    println!("{:#?}", ast.unwrap());
    //let result = Codegen::generate(&tokens);
}
