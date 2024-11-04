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
            _ => {
                println!("expected value: either literal or variable name");
                return None
            }
        }

        match tokens.pop_front().unwrap() {
            Token::IntLit(lit) => {
                return Some(Value::IntLit(lit))
            },
            Token::VarName(name) => {
                return Some(Value::Var(name))
            },
            _ => { return None }
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
            _ => {
                println!("expected statement start: either exit or variable declaration");
            }
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

struct Codegen {
    stack_size: i64,
    output_string: String,
    varnames: Vec<(String, i64)>,
}

impl Codegen {
    fn new() -> Self {
        Self {
            stack_size: 0,
            output_string: String::new(),
            varnames: vec![],
        }
    }

    fn push(&mut self, reg: &str) {
        self.output_string += format!("    push {}\n", reg).as_str();
        self.stack_size += 1;
    }
    
    fn pop(&mut self, reg: &str) {
        self.output_string += format!("    pop {}\n", reg).as_str();
        self.stack_size -= 1;
    }

    fn gen_value(&mut self, value: &Value) {
        match value {
            Value::IntLit(IntLiteral(num)) => {
                self.output_string += format!("    mov rax, {}\n", num).as_str();
                self.push("rax");
            }
            Value::Var(Variable(varname)) => {
                let var_position = self.varnames.iter().find(|var| var.0 == *varname).take().unwrap().1;
                self.output_string += format!("    mov rax, [rsp + 8 * {}]\n", var_position).as_str();
                self.push("rax");
            }
        }
    }

    fn generate_stmt(&mut self, statement: &Stmt) {
        match &statement {
            Stmt::DeclarationStmt(Variable(varname), value) => {
                self.gen_value(value);
                self.pop("rdi");

                self.varnames.push((varname.to_string(), self.stack_size));
                self.push("rax");
            }
            Stmt::ExitStmt(value) => {
                self.gen_value(value);
                self.output_string += format!("    mov rax, 60\n").as_str();
                self.pop("rdi");
                self.output_string += format!("    syscall\n").as_str();
            }
        }
    }

    fn generate(&mut self, ast: &AST) -> String {
        self.output_string += format!("global _start\n").as_str();
        self.output_string += format!("_start: \n").as_str();

        for statement in ast.statements.iter() {
            self.generate_stmt(statement);
        }

        let result = self.output_string.clone();
        *self = Codegen::new();
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
    let ast = Parser::parse(tokens).unwrap();
    let program = Codegen::new().generate(&ast);
    println!("{}", program);
    //let result = Codegen::generate(&tokens);
}
