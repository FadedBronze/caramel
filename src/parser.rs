use std::collections::VecDeque;

use crate::tokenizer::{IntLiteral, Token, Variable};

#[derive(Debug, Clone, Copy)]
pub enum Operation {
    Multiply,
    Divide,
    Add,
    Subtract,

    Equal,
    NotEqual,

    GreaterEqual,
    LessEqual,
    Greater,
    Less,

    Or,
    And,
}

const PRECEDENCE: &[u8] = &[
    4,
    4,
    3,
    3,
    
    2,
    2,

    2,
    2,
    2,
    2,

    1,
    0,
];

const ASSOCIATIVITY: &[u8] = &[
    0,
    0,
    0,
    0,
       
    0,
    0,

    0,
    0,
    0,
    0,

    0,
    0,
];

#[derive(Debug)]
pub enum Value {
    Var(Variable),
    IntLit(IntLiteral)
}

#[derive(Debug)]
pub enum Expr {
    Expr(Box<Expr>, Operation, Box<Expr>),
    Value(Value)
}

impl Expr {
    pub fn get_value(&self) -> Option<&Value> {
        match self {
            Expr::Value(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum Stmt {
    ExitStmt(Expr),
    DeclarationStmt(Variable, Expr),
}

#[derive(Debug)]
pub enum SubIfStmt {
    Elif(Expr, Scope),
    Else(Scope)
}

#[derive(Debug)]
pub struct IfStmt {
    pub expr: Expr, 
    pub scope: Scope,
    pub sub_stmts: Vec<SubIfStmt>,
}

#[derive(Debug)]
pub enum ScopeItem {
    Scope(Scope),
    Stmt(Stmt),
    IfStmt(IfStmt),
}

#[derive(Debug)]
pub struct Scope {
    pub contents: Vec<ScopeItem>,
}

#[derive(Debug)]
pub struct AST {
    pub global: Scope,
}

#[derive(Debug)]
enum ParseStmtError {
    EndOfTokens,
    NotStmt,
    Syntax,
}

pub struct Parser;

impl Parser {
    fn parse_op(tokens: &mut VecDeque<Token>) -> Option<Operation> {
        match tokens[0] {
            Token::Plus => {
                tokens.pop_front();
                Some(Operation::Add)
            }
            Token::Asterix => {
                tokens.pop_front();
                Some(Operation::Multiply)
            }
            Token::Slash => {
                tokens.pop_front();
                Some(Operation::Divide)
            }
            Token::Minus => {
                tokens.pop_front();
                Some(Operation::Subtract)
            }
            Token::EqualSign => {
                tokens.pop_front();
                match tokens[0] {
                    Token::EqualSign => {
                        tokens.pop_front();
                        Some(Operation::Equal) 
                    } 
                    _ => {
                        println!("expected '=='");
                        None
                    }
                }
            }
            Token::Exclamation => {
                tokens.pop_front();
                match tokens[0] {
                    Token::EqualSign => {
                        tokens.pop_front();
                        Some(Operation::NotEqual) 
                    } 
                    _ => {
                        println!("expected '!='");
                        None
                    }
                }
            }
            Token::Ampersand => {
                tokens.pop_front();
                if let Some(Token::Ampersand) = tokens.pop_front() {
                    Some(Operation::And)
                } else {
                    println!("expected '&&'");
                    None
                }
            }
            Token::VertBar => {
                tokens.pop_front();
                if let Some(Token::VertBar) = tokens.pop_front() {
                    Some(Operation::Or)
                } else {
                    println!("expected '||'");
                    None
                }
            }
            Token::GreaterThan => {
                tokens.pop_front();

                match tokens[0] {
                    Token::EqualSign => {
                        tokens.pop_front();
                        Some(Operation::GreaterEqual) 
                    } 
                    _ => Some(Operation::Greater)
                }
            }
            Token::LessThan => {
                tokens.pop_front();

                match tokens[0] {
                    Token::EqualSign => {
                        tokens.pop_front();
                        Some(Operation::LessEqual) 
                    } 
                    _ => Some(Operation::Less)
                }
            }
            _ => {
                None
            }
        }
    }

    fn compute_atom(tokens: &mut VecDeque<Token>) -> Option<Expr> {
        let a = tokens.pop_front()?;

        if let Token::LeftParen = a {
            let expr = Self::parse_expr(tokens, 0);

            if let Token::RightParen = tokens[0] {
                tokens.pop_front();
                return expr;
            }

            println!("expected ')'");
            return None;
        }

        return Some(Expr::Value(a.get_value()?)); 
    }
    
    fn parse_expr(tokens: &mut VecDeque<Token>, min_prec: u8) -> Option<Expr> {
        let mut result = Self::compute_atom(tokens)?;
            
        let mut op_pres = 0;

        while op_pres >= min_prec {
            let Some(op) = Self::parse_op(tokens) else {
                return Some(result);
            };
        
            op_pres = PRECEDENCE[op as usize];

            result = Expr::Expr(
                Box::new(result),
                op, 
                Box::new(Self::parse_expr(tokens, op_pres + ASSOCIATIVITY[op as usize])?),
            );
        }        

        return Some(result);
    }

    fn parse_stmt(tokens: &mut VecDeque<Token>) -> Result<Stmt, ParseStmtError> {
        if tokens.len() == 0 {
            return Err(ParseStmtError::EndOfTokens);
        }

        match tokens[0] {
            Token::Exit => {
                tokens.pop_front();

                let next = tokens.pop_front();
                let Some(Token::LeftParen) = next else { 
                    println!("expected ( after exit");
                    return Err(ParseStmtError::Syntax);
                };

                let result = if let Some(expr) = Parser::parse_expr(tokens, 0) {
                    Stmt::ExitStmt(expr)
                } else {
                    return Err(ParseStmtError::Syntax);
                };
                
                let next = tokens.pop_front();
                let Some(Token::RightParen) = next else { 
                    println!("expected )");
                    return Err(ParseStmtError::Syntax);
                };
                
                let next = tokens.pop_front();
                let Some(Token::Semicolon) = next else { 
                    println!("expected ;");
                    return Err(ParseStmtError::Syntax);
                };

                return Ok(result);
            }
            Token::LetDeclaration => {
                tokens.pop_front();

                let next = tokens.pop_front();
                let Some(Token::VarName(dest)) = next else { 
                    println!("expected variable name");
                    return Err(ParseStmtError::Syntax);
                };

                let next = tokens.pop_front();
                let Some(Token::EqualSign) = next else { 
                    println!("expected =");
                    return Err(ParseStmtError::Syntax);
                };
                
                let result = if let Some(expr) = Parser::parse_expr(tokens, 0) {
                    Stmt::DeclarationStmt(dest, expr)
                } else {
                    return Err(ParseStmtError::Syntax);
                };

                let next = tokens.pop_front();
                let Some(Token::Semicolon) = next else { 
                    println!("expected ;");
                    return Err(ParseStmtError::Syntax);
                };

                return Ok(result);
            }
            _ => {}
        }

        return Err(ParseStmtError::NotStmt);
    }

    fn parse_if(tokens: &mut VecDeque<Token>) -> Option<IfStmt> {
        tokens.pop_front();
        let expr = Self::parse_expr(tokens, 0)?;
        let scope = Self::parse_scope(tokens, true)?;
        let mut ifstmt = IfStmt{ expr, scope, sub_stmts: vec![] };

        loop {
            match &tokens[0] {
                Token::Elif => {
                    tokens.pop_front();
                    let expr = Self::parse_expr(tokens, 0)?;
                    let scope = Self::parse_scope(tokens, true)?;
                    
                    let elifstmt = SubIfStmt::Elif(expr, scope);
                    ifstmt.sub_stmts.push(elifstmt);
                },
                Token::Else => {
                    tokens.pop_front();
                    let scope = Self::parse_scope(tokens, true)?;
                    ifstmt.sub_stmts.push(SubIfStmt::Else(scope));
                    return Some(ifstmt);
                }
                _ => {
                    return Some(ifstmt);
                }
            }
        }
    }

    fn parse_scope(tokens: &mut VecDeque<Token>, expect_paren: bool) -> Option<Scope> {
        if expect_paren {
            tokens.pop_front();
        }

        let mut contents: Vec<ScopeItem> = vec![];

        loop {
            match Self::parse_stmt(tokens) {
                Ok(stmt) => {
                    contents.push(ScopeItem::Stmt(stmt));
                    continue;
                }
                Err(err) => match err {
                    ParseStmtError::Syntax => { return None },
                    ParseStmtError::NotStmt => {},
                    ParseStmtError::EndOfTokens => { break; }
                }
            }

            match &tokens[0] {
                Token::LeftCurlyParen => {
                    contents.push(ScopeItem::Scope(Self::parse_scope(tokens, true)?));
                }
                Token::If => {
                    contents.push(ScopeItem::IfStmt(Self::parse_if(tokens)?));
                }
                _ => { break; }
            }
        }

        if expect_paren {
            if let Some(Token::RightCurlyParen) = tokens.pop_front() {
                Some(Scope { contents })
            } else {
                println!("expected '}}'");
                None
            }
        } else {
            Some(Scope { contents })
        }
    }

    pub fn parse(mut tokens: VecDeque<Token>) -> Option<AST> {
        Some(AST { global: Self::parse_scope(&mut tokens, false)? })
    }
}

impl Token {
    fn get_value(self) -> Option<Value> {
        if let Token::IntLit(intlit) = self {
            return Some(Value::IntLit(intlit));
        }
        if let Token::VarName(varname) = self {
            return Some(Value::Var(varname));
        }
        println!("expected value got {:?}", self);
        return None
    }
}
