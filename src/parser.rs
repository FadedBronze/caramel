use std::collections::VecDeque;

use crate::tokenizer::{IntLiteral, Token, Variable};

#[derive(Debug, Clone, Copy)]
pub enum NodeOperation {
    As,

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
    5,

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
    1,

    1,
    1,
    1,
    1,
       
    1,
    1,

    1,
    1,
    1,
    1,

    1,
    1,
];

#[derive(Debug, Clone, Copy)]
pub enum NodeNumber {
    FloatLiteral(f64),
    IntLiteral(i64),
    UintLiteral(u64)
}

#[derive(Debug)]
pub enum NodeValue {
    Var(Variable),
    Number(NodeNumber),
    Type(NodePrimativeType),
}

#[derive(Debug)]
pub enum NodeExpr {
    Expr {
        lhs: Box<NodeExpr>, 
        rhs: Box<NodeExpr>, 
        op: NodeOperation, 
    },
    Value(NodeValue)
}

impl NodeExpr {
    pub fn get_value(&self) -> Option<&NodeValue> {
        match self {
            NodeExpr::Value(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum NodePrimativeType {
    Uint64,
    Int64,
    Float64
}

#[derive(Debug)]
pub enum NodeStmt {
    ExitStmt(NodeExpr),
    DeclarationStmt(Variable, NodePrimativeType, NodeExpr),
    AssignmentStmt(Variable, NodeExpr),
}

#[derive(Debug)]
pub enum NodeSubIfStmt {
    Elif(NodeExpr, NodeScope),
    Else(NodeScope)
}

#[derive(Debug)]
pub struct NodeIfStmt {
    pub expr: NodeExpr, 
    pub scope: NodeScope,
    pub sub_stmts: Vec<NodeSubIfStmt>,
}

#[derive(Debug)]
pub enum NodeScopeItem {
    Scope(NodeScope),
    Stmt(NodeStmt),
    IfStmt(NodeIfStmt),
    WhileLoop(NodeExpr, NodeScope),
}

#[derive(Debug)]
pub struct NodeScope {
    pub contents: Vec<NodeScopeItem>,
}

#[derive(Debug)]
pub struct AbstractSyntaxTree {
    pub global: NodeScope,
}

#[derive(Debug)]
enum ParseStmtError {
    EndOfTokens,
    NotStmt,
    Syntax,
}

pub struct Parser;

impl Parser {
    fn parse_op(tokens: &mut VecDeque<Token>) -> Option<NodeOperation> {
        match tokens[0] {
            Token::Plus => {
                tokens.pop_front();
                Some(NodeOperation::Add)
            }
            Token::As => {
                tokens.pop_front();
                Some(NodeOperation::As)
            }
            Token::Asterix => {
                tokens.pop_front();
                Some(NodeOperation::Multiply)
            }
            Token::Slash => {
                tokens.pop_front();
                Some(NodeOperation::Divide)
            }
            Token::Minus => {
                tokens.pop_front();
                Some(NodeOperation::Subtract)
            }
            Token::EqualSign => {
                tokens.pop_front();
                match tokens[0] {
                    Token::EqualSign => {
                        tokens.pop_front();
                        Some(NodeOperation::Equal) 
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
                        Some(NodeOperation::NotEqual) 
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
                    Some(NodeOperation::And)
                } else {
                    println!("expected '&&'");
                    None
                }
            }
            Token::VertBar => {
                tokens.pop_front();
                if let Some(Token::VertBar) = tokens.pop_front() {
                    Some(NodeOperation::Or)
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
                        Some(NodeOperation::GreaterEqual) 
                    } 
                    _ => Some(NodeOperation::Greater)
                }
            }
            Token::LessThan => {
                tokens.pop_front();

                match tokens[0] {
                    Token::EqualSign => {
                        tokens.pop_front();
                        Some(NodeOperation::LessEqual) 
                    } 
                    _ => Some(NodeOperation::Less)
                }
            }
            _ => {
                None
            }
        }
    }

    fn parse_number_lit(tokens: &mut VecDeque<Token>) -> Option<NodeNumber> {
        let mut sign: i64 = 1;
        let mut num = 0;

        if let Token::Minus = tokens[0] {
            tokens.pop_front();
            sign = -1;
        }

        let Some(int_lit) = tokens.pop_front() else { 
            println!("expected number"); 
            return None; 
        };

        if let Token::IntLit(IntLiteral(int_lit)) = int_lit {
            num = int_lit
        }
        
        if let Token::Dot = tokens[0] {
            tokens.pop_front();

            let Some(Token::IntLit(IntLiteral(int_lit))) = tokens.pop_front() else {
                println!("expected {}.0", num);
                return None;
            };

            let num: f64 = num as f64;
            let int_lit: f64 = int_lit as f64;
            let sign: f64 = sign as f64;
            let denom = (10.0 as f64).powi(int_lit.log10().ceil() as i32);
            let decimal = int_lit * if denom > 0.0 { 1.0/denom } else { 0.0 };

            return Some(NodeNumber::FloatLiteral((num + decimal) * sign))
        }
        
        if let Token::U = tokens[0] {
            if sign == -1 {
                println!("negative numbers aren't supported in unsigned datatypes");
                return None;
            }
            tokens.pop_front();

            return Some(NodeNumber::IntLiteral(num * sign))
        }

        return Some(NodeNumber::IntLiteral(num * sign))
    }

    fn parse_value(tokens: &mut VecDeque<Token>) -> Option<NodeValue> {
        match tokens[0] {
            Token::VarName(_) => {
                let Some(Token::VarName(name)) = tokens.pop_front() else { return None };
                Some(NodeValue::Var(name))
            }
            Token::IntLit(_) | Token::Minus => {
                Some(NodeValue::Number(Self::parse_number_lit(tokens)?))
            }
            _ => {
                if let Some(typehint) = Self::parse_type(tokens) {
                    Some(NodeValue::Type(typehint))
                } else {
                    println!("expected value got: {:?}", tokens[0]);
                    None
                }
            }
        }
    }

    fn compute_atom(tokens: &mut VecDeque<Token>) -> Option<NodeExpr> {
        if let Token::LeftParen = tokens[0] {
            tokens.pop_front();

            let expr = Self::parse_expr(tokens, 0);

            if let Token::RightParen = tokens[0] {
                tokens.pop_front();
                return expr;
            }

            println!("expected ')'");
            return None;
        }

        return Some(NodeExpr::Value(Self::parse_value(tokens)?)); 
    }
    
    fn parse_expr(tokens: &mut VecDeque<Token>, min_prec: u8) -> Option<NodeExpr> {
        let mut result = Self::compute_atom(tokens)?;
            
        loop {
            let Some(op) = Self::parse_op(tokens) else {
                break;
            };
            
            let op_pres = PRECEDENCE[op as usize];

            if op_pres < min_prec {
                break;
            }    

            result = NodeExpr::Expr {
                lhs: Box::new(result),
                rhs: Box::new(Self::parse_expr(tokens, op_pres + ASSOCIATIVITY[op as usize])?),
                op, 
            };
        }        

        return Some(result);
    }

    fn parse_type(tokens: &mut VecDeque<Token>) -> Option<NodePrimativeType> {
        match tokens[0] {
            Token::Int64 => {
                tokens.pop_front();
                Some(NodePrimativeType::Int64)
            }
            Token::Uint64 => {
                tokens.pop_front();
                Some(NodePrimativeType::Uint64)
            }
            Token::Float64 => {
                tokens.pop_front();
                Some(NodePrimativeType::Float64)
            }
            _ => {
                println!("expected type indicator");
                return None;
            }
        }
    }

    fn parse_stmt(tokens: &mut VecDeque<Token>) -> Result<NodeStmt, ParseStmtError> {
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
                    NodeStmt::ExitStmt(expr)
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
                    println!("expected ; got {:?}", next);
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
                let Some(Token::Colon) = next else { 
                    println!("expected :");
                    return Err(ParseStmtError::Syntax);
                };
                
                let Some(typehint) = Self::parse_type(tokens) else { return Err(ParseStmtError::Syntax) };

                let next = tokens.pop_front();
                let Some(Token::EqualSign) = next else { 
                    println!("expected =");
                    return Err(ParseStmtError::Syntax);
                };
                
                let result = if let Some(expr) = Parser::parse_expr(tokens, 0) {
                    NodeStmt::DeclarationStmt(dest, typehint, expr)
                } else {
                    return Err(ParseStmtError::Syntax);
                };

                let next = tokens.pop_front();
                let Some(Token::Semicolon) = next else { 
                    println!("expected ; got {:?}", next);
                    return Err(ParseStmtError::Syntax);
                };

                return Ok(result);
            }
            Token::VarName(_) => {
                let Some(Token::VarName(dest)) = tokens.pop_front() else { panic!("") };

                let next = tokens.pop_front();
                let Some(Token::EqualSign) = next else { 
                    println!("expected =");
                    return Err(ParseStmtError::Syntax);
                };
                
                let result = if let Some(expr) = Parser::parse_expr(tokens, 0) {
                    NodeStmt::AssignmentStmt(dest.clone(), expr)
                } else {
                    return Err(ParseStmtError::Syntax);
                };

                let next = tokens.pop_front();
                let Some(Token::Semicolon) = next else { 
                    println!("expected ; got {:?}", next);
                    return Err(ParseStmtError::Syntax);
                };

                return Ok(result);
            }
            _ => {}
        }

        return Err(ParseStmtError::NotStmt);
    }

    fn parse_if(tokens: &mut VecDeque<Token>) -> Option<NodeIfStmt> {
        tokens.pop_front();
        let expr = Self::parse_expr(tokens, 0)?;
        let scope = Self::parse_scope(tokens, true)?;
        let mut ifstmt = NodeIfStmt{ expr, scope, sub_stmts: vec![] };

        loop {
            match &tokens[0] {
                Token::Elif => {
                    tokens.pop_front();
                    let expr = Self::parse_expr(tokens, 0)?;
                    let scope = Self::parse_scope(tokens, true)?;
                    
                    let elifstmt = NodeSubIfStmt::Elif(expr, scope);
                    ifstmt.sub_stmts.push(elifstmt);
                },
                Token::Else => {
                    tokens.pop_front();
                    let scope = Self::parse_scope(tokens, true)?;
                    ifstmt.sub_stmts.push(NodeSubIfStmt::Else(scope));
                    return Some(ifstmt);
                }
                _ => {
                    return Some(ifstmt);
                }
            }
        }
    }

    fn parse_scope(tokens: &mut VecDeque<Token>, expect_paren: bool) -> Option<NodeScope> {
        if expect_paren {
            tokens.pop_front();
        }

        let mut contents: Vec<NodeScopeItem> = vec![];

        loop {
            match Self::parse_stmt(tokens) {
                Ok(stmt) => {
                    contents.push(NodeScopeItem::Stmt(stmt));
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
                    contents.push(NodeScopeItem::Scope(Self::parse_scope(tokens, true)?));
                }
                Token::While => {
                    tokens.pop_front();
                    let expr = Self::parse_expr(tokens, 0)?;
                    let scope = Self::parse_scope(tokens, true)?;
                    contents.push(NodeScopeItem::WhileLoop(expr, scope));
                }
                Token::If => {
                    contents.push(NodeScopeItem::IfStmt(Self::parse_if(tokens)?));
                }
                _ => { break; }
            }
        }

        if expect_paren {
            if let Some(Token::RightCurlyParen) = tokens.pop_front() {
                Some(NodeScope { contents })
            } else {
                println!("expected '}}'");
                None
            }
        } else {
            Some(NodeScope { contents })
        }
    }

    pub fn parse(mut tokens: VecDeque<Token>) -> Option<AbstractSyntaxTree> {
        Some(AbstractSyntaxTree { global: Self::parse_scope(&mut tokens, false)? })
    }
}

