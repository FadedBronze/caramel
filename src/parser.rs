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

pub struct Parser {
    tokens: VecDeque<Token>,
}

impl Parser {
    pub fn new(tokens: VecDeque<Token>) -> Parser {
        Parser { tokens }
    }
    fn parse_op(&mut self) -> Option<NodeOperation> {
        match self.tokens[0] {
            Token::Plus => {
                self.tokens.pop_front();
                Some(NodeOperation::Add)
            }
            Token::As => {
                self.tokens.pop_front();
                Some(NodeOperation::As)
            }
            Token::Asterix => {
                self.tokens.pop_front();
                Some(NodeOperation::Multiply)
            }
            Token::Slash => {
                self.tokens.pop_front();
                Some(NodeOperation::Divide)
            }
            Token::Minus => {
                self.tokens.pop_front();
                Some(NodeOperation::Subtract)
            }
            Token::EqualSign => {
                self.tokens.pop_front();
                match self.tokens[0] {
                    Token::EqualSign => {
                        self.tokens.pop_front();
                        Some(NodeOperation::Equal) 
                    } 
                    _ => {
                        println!("expected '=='");
                        None
                    }
                }
            }
            Token::Exclamation => {
                self.tokens.pop_front();
                match self.tokens[0] {
                    Token::EqualSign => {
                        self.tokens.pop_front();
                        Some(NodeOperation::NotEqual) 
                    } 
                    _ => {
                        println!("expected '!='");
                        None
                    }
                }
            }
            Token::Ampersand => {
                self.tokens.pop_front();
                if let Some(Token::Ampersand) = self.tokens.pop_front() {
                    Some(NodeOperation::And)
                } else {
                    println!("expected '&&'");
                    None
                }
            }
            Token::VertBar => {
                self.tokens.pop_front();
                if let Some(Token::VertBar) = self.tokens.pop_front() {
                    Some(NodeOperation::Or)
                } else {
                    println!("expected '||'");
                    None
                }
            }
            Token::GreaterThan => {
                self.tokens.pop_front();

                match self.tokens[0] {
                    Token::EqualSign => {
                        self.tokens.pop_front();
                        Some(NodeOperation::GreaterEqual) 
                    } 
                    _ => Some(NodeOperation::Greater)
                }
            }
            Token::LessThan => {
                self.tokens.pop_front();

                match self.tokens[0] {
                    Token::EqualSign => {
                        self.tokens.pop_front();
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

    fn parse_number_lit(&mut self) -> Option<NodeNumber> {
        let mut sign: i64 = 1;
        let mut num = 0;

        if let Token::Minus = self.tokens[0] {
            self.tokens.pop_front();
            sign = -1;
        }

        let Some(int_lit) = self.tokens.pop_front() else { 
            println!("expected number"); 
            return None; 
        };

        if let Token::IntLit(IntLiteral(int_lit)) = int_lit {
            num = int_lit
        }
        
        if let Token::Dot = self.tokens[0] {
            self.tokens.pop_front();

            let Some(Token::IntLit(IntLiteral(int_lit))) = self.tokens.pop_front() else {
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
        
        if let Token::U = self.tokens[0] {
            if sign == -1 {
                println!("negative numbers aren't supported in unsigned datatypes");
                return None;
            }
            self.tokens.pop_front();

            return Some(NodeNumber::IntLiteral(num * sign))
        }

        return Some(NodeNumber::IntLiteral(num * sign))
    }

    fn parse_value(&mut self) -> Option<NodeValue> {
        match self.tokens[0] {
            Token::VarName(_) => {
                let Some(Token::VarName(name)) = self.tokens.pop_front() else { return None };
                Some(NodeValue::Var(name))
            }
            Token::IntLit(_) | Token::Minus => {
                Some(NodeValue::Number(self.parse_number_lit()?))
            }
            _ => {
                if let Some(typehint) = self.parse_type() {
                    Some(NodeValue::Type(typehint))
                } else {
                    println!("expected value got: {:?}", self.tokens[0]);
                    None
                }
            }
        }
    }

    fn compute_atom(&mut self) -> Option<NodeExpr> {
        if let Token::LeftParen = self.tokens[0] {
            self.tokens.pop_front();

            let expr = self.parse_expr(0);

            if let Token::RightParen = self.tokens[0] {
                self.tokens.pop_front();
                return expr;
            }

            println!("expected ')'");
            return None;
        }

        return Some(NodeExpr::Value(self.parse_value()?)); 
    }
    
    fn parse_expr(&mut self, min_prec: u8) -> Option<NodeExpr> {
        let mut result = self.compute_atom()?;
           
        loop {
            let Some(op) = self.parse_op() else {
                break;
            };
            
            let op_pres = PRECEDENCE[op as usize];

            if op_pres < min_prec {
                break;
            }    

            let expr = self.parse_expr(op_pres + ASSOCIATIVITY[op as usize])?;
        
            result = NodeExpr::Expr {
                lhs: Box::new(result),
                rhs: Box::new(expr),
                op, 
            };
        }        

        return Some(result);
    }

    fn parse_type(&mut self) -> Option<NodePrimativeType> {
        match self.tokens[0] {
            Token::Int64 => {
                self.tokens.pop_front();
                Some(NodePrimativeType::Int64)
            }
            Token::Uint64 => {
                self.tokens.pop_front();
                Some(NodePrimativeType::Uint64)
            }
            Token::Float64 => {
                self.tokens.pop_front();
                Some(NodePrimativeType::Float64)
            }
            _ => {
                println!("expected type indicator");
                return None;
            }
        }
    }

    fn parse_stmt(&mut self) -> Result<NodeStmt, ParseStmtError> {
        if self.tokens.len() == 0 {
            return Err(ParseStmtError::EndOfTokens);
        }

        match self.tokens[0] {
            Token::Exit => {
                self.tokens.pop_front();

                let next = self.tokens.pop_front();
                let Some(Token::LeftParen) = next else { 
                    println!("expected ( after exit");
                    return Err(ParseStmtError::Syntax);
                };

                let result = if let Some(expr) = self.parse_expr(0) {
                    NodeStmt::ExitStmt(expr)
                } else {
                    return Err(ParseStmtError::Syntax);
                };
                
                let next = self.tokens.pop_front();
                let Some(Token::RightParen) = next else { 
                    println!("expected )");
                    return Err(ParseStmtError::Syntax);
                };
                
                let next = self.tokens.pop_front();
                let Some(Token::Semicolon) = next else { 
                    println!("expected ; got {:?}", next);
                    return Err(ParseStmtError::Syntax);
                };

                return Ok(result);
            }
            Token::LetDeclaration => {
                self.tokens.pop_front();

                let next = self.tokens.pop_front();
                let Some(Token::VarName(dest)) = next else { 
                    println!("expected variable name");
                    return Err(ParseStmtError::Syntax);
                };
                
                let next = self.tokens.pop_front();
                let Some(Token::Colon) = next else { 
                    println!("expected :");
                    return Err(ParseStmtError::Syntax);
                };
                
                let Some(typehint) = self.parse_type() else { return Err(ParseStmtError::Syntax) };

                let next = self.tokens.pop_front();
                let Some(Token::EqualSign) = next else { 
                    println!("expected =");
                    return Err(ParseStmtError::Syntax);
                };
                
                let result = if let Some(expr) = self.parse_expr(0) {
                    NodeStmt::DeclarationStmt(dest, typehint, expr)
                } else {
                    return Err(ParseStmtError::Syntax);
                };

                let next = self.tokens.pop_front();
                let Some(Token::Semicolon) = next else { 
                    println!("expected ; got {:?}", next);
                    return Err(ParseStmtError::Syntax);
                };

                return Ok(result);
            }
            Token::VarName(_) => {
                let Some(Token::VarName(dest)) = self.tokens.pop_front() else { panic!("") };

                let next = self.tokens.pop_front();
                let Some(Token::EqualSign) = next else { 
                    println!("expected =");
                    return Err(ParseStmtError::Syntax);
                };
                
                let result = if let Some(expr) = self.parse_expr(0) {
                    NodeStmt::AssignmentStmt(dest.clone(), expr)
                } else {
                    return Err(ParseStmtError::Syntax);
                };

                let next = self.tokens.pop_front();
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

    fn parse_if(&mut self) -> Option<NodeIfStmt> {
        self.tokens.pop_front();
        let expr = self.parse_expr(0)?;
        let scope = self.parse_scope(true)?;
        let mut ifstmt = NodeIfStmt{ expr, scope, sub_stmts: vec![] };

        loop {
            match &self.tokens[0] {
                Token::Elif => {
                    self.tokens.pop_front();
                    let expr = self.parse_expr(0)?;
                    let scope = self.parse_scope(true)?;
                    
                    let elifstmt = NodeSubIfStmt::Elif(expr, scope);
                    ifstmt.sub_stmts.push(elifstmt);
                },
                Token::Else => {
                    self.tokens.pop_front();
                    let scope = self.parse_scope(true)?;
                    ifstmt.sub_stmts.push(NodeSubIfStmt::Else(scope));
                    return Some(ifstmt);
                }
                _ => {
                    return Some(ifstmt);
                }
            }
        }
    }

    fn parse_scope(&mut self, expect_paren: bool) -> Option<NodeScope> {
        if expect_paren {
            self.tokens.pop_front();
        }

        let mut contents: Vec<NodeScopeItem> = vec![];

        loop {
            match self.parse_stmt() {
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

            match &self.tokens[0] {
                Token::LeftCurlyParen => {
                    contents.push(NodeScopeItem::Scope(self.parse_scope(true)?));
                }
                Token::While => {
                    self.tokens.pop_front();
                    let expr = self.parse_expr(0)?;
                    let scope = self.parse_scope(true)?;
                    contents.push(NodeScopeItem::WhileLoop(expr, scope));
                }
                Token::If => {
                    contents.push(NodeScopeItem::IfStmt(self.parse_if()?));
                }
                _ => { break; }
            }
        }

        if expect_paren {
            if let Some(Token::RightCurlyParen) = self.tokens.pop_front() {
                Some(NodeScope { contents })
            } else {
                println!("expected '}}'");
                None
            }
        } else {
            Some(NodeScope { contents })
        }
    }

    pub fn parse(&mut self) -> Option<AbstractSyntaxTree> {
        Some(AbstractSyntaxTree { global: self.parse_scope(false)? })
    }
}

