mod nonrecursive;
mod scopes;

use std::{collections::VecDeque, error::Error};

use crate::tokenizer::{Token, VarName};


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NodeNumber {
    FloatLiteral(f64),
    IntLiteral(i64),
}

#[derive(Debug, PartialEq)]
pub enum NodeValue {
    Var(VarName),
    Number(NodeNumber),
    Type(NodePrimativeType),
    FuncCall(FunctionCall),
}

#[derive(Debug, PartialEq)]
pub enum NodeExpr {
    Expr {
        lhs: Box<NodeExpr>, 
        rhs: Box<NodeExpr>, 
        op: NodeOperation, 
    },
    Value(NodeValue),
}

impl NodeExpr {
    pub fn get_value(&self) -> Option<&NodeValue> {
        match self {
            NodeExpr::Value(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum NodePrimativeType {
    Uint64,
    Int64,
    Float64,
    Unknown,
}

#[derive(Debug)]
pub enum NodeStmt {
    ExitStmt(NodeExpr),
    DeclarationStmt(VarName, NodeExpr),
    AssignmentStmt(VarName, NodeExpr),
    Expr(NodeExpr),
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
pub struct VariableDeclaration(VarName, NodePrimativeType);

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: VarName,
    pub return_type: NodePrimativeType,
    pub params: Vec<VariableDeclaration>,
    pub scope: NodeScope,
}

#[derive(Debug, PartialEq)]
pub struct FunctionCall {
    name: VarName,
    params: Vec<NodeExpr>,
}

#[derive(Debug)]
pub enum NodeScopeItem {
    Scope(NodeScope),
    Stmt(NodeStmt),
    IfStmt(NodeIfStmt),
    WhileLoop(NodeExpr, NodeScope),
    FuncDec(FunctionDeclaration),
    FuncCall(FunctionCall),
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
    fn take_if(&mut self, token: Token) -> Option<Token> {
        if token == self.tokens[0]  {
            return self.tokens.pop_front();
        }
        return None
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
            if self.tokens.len() == 0 {
                break;
            }

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
                    println!("expected ')' got {:?}", next);
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
                
                if let Token::Colon = self.tokens[0] { 
                    self.tokens.pop_front();

                    let Some(typehint) = self.parse_type() else { return Err(ParseStmtError::Syntax) };
                };

                let next = self.tokens.pop_front();
                let Some(Token::EqualSign) = next else { 
                    println!("expected = got {:?}", next);
                    return Err(ParseStmtError::Syntax);
                };
                
                let result = if let Some(expr) = self.parse_expr(0) {
                    NodeStmt::DeclarationStmt(dest, expr)
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
                if let Token::EqualSign = self.tokens[1] {
                    let Some(Token::VarName(dest)) = self.tokens.pop_front() else { panic!("") };

                    let next = self.tokens.pop_front();
                    let Some(Token::EqualSign) = next else { 
                        println!("expected = got {:?}", next);
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
                } else {
                    let func = self.parse_fn_call();
                    if let None = func {
                        return Err(ParseStmtError::Syntax)
                    }
                    return Ok(NodeStmt::Expr(NodeExpr::Value(NodeValue::FuncCall(func.unwrap()))))
                }
            }
            _ => {}
        }

        return Err(ParseStmtError::NotStmt);
    }

    pub fn parse(&mut self) -> Option<AbstractSyntaxTree> {
        Some(AbstractSyntaxTree { global: self.parse_scope(false)? })
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use crate::{parser::{NodeExpr, NodeOperation, NodeValue}, tokenizer::{Token, VarName}};

    use super::Parser;

    #[test]
    fn test_parse_expr() {
        {
            let tokens = VecDeque::from(vec![
                Token::VarName(VarName("x".to_string())),
                Token::Plus,
                Token::VarName(VarName("y".to_string())),
                Token::Asterix,
                Token::VarName(VarName("z".to_string())),
            ]);
     
            let expected_expr = NodeExpr::Expr { 
                lhs: Box::new(NodeExpr::Value(NodeValue::Var(VarName("x".to_string())))), 
                rhs: Box::new(NodeExpr::Expr { 
                    lhs: Box::new(NodeExpr::Value(NodeValue::Var(VarName("y".to_string())))), 
                    rhs: Box::new(NodeExpr::Value(NodeValue::Var(VarName("z".to_string())))), 
                    op: NodeOperation::Multiply, 
                }), 
                op: NodeOperation::Add, 
            };

            let expr = Parser::new(tokens).parse_expr(0);
            assert_eq!(expr, Some(expected_expr));
        }
        {
            let tokens = VecDeque::from(vec![
                Token::LeftParen,
                Token::VarName(VarName("x".to_string())),
                Token::Plus,
                Token::VarName(VarName("y".to_string())),
                Token::RightParen,
                Token::Asterix,
                Token::VarName(VarName("z".to_string())),
            ]);
     
            let expected_expr = NodeExpr::Expr { 
                lhs: Box::new(NodeExpr::Expr { 
                    lhs: Box::new(NodeExpr::Value(NodeValue::Var(VarName("x".to_string())))), 
                    rhs: Box::new(NodeExpr::Value(NodeValue::Var(VarName("y".to_string())))), 
                    op: NodeOperation::Add, 
                }), 
                rhs: Box::new(NodeExpr::Value(NodeValue::Var(VarName("z".to_string())))), 
                op: NodeOperation::Multiply, 
            };

            let mut parser = Parser::new(tokens); 
            let expr = parser.parse_expr(0);

            assert_eq!(parser.tokens, VecDeque::from(vec![]));
            assert_eq!(expr, Some(expected_expr));
        }
        {
            let tokens = VecDeque::from(vec![
                Token::LeftParen,
                Token::LeftParen,
                Token::LeftParen,
                Token::LeftParen,
                Token::LeftParen,
                Token::VarName(VarName("x".to_string())),
                Token::Plus,
                Token::LeftParen,
                Token::LeftParen,
                Token::VarName(VarName("y".to_string())),
                Token::Asterix,
                Token::VarName(VarName("z".to_string())),
                Token::RightParen,
                Token::RightParen,
                Token::RightParen,
                Token::RightParen,
                Token::RightParen,
                Token::RightParen,
                Token::RightParen,
            ]);
     
            let expected_expr = NodeExpr::Expr { 
                lhs: Box::new(NodeExpr::Value(NodeValue::Var(VarName("x".to_string())))), 
                rhs: Box::new(NodeExpr::Expr { 
                    lhs: Box::new(NodeExpr::Value(NodeValue::Var(VarName("y".to_string())))), 
                    rhs: Box::new(NodeExpr::Value(NodeValue::Var(VarName("z".to_string())))), 
                    op: NodeOperation::Multiply, 
                }), 
                op: NodeOperation::Add, 
            };

            let mut parser = Parser::new(tokens); 
            let expr = parser.parse_expr(0);

            assert_eq!(parser.tokens, VecDeque::from(vec![]));
            assert_eq!(expr, Some(expected_expr));
        }
        {
            let tokens = VecDeque::from(vec![
                Token::LeftParen,
                Token::VarName(VarName("x".to_string())),
                Token::Plus,
                Token::LeftParen,
                Token::VarName(VarName("y".to_string())),
                Token::Asterix,
                Token::VarName(VarName("z".to_string())),
                Token::RightParen,
                Token::RightParen,

                Token::RightParen,
                Token::RightParen,
            ]);
     
            let expected_expr = NodeExpr::Expr { 
                lhs: Box::new(NodeExpr::Value(NodeValue::Var(VarName("x".to_string())))), 
                rhs: Box::new(NodeExpr::Expr { 
                    lhs: Box::new(NodeExpr::Value(NodeValue::Var(VarName("y".to_string())))), 
                    rhs: Box::new(NodeExpr::Value(NodeValue::Var(VarName("z".to_string())))), 
                    op: NodeOperation::Multiply, 
                }), 
                op: NodeOperation::Add, 
            };

            let mut parser = Parser::new(tokens);
            let expr = parser.parse_expr(0);

            assert_eq!(parser.tokens, VecDeque::from(vec![Token::RightParen, Token::RightParen]));
            assert_eq!(expr, Some(expected_expr));
        }
    }
}
