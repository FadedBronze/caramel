use crate::tokenizer::Token;

use super::{FunctionCall, FunctionDeclaration, NodeIfStmt, NodePrimativeType, NodeScope, NodeScopeItem, NodeSubIfStmt, ParseStmtError, Parser, VariableDeclaration};

impl Parser {
    pub fn parse_fn_call(&mut self) -> Option<FunctionCall> {
        let Token::VarName(name) = self.tokens[0].clone() else {
            return None;
        };

        self.tokens.pop_front();

        let Some(..) = self.take_if(Token::LeftParen) else {
            println!("expected ')'");
            return None;
        };

        let mut vars = vec![];

        loop {
            let Some(expr) = self.parse_expr(0) else {
                break;
            };
            vars.push(expr);
            let Token::Comma = self.tokens[0] else { break; };
            self.tokens.pop_front();
        }

        let Some(..) = self.take_if(Token::RightParen) else {
            println!("expected ')'");
            return None;
        };

        Some(FunctionCall { name, params: vars })
    }

    pub(super) fn parse_scope(&mut self, expect_paren: bool) -> Option<NodeScope> {
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
                Token::Fn => {
                    contents.push(NodeScopeItem::FuncDec(self.parse_function_declaration()?));
                },
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
    
    fn parse_function_declaration(&mut self) -> Option<FunctionDeclaration> {
        let Token::Fn = self.tokens.pop_front()? else { return None };
        let token = self.tokens.pop_front()?;
        let Token::VarName(name) = token else { 
            println!("expected function name got {:?}", token);
            return None 
        };
        let Token::LeftParen = self.tokens.pop_front()? else { 
            println!("expected '('");
            return None 
        };

        let mut vars: Vec<VariableDeclaration> = vec![];

        loop {
            let Some(Token::VarName(var)) = self.tokens.pop_front() else { break; };
            let Some(Token::Colon) = self.tokens.pop_front() else { 
                println!("expected type declaration after ':'");
                return None 
            };
            let typehint = self.parse_type()?;
            vars.push(VariableDeclaration(var, typehint));
            let Token::Comma = self.tokens[0] else { break; };
            self.tokens.pop_front();
        }

        let Token::RightParen = self.tokens.pop_front()? else { 
            println!("expected ')' got {:?}", self.tokens[0]);
            return None 
        };
        
        let scope = self.parse_scope(true)?;

        Some(FunctionDeclaration {
            name,
            scope,
            params: vars,
            return_type: NodePrimativeType::Int64,
        })
    }
}

//samyat note: you haven't implemented callling functions or at least properly
