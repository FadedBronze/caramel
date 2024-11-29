use crate::tokenizer::{IntLiteral, Token};
use super::{NodeNumber, NodeOperation, NodePrimativeType, NodeValue, Parser};

impl Parser {
    pub(super) fn parse_value(&mut self) -> Option<NodeValue> {
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

    pub(super) fn parse_number_lit(&mut self) -> Option<NodeNumber> {
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
        
        return Some(NodeNumber::IntLiteral(num * sign))
    }

    pub(super) fn parse_type(&mut self) -> Option<NodePrimativeType> {
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

    pub(super) fn parse_op(&mut self) -> Option<NodeOperation> {
        assert_ne!(self.tokens.len(), 0);

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
}
