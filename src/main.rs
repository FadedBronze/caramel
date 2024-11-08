use std::{collections::VecDeque, env, fs};

#[derive(Debug, Clone)]
struct Variable(String);
#[derive(Debug, Clone)]
struct IntLiteral(i64);

#[derive(Debug, Clone)]
enum Token {
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
                    "if" => {
                        tokens.push_back(Token::If)
                    },
                    "else" => {
                        tokens.push_back(Token::Else)
                    },
                    "elif" => {
                        tokens.push_back(Token::Elif)
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

#[derive(Debug, Clone, Copy)]
enum Operation {
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
enum Value {
    Var(Variable),
    IntLit(IntLiteral)
}

#[derive(Debug)]
enum Expr {
    Expr(Box<Expr>, Operation, Box<Expr>),
    Value(Value)
}

impl Expr {
    fn get_value(&self) -> Option<&Value> {
        match self {
            Expr::Value(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(Debug)]
enum Stmt {
    ExitStmt(Expr),
    DeclarationStmt(Variable, Expr),
}

#[derive(Debug)]
enum SubIfStmt {
    Elif(Expr, Scope),
    Else(Scope)
}

#[derive(Debug)]
struct IfStmt {
    expr: Expr, 
    scope: Scope,
    sub_stmts: Vec<SubIfStmt>,
}

#[derive(Debug)]
enum ScopeItem {
    Scope(Scope),
    Stmt(Stmt),
    IfStmt(IfStmt),
}

#[derive(Debug)]
struct Scope {
    contents: Vec<ScopeItem>,
}

#[derive(Debug)]
struct AST {
    global: Scope,
}

#[derive(Debug)]
enum ParseStmtError {
    EndOfTokens,
    NotStmt,
    Syntax,
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

    fn parse(mut tokens: VecDeque<Token>) -> Option<AST> {
        Some(AST { global: Self::parse_scope(&mut tokens, false)? })
    }
}

struct GenVar {
    name: String,
    stack_loc: i64,
}

struct GenScope {
    vars: Vec<GenVar>,
    stack_loc: i64,
}

struct Codegen {
    stack_size: i64,
    output_string: String,
    scopes: Vec<GenScope>,
}

//save stack loc
//pop for (i = stack pos - stack loc; i--)

impl Codegen {
    fn new() -> Self {
        Self {
            stack_size: 0,
            output_string: String::new(),
            scopes: vec![],
        }
    }
    
    fn add_label(&mut self, label: &str) {
        self.output_string += label;
        self.output_string += ":\n";
    }

    fn add_line(&mut self, line: &str) {
        self.output_string += "    ";
        self.output_string += line;
        self.output_string += "\n";
    }

    fn push(&mut self, reg: &str) {
        self.add_line(format!("push {}", reg).as_str());
        self.stack_size += 1;
    }
    
    fn pop(&mut self, reg: &str) {
        self.add_line(format!("pop {}", reg).as_str());
        self.stack_size -= 1;
    }
    
    fn gen_value(&mut self, value: &Value) {
        match value {
            Value::IntLit(IntLiteral(num)) => {
                self.add_line(format!("mov rax, {}", num).as_str());
                self.push("rax");
            }
            Value::Var(Variable(varname)) => {
                let var_position = self.get_var_offset(varname).unwrap();
                if self.stack_size - var_position == 0 {
                    self.add_line(format!("mov rax, [rsp] ; {}", varname).as_str());
                } else {
                    self.add_line(format!("mov rax, [rsp + {}] ; {}", 8 * (self.stack_size - var_position), varname).as_str());
                }
                self.push("rax");
            }
        }
    }
    
    fn gen_expr(&mut self, value: &Expr) {
        match &value {
            Expr::Value(value) => {
                self.gen_value(value)
            },
            Expr::Expr(a, op, b) => {
                if let Some(b) = b.get_value() {
                    self.gen_value(b);
                } else {
                    self.gen_expr(b);    
                }
                
                if let Some(a) = a.get_value() {
                    self.gen_value(a);
                } else {
                    self.gen_expr(a);    
                }

                self.pop("rax");
                self.pop("r8");

                match op {
                    Operation::Add => {
                        self.add_line("add rax, r8");
                    }
                    Operation::Multiply => {
                        self.add_line("mul r8");
                    }
                    Operation::Divide => {
                        self.add_line("mov rdx, 0");
                        self.add_line("div r8");
                    }
                    Operation::Subtract => {
                        self.add_line("sub rax, r8");
                    }

                    Operation::And => {
                        self.add_line("and al, r8b");
                        self.add_line("movzx rax, al");
                    }
                    Operation::Or => {
                        self.add_line("or al, r8b");
                        self.add_line("movzx rax, al");
                    }

                    Operation::Equal => {
                        self.add_line("cmp al, r8b");
                        self.add_line("sete al");
                        self.add_line("movzx rax, al");
                    }
                    Operation::NotEqual => {
                        self.add_line("cmp al, r8b");
                        self.add_line("setne al");
                        self.add_line("movzx rax, al");
                    }

                    Operation::Less => {
                        self.add_line("cmp al, r8b");
                        self.add_line("setl al");
                        self.add_line("movzx rax, al");
                    }
                    Operation::LessEqual => {
                        self.add_line("cmp al, r8b");
                        self.add_line("setle al");
                        self.add_line("movzx rax, al");
                    }
                    Operation::Greater => {
                        self.add_line("cmp al, r8b");
                        self.add_line("setg al");
                        self.add_line("movzx rax, al");
                    }
                    Operation::GreaterEqual => {
                        self.add_line("cmp al, r8b");
                        self.add_line("setge al");
                        self.add_line("movzx rax, al");
                    }
                }
                
                self.push("rax");
            }
        }
    }
    
    fn get_var_offset(&self, varname: &String) -> Option<i64> {
        for scope in self.scopes.iter().rev() {
            for var in scope.vars.iter() {
                if var.name == *varname {
                    return Some(var.stack_loc);
                }
            }
        }

        return None
    }

    fn create_var(&mut self, varname: String) {
        let last = self.scopes.len()-1;
        self.scopes[last].vars.push(GenVar {
            stack_loc: self.stack_size,
            name: varname,
        });
    }

    fn generate_stmt(&mut self, statement: &Stmt) {
        match &statement {
            Stmt::DeclarationStmt(Variable(varname), expr) => {
                self.gen_expr(expr);
                self.create_var(varname.to_string());
            }
            Stmt::ExitStmt(value) => {
                self.gen_expr(value);
                self.add_line("mov rax, 60");
                self.pop("rdi");
                self.add_line("syscall");
            }
        }
    }

    //do if a {} elif b {} else {}
    //
    //if a
    //je aa
    //
    //if b
    //je bb
    //
    //jmp x
    //
    //bb
    //jmp x
    //
    //aa
    //jmp x
    //x
    //
    //
    //if a
    //je aa
    //jmp x
    //aa
    //{}
    //x

    fn gen_if_recurse(&mut self, sub_stmts: &Vec<SubIfStmt>, position: usize, exit_addr: &str) {
        if sub_stmts.len() <= position {
            self.add_line(format!("jmp {}", exit_addr).as_str());
            return;
        }

        let sub_stmt = &sub_stmts[position];

        match sub_stmt {
            SubIfStmt::Elif(expr, scope) => {
                let addr = format!("elif_{}", rand::random::<u64>());
                self.gen_expr(&expr);
                self.pop("rax");
                self.add_line("test rax, rax");
                self.add_line(format!("jnz {}", addr).as_str());
                
                self.gen_if_recurse(sub_stmts, position+1, exit_addr);
        
                self.add_label(&addr);
                self.gen_scope(scope);
                
                self.add_line(format!("jmp {}", exit_addr).as_str());
                return;
            }
            SubIfStmt::Else(scope) => {
                self.gen_scope(scope);
                self.add_line(format!("jmp {}", exit_addr).as_str());
            }
        }
    }

    fn gen_if(&mut self, if_stmt: &IfStmt) {
        let IfStmt { expr, scope, sub_stmts } = if_stmt;

        let if_addr = format!("if_{}", rand::random::<u64>());
        let exit_addr = format!("exit_{}", rand::random::<u64>());

        self.gen_expr(expr);
        self.pop("rax");
        self.add_line("test rax, rax");
        self.add_line(format!("jnz {}", if_addr).as_str());
        
        self.gen_if_recurse(sub_stmts, 0, &exit_addr);

        self.add_label(&if_addr);
        self.gen_scope(scope);
        self.add_label(&exit_addr);
    }

    fn gen_scope(&mut self, scope: &Scope) {
        self.scopes.push(GenScope { 
            vars: vec![], 
            stack_loc: self.stack_size 
        });

        for item in scope.contents.iter() {
            match item {
                ScopeItem::Scope(scope) => {
                    self.gen_scope(scope);
                }
                ScopeItem::Stmt(stmt) => {
                    self.generate_stmt(stmt);
                }
                ScopeItem::IfStmt(stmt) => {
                    self.gen_if(stmt);
                }
            }
        }

        self.scopes.pop();
    }

    fn generate(&mut self, ast: &AST) -> String {
        self.output_string += "global _start\n";
        self.add_label("_start");

        self.gen_scope(&ast.global);

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
    println!("{:#?}", tokens);
    let ast = Parser::parse(tokens).unwrap();
    println!("{:#?}", ast);
    let program = Codegen::new().generate(&ast);
    println!("{}", program);
}
