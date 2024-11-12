use crate::{parser::{NodeExpr, NodeIfStmt, NodeNumber, NodeOperation, NodeScope, NodeScopeItem, NodeStmt, NodeSubIfStmt, NodeValue, AbstractSyntaxTree}, tokenizer::Variable};

struct CodeGenerationVar {
    name: String,
    stack_loc: i64, 
}

struct CodeGenerationScope {
    vars: Vec<CodeGenerationVar>,
}

pub struct CodeGeneration {
    stack_size: i64,
    output_string: String,
    scopes: Vec<CodeGenerationScope>,
}

//save stack loc
//pop for (i = stack pos - stack loc; i--)

impl CodeGeneration {
    pub fn new() -> Self {
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
    
    fn gen_value(&mut self, value: &NodeValue) {
        match value {
            NodeValue::Number(NodeNumber::IntLiteral(num)) => {
                self.add_line(format!("mov rax, {}", num).as_str());
                self.push("rax");
            }
            NodeValue::Var(Variable(varname)) => {
                let var_position = self.get_var_stack_loc(varname).unwrap();
                if self.stack_size - var_position == 0 {
                    self.add_line(format!("mov rax, [rsp] ; {}", varname).as_str());
                } else {
                    self.add_line(format!("mov rax, [rsp + {}] ; {}", 8 * (self.stack_size - var_position), varname).as_str());
                }
                self.push("rax");
            }
            NodeValue::Type(_) => todo!(),
            NodeValue::Number(NodeNumber::FloatLiteral(_)) => todo!(),
        }
    }
    
    fn gen_expr(&mut self, value: &NodeExpr) {
        match &value {
            NodeExpr::Value(value) => {
                self.gen_value(value)
            },
            NodeExpr::Expr {
                lhs: a,
                rhs: b,
                op,
                ..
            } => {
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
                    NodeOperation::As => {}
                    NodeOperation::Add => {
                        self.add_line("add rax, r8");
                    }
                    NodeOperation::Multiply => {
                        self.add_line("mul r8");
                    }
                    NodeOperation::Divide => {
                        self.add_line("mov rdx, 0");
                        self.add_line("div r8");
                    }
                    NodeOperation::Subtract => {
                        self.add_line("sub rax, r8");
                    }

                    NodeOperation::And => {
                        self.add_line("and al, r8b");
                        self.add_line("movzx rax, al");
                    }
                    NodeOperation::Or => {
                        self.add_line("or al, r8b");
                        self.add_line("movzx rax, al");
                    }

                    NodeOperation::Equal => {
                        self.add_line("cmp al, r8b");
                        self.add_line("sete al");
                        self.add_line("movzx rax, al");
                    }
                    NodeOperation::NotEqual => {
                        self.add_line("cmp al, r8b");
                        self.add_line("setne al");
                        self.add_line("movzx rax, al");
                    }

                    NodeOperation::Less => {
                        self.add_line("cmp al, r8b");
                        self.add_line("setl al");
                        self.add_line("movzx rax, al");
                    }
                    NodeOperation::LessEqual => {
                        self.add_line("cmp al, r8b");
                        self.add_line("setle al");
                        self.add_line("movzx rax, al");
                    }
                    NodeOperation::Greater => {
                        self.add_line("cmp al, r8b");
                        self.add_line("setg al");
                        self.add_line("movzx rax, al");
                    }
                    NodeOperation::GreaterEqual => {
                        self.add_line("cmp al, r8b");
                        self.add_line("setge al");
                        self.add_line("movzx rax, al");
                    }
                }
                
                self.push("rax");
            }
        }
    }
    
    fn get_var_stack_loc(&self, varname: &String) -> Option<i64> {
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
        self.scopes[last].vars.push(CodeGenerationVar {
            stack_loc: self.stack_size,
            name: varname,
        });
    }

    fn generate_stmt(&mut self, statement: &NodeStmt) {
        match &statement {
            NodeStmt::DeclarationStmt(Variable(varname), typehint, expr) => {
                self.gen_expr(expr);
                self.create_var(varname.to_string());
            }
            NodeStmt::AssignmentStmt(Variable(varname), expr) => {
                self.gen_expr(expr);
                let Some(loc) = self.get_var_stack_loc(varname) else {
                    println!("Cannot assign to undeclared value");
                    return;
                };

                self.pop("rax");
                self.add_line(format!("mov [rsp + {}], rax", (self.stack_size - loc)*8).as_str());
            }
            NodeStmt::ExitStmt(value) => {
                self.gen_expr(value);
                self.add_line("mov rax, 60");
                self.pop("rdi");
                self.add_line("syscall");
            }
        }
    }

    fn gen_if_recurse(&mut self, sub_stmts: &Vec<NodeSubIfStmt>, position: usize, exit_addr: &str) {
        if sub_stmts.len() <= position {
            self.add_line(format!("jmp {}", exit_addr).as_str());
            return;
        }

        let sub_stmt = &sub_stmts[position];

        match sub_stmt {
            NodeSubIfStmt::Elif(expr, scope) => {
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
            NodeSubIfStmt::Else(scope) => {
                self.gen_scope(scope);
                self.add_line(format!("jmp {}", exit_addr).as_str());
            }
        }
    }

    fn gen_if(&mut self, if_stmt: &NodeIfStmt) {
        let NodeIfStmt { expr, scope, sub_stmts } = if_stmt;

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

    fn gen_scope(&mut self, scope: &NodeScope) {
        self.scopes.push(CodeGenerationScope { 
            vars: vec![], 
        });

        for item in scope.contents.iter() {
            match item {
                NodeScopeItem::Scope(scope) => {
                    self.gen_scope(scope);
                }
                NodeScopeItem::Stmt(stmt) => {
                    self.generate_stmt(stmt);
                }
                NodeScopeItem::IfStmt(stmt) => {
                    self.gen_if(stmt);
                }
                NodeScopeItem::WhileLoop(expr, scope) => {
                    let while_addr = format!("while_{}", rand::random::<u64>());
                    let exit_addr = format!("exit_{}", rand::random::<u64>());

                    self.add_label(&while_addr);

                    self.gen_expr(expr);
                    self.pop("rax");
                    self.add_line("test rax, rax");
                    self.add_line(format!("jz {}", exit_addr).as_str());

                    self.gen_scope(scope);
                    
                    self.add_line(format!("jmp {}", while_addr).as_str());
                    self.add_label(&exit_addr);
                }
            }
        }

        self.scopes.pop();
    }

    pub fn generate(&mut self, ast: &AbstractSyntaxTree) -> String {
        self.output_string += "global _start\n";
        self.add_label("_start");

        self.gen_scope(&ast.global);

        let result = self.output_string.clone();
        *self = CodeGeneration::new();
        result
    }
}
