mod parser;
mod tokenizer;
mod codegen;

use std::{collections::VecDeque, env, fs};

use parser::Parser;
use tokenizer::Tokenizer;

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
    let ast = Parser::new(tokens).parse().unwrap();
    println!("{:#?}", ast);
 //   let program = CodeGeneration::new().generate(&ast);
 //   println!("{}", program);
}
