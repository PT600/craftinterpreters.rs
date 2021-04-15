mod ast;
mod value;
mod ast_printer;
mod enviorment;
mod interpreter;
mod lox;
mod parser;
mod scanner;
mod bytecode;

use anyhow::Result;
use std::process::exit;
use std::env;
fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        println!("Usage: jlox [script]");
        exit(64);
    } else if args.len() == 2 {
        lox::run_file(&args[1])
    } else {
        lox::run_prompt()
    }
}

#[test]
fn test() {
}
