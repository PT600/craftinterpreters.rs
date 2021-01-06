mod ast;
mod ast_printer;
mod interpreter;
mod parser;
mod scanner;
mod lox;

use anyhow::{bail, Error, Result};
use std::io::{self, Read};
use std::process::exit;
use std::{env, fs};
use lox::Lox;

fn main() -> Result<()> {
    let lox = Lox::new();
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        println!("Usage: jlox [script]");
        exit(64);
    } else if args.len() == 2 {
        lox.run_file(&args[1])
    } else {
        lox.run_prompt()
    }
}
