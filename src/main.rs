mod ast;
mod ast_printer;
mod enviorment;
mod interpreter;
mod lox;
mod parser;
mod scanner;

use anyhow::{bail, Error, Result};
use lox::Lox;
use std::{io::{self, Read}, ops::Deref, sync::Arc};
use std::process::exit;
use std::{env, fs};

fn main() -> Result<()> {
    let mut lox = Lox::new();
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

#[test]
fn test() {
}
