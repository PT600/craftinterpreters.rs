mod ast;
mod ast_printer;
mod interpreter;
mod parser;
mod scanner;
mod lox;
mod enviorment;

use anyhow::{bail, Error, Result};
use std::io::{self, Read};
use std::process::exit;
use std::{env, fs};
use lox::Lox;

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
fn test(){
    let immut_val = String::from("immut");
    let fn_closure = || {
        println!("Len: {}", immut_val.len());
    };

    println!("Value: {}", immut_val); //ok
    fn_closure();                     //ok

    // cannot borrow mutably because it is already borrowed immutably
    // immut_val.push_str("-push");   
    // fn_closure();
}