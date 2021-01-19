use crate::interpreter::Interpreter;
use crate::parser::{self, Parser};
use crate::scanner::Scanner;
use anyhow::{bail, Error, Result};
use std::fs;
use std::io::{self};

pub fn run_file(path: &str) -> Result<()> {
    let mut interpreter = Interpreter::new(false);
    let content = fs::read_to_string(path)?;
    run(&mut interpreter, content)?;
    Ok(())
}

pub fn run_prompt() -> Result<()> {
    println!("Welcome to Lox console!");
    let mut interpreter = Interpreter::new(true);
    let stdin = io::stdin(); // We get `Stdin` here.
    loop {
        let mut buffer = String::new();
        let size = stdin.read_line(&mut buffer)?;
        if size == 0 || buffer.eq("quit\n") {
            break;
        }
        run(&mut interpreter, buffer)?;
        // println!("=>{:?}", value);
    }
    Ok(())
}

fn run(interpreter: &mut Interpreter, source: String) -> Result<()> {
    let mut scanner = Scanner::new(&source);
    scanner.scan_tokens()?;
    let mut parser = Parser::new(scanner.tokens);
    let expr = parser.parse();
    interpreter.interpret(expr)
}

fn error(line: usize, message: &str) -> Result<()> {
    report(line, "", message)
}

fn report(line: usize, location: &str, message: &str) -> Result<()> {
    bail!("[line {} ] Error {}: {}", line, location, message)
}
