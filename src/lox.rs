use anyhow::{bail, Error, Result};
use crate::interpreter::Interpreter; 
use crate::parser::{self, Parser};
use crate::scanner::Scanner;
use crate::ast::*;
use std::io::{self};
use std::fs;


pub struct Lox {
    interpreter: Interpreter,
}

impl Lox {
    pub fn new() -> Self {
        let interpreter = Interpreter::new();
        Lox { interpreter }
    }
    pub fn run_file(&mut self, path: &str) -> Result<()> {
        let content = fs::read_to_string(path)?;
        self.run(content)?;
        Ok(())
    }

    pub fn run_prompt(&mut self) -> Result<()> {
        println!("Welcome to Lox console!");
        let stdin = io::stdin(); // We get `Stdin` here.
        loop {
            let mut buffer = String::new();
            let size = stdin.read_line(&mut buffer)?;
            if size == 0 || buffer.eq("quit\n") {
                break;
            }
            self.run(buffer)?;
            // println!("=>{:?}", value);
        }
        Ok(())
    }

    fn run(&mut self, source: String) -> Result<()> {
        let mut scanner = Scanner::new(&source);
        scanner.scan_tokens()?;
        let mut parser = Parser::new(scanner.tokens);
        let expr = parser.parse();
        self.interpreter.interpret(expr)
    }

    fn error(&self, line: usize, message: &str) -> Result<()> {
        self.report(line, "", message)
    }

    fn report(&self, line: usize, location: &str, message: &str) -> Result<()> {
        bail!("[line {} ] Error {}: {}", line, location, message)
    }
}
