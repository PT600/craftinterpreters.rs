use anyhow::{bail, Error, Result};
use fs::write;
use crate::{interpreter::{Interpreter, Value}, parser::{self, Parser}};
use crate::scanner::Scanner;
use std::io::{self, Write};
use std::fs;

pub struct Lox {
    interpreter: Interpreter,
}

impl Lox {
    pub fn new() -> Self {
        let interpreter = Interpreter {};
        Lox { interpreter }
    }
    pub fn run_file(&self, path: &str) -> Result<()> {
        let content = fs::read_to_string(path)?;
        self.run(content)?;
        Ok(())
    }

    pub fn run_prompt(&self) -> Result<()> {
        println!("Welcome to Lox console!");
        let stdin = io::stdin(); // We get `Stdin` here.
        loop {
            let mut buffer = String::new();
            let size = stdin.read_line(&mut buffer)?;
            if size == 0 || buffer.eq("quit\n") {
                break;
            }
            let value = self.run(buffer)?;
            println!("=>{:?}", value);
        }
        Ok(())
    }

    fn run(&self, source: String) -> Result<Value> {
        let mut scanner = Scanner::new(&source);
        scanner.scan_tokens()?;
        let expr = Parser::parse(scanner.tokens);
        self.interpreter.evaluate(&expr)
    }

    fn error(&self, line: usize, message: &str) -> Result<()> {
        self.report(line, "", message)
    }

    fn report(&self, line: usize, location: &str, message: &str) -> Result<()> {
        bail!("[line {} ] Error {}: {}", line, location, message)
    }
}
