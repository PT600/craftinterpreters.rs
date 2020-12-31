mod scanner;
mod parser;
mod ast_printer;
mod ast;
#[cfg(test)]
mod scanner_test;
#[cfg(test)]
mod parser_test;

use std::{env, fs};
use std::io::{self, Read};
use std::process::exit;
use anyhow::{Result, Error, bail};
use scanner::Scanner;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        println!("Usage: jlox [script]");
        exit(64);
    } else if args.len() == 2 {
        run_file(&args[1])
    } else {
        run_prompt()
    }
}

fn run_file(path: &str) -> Result<()> {
    let content = fs::read_to_string(path)?;
    run(content)
}

fn run_prompt() -> Result<()> {
    let mut stdin = io::stdin(); // We get `Stdin` here.
    loop {
        let mut buffer = String::new();
        let size = stdin.read_line(&mut buffer)?;
        if size == 0 || buffer.eq("quit\n"){
            break;
        }
        println!("{}, {}", buffer, size);
        run(buffer)?;
    }
    Ok(())
}

fn run(source: String) -> Result<()> {
    let mut scanner = Scanner::new(&source);
    scanner.scan_tokens()?;
    for token in &scanner.tokens {
        println!("token: {:?}", token);
    }
    Ok(())
}

fn error(line: usize, message: &str) -> Result<()> {
    report(line, "", message)
}

fn report(line: usize, location: &str, message: &str) -> Result<()> {
    bail!("[line {} ] Error {}: {}", line, location, message)
}

