use super::chunk::*;
use crate::scanner::{
    Scanner, Token,
    TokenType::{self, *},
};

use anyhow::{bail, Context, Result};
use std::{iter::Peekable, vec::IntoIter};

pub struct Parser {
    it: Peekable<IntoIter<Token>>,
}

impl Parser {
    pub fn new(source: &str) -> Parser {
        let tokens = Scanner::scan(source);
        Parser {
            it: tokens.into_iter().peekable(),
        }
    }
    fn next(&mut self) -> Option<Token> {
        self.it.next()
    }
    fn peek(&mut self) -> Option<&Token> {
        self.it.peek()
    }

    fn consume(&mut self, target: TokenType) -> Option<Token> {
        self.it
            .next()
            .filter(|token| matches!(&token.ttype, target))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Start,
    AssignmentPrev,
    Assignment,  // =
    Conditional, // ?:
    Or,          // or
    And,         // and
    Equality,    // == !=
    Comparision, // < > <= >=
    Term,        // + -
    Factor,      // * /
    Unary,       // ! -
    Exponent,    // ^
    Postfix,
    Call, // . ()
}

pub struct Compiler {
    parser: Parser,
    pub chunk: Chunk,
}

impl Compiler {
    pub fn compile(source: &str) -> Self {
        let parser = Parser::new(source);
        let chunk = Chunk::default();
        let mut compiler = Compiler {parser, chunk};
        compiler.parse_precedence(Precedence::Start);
        compiler
    }

    pub fn parse_precedence(&mut self, precedence: Precedence) -> Result<()> {
        if let Some(token) = self.parser.next() {
            self.unary(token)?;
            loop {
                let next_precedence = self.next_precedence();
                if precedence >= next_precedence {
                    break;
                }
                let token = self.parser.next().unwrap();
                self.binary(token, next_precedence)?;
            }
        }
        Ok(())
    }

    fn next_precedence(&mut self) -> Precedence {
        self.parser
            .peek()
            .map(|t| match t.ttype {
                PLUS | MINUS => Precedence::Term,
                STAR | SLASH => Precedence::Factor,
                _ => Precedence::Start,
            })
            .unwrap_or(Precedence::Start)
    }

    fn unary(&mut self, token: Token) -> Result<()> {
        match token.ttype {
            NUMBER(num) => {
                self.chunk.write_const(num, token.line);
            }
            LeftParen => {
                self.parse_precedence(Precedence::Start);
                self.parser
                    .consume(RightParen)
                    .context("Expect ')' after '(' in line {}")?;
            }
            MINUS => {
                self.parse_precedence(Precedence::Unary);
                self.emit_byte(OpCode::Negate, token.line);
            }
            _ => return bail!("unsupport unary {:?}", token.ttype),
        }
        Ok(())
    }

    fn binary(&mut self, token: Token, precedence: Precedence) -> Result<()> {
        self.parse_precedence(precedence);
        let line = token.line;
        match &token.ttype {
            PLUS => self.emit_byte(OpCode::Add, line),
            MINUS => self.emit_byte(OpCode::Substract, line),
            STAR => self.emit_byte(OpCode::Multiply, line),
            SLASH => self.emit_byte(OpCode::Divide, line),
            _ => return bail!("unsupport binary {:?}", token.ttype),
        }
        Ok(())
    }

    fn emit_byte(&mut self, code: OpCode, line: usize) {
        self.chunk.write(code, line)
    }
}

#[test]
fn test(){
  let compiler = Compiler::compile("-1 - 1 + 4");
  println!("{:?}", compiler.chunk.codes)
}
