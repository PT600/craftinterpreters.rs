use super::{chunk::*, strings::Strings};
use super::value::Value;
use super::table::Table;

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
    pub strings: Strings,
}

pub fn compile(source: &str) -> Result<Chunk> {
    let mut compiler = Compiler::new();
    compiler.compile(source)
}

impl Compiler {

    pub fn new() -> Self{
        let strings = Strings::new();
        Compiler { strings }
    }

    pub fn compile(&mut self, source: &str) -> Result<Chunk> {
        let mut parser = Parser::new(source);
        let mut chunk = Chunk::default();
        self.parse_precedence(Precedence::Start, &mut chunk, &mut parser)?;
        Ok(chunk)
    }

    pub fn parse_precedence(&mut self, precedence: Precedence, chunk: &mut Chunk, parser: &mut Parser) -> Result<()> {
        if let Some(token) = parser.next() {
            self.unary(token, chunk, parser)?;
            loop {
                let next_precedence = self.next_precedence(parser);
                if precedence >= next_precedence {
                    break;
                }
                let token = parser.next().unwrap();
                self.binary(token, next_precedence, chunk, parser)?;
            }
        }
        Ok(())
    }

    fn next_precedence(&mut self, parser: &mut Parser) -> Precedence {
        parser
            .peek()
            .map(|t| match t.ttype {
                PLUS | MINUS => Precedence::Term,
                STAR | SLASH => Precedence::Factor,
                _ => Precedence::Start,
            })
            .unwrap_or(Precedence::Start)
    }

    fn unary(&mut self, token: Token, chunk: &mut Chunk, parser: &mut Parser) -> Result<()> {
        let line = token.line;
        match &token.ttype {
            NIL => {
                chunk.write(OpCode::Nil, line);
            }
            TRUE => {
                chunk.write(OpCode::True, line);
            }
            FALSE => {
                chunk.write(OpCode::False, line);
            }
            BANG => chunk.write(OpCode::Not, line),
            NUMBER(num) => {
                chunk.write_const(Value::Number(*num), line);
            }
            STRING(str) => {
                let s = self.strings.add(str);
                chunk.write_const(Value::ObjString(s), line);
            }
            LeftParen => {
                self.parse_precedence(Precedence::Start, chunk, parser)?;
                parser
                    .consume(RightParen)
                    .context("Expect ')' after '(' in line {}")?;
            }
            MINUS => {
                self.parse_precedence(Precedence::Unary, chunk, parser)?;
                chunk.write(OpCode::Negate, line);
            }
            _ => bail!("unsupport unary {:?}", token.ttype),
        }
        Ok(())
    }

    fn binary(&mut self, token: Token, precedence: Precedence, chunk: &mut Chunk, parser: &mut Parser) -> Result<()> {
        self.parse_precedence(precedence, chunk, parser);
        let code = match &token.ttype {
            PLUS => OpCode::Add,
            MINUS => OpCode::Substract,
            STAR => OpCode::Multiply,
            SLASH => OpCode::Divide,
            _ => bail!("unsupport binary {:?}", token.ttype),
        };
        chunk.write(code, token.line);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::OpCode::*;
    use super::*;

    fn compile_assert(source: &str, codes: Vec<u8>) {
        let chunk = compile(source).unwrap();
        assert_eq!(chunk.codes, codes);
    }
    #[test]
    fn unary() {
        compile_assert("1", vec![Const as u8, 0u8]);

        compile_assert("-1", vec![Const as u8, 0u8, Negate as u8]);
    }

    #[test]
    fn binary() {
        compile_assert("1 + 1", vec![Const as u8, 0, Const as u8, 1, Add as u8]);
    }
}
