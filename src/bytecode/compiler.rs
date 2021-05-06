use super::table::Table;
use super::value::Value;
use super::{chunk::*, strings::Strings};

use crate::scanner::{
    Scanner, Token,
    TokenType::{self, *},
};

use anyhow::{bail, Context, Result};
use smol_str::SmolStr;
use std::{iter::Peekable, usize, vec::IntoIter};

pub struct Parser {
    it: Peekable<IntoIter<Token>>,
    line: usize,
}

impl Parser {
    pub fn new(source: &str) -> Parser {
        let tokens = Scanner::scan(source);
        Parser {
            it: tokens.into_iter().peekable(),
            line: 0,
        }
    }
    fn next(&mut self) -> Option<Token> {
        self.it.next().map(|t| {
            self.line = t.line;
            t
        })
    }
    fn peek(&mut self) -> Option<&Token> {
        self.it.peek()
    }

    // fn check(&mut self, ttype: TokenType) -> bool {
    //     self.it.peek().map_or(false, |token| matches!(&token.ttype, ttype))
    // }
    fn check(&mut self, ttype: TokenType) -> bool {
        self.it
            .peek()
            .map(|token| token.ttype == ttype)
            .unwrap_or(false)
    }
    fn matches(&mut self, ttype: TokenType) -> bool {
        if self.check(ttype) {
            self.next();
            true
        } else {
            false
        }
    }

    fn consume(&mut self, target: TokenType) -> Option<Token> {
        self.next().filter(|token| matches!(&token.ttype, target))
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

#[derive(Debug, Clone)]
struct Local {
    name: Option<SmolStr>,
    depth: i32,
}

pub struct Compiler {
    pub strings: Strings,
    pub parser: Parser,
    pub chunk: Chunk,
    pub locals: [Local; u8::MAX as usize],
    pub local_count: usize,
    pub scope_depth: i32,
}

impl Compiler {
    pub fn new(source: &str) -> Self {
        let strings = Strings::new();
        let parser = Parser::new(source);
        let chunk = Chunk::default();
        Compiler {
            strings,
            parser,
            chunk,
            locals: [Local {
                name: None,
                depth: 0,
            }; u8::MAX as usize],
            local_count: 0,
            scope_depth: 0,
        }
    }

    pub fn compile(&mut self) -> Result<()> {
        while self.parser.peek().is_some() {
            self.decl()?;
        }
        Ok(())
    }

    pub fn decl(&mut self) -> Result<()> {
        if self.parser.matches(TokenType::VAR) {
            self.var_decl()
        } else {
            self.statement()
        }
    }

    fn var_decl(&mut self) -> Result<()> {
        if let Some(token) = self.parser.next() {
            if let IDENTIFIER(id) = token.ttype {
                if self.parser.matches(TokenType::EQUAL) {
                    self.parse_precedence(Precedence::AssignmentPrev)?;
                } else {
                    self.emit_byte(OpCode::Nil);
                }
                self.parser
                    .consume(TokenType::SEMICOLON)
                    .context("expect ';' after expression!")?;
                self.define_variable(&id);
                return Ok(());
            }
        }
        bail!("expect identifier after var!")
    }

    fn define_variable(&mut self, id: &SmolStr) -> Result<()> {
        if self.scope_depth == 0 {
            let obj_str = self.strings.add(id.to_string());
            self.emit_byte(OpCode::DefineGlobal);
            self.chunk
                .write_const(Value::ObjString(obj_str), self.parser.line);
        } else {
            if self.local_count >= self.locals.len() {
                bail!("too many locals!")
            }
            let mut index = self.local_count - 1;
            while index >= 0 {
                let local = &self.locals[index];
                if local.depth != -1 && local.depth < self.scope_depth {
                    break;
                }
                if matches!(&local.name, Some(name) if name == id) {
                    bail!("Already variable with this name in this scope.")
                }
                index -= 1;
            }
            let local = &mut self.locals[self.local_count];
            local.name.replace(id.clone());
            local.depth = self.scope_depth;
            self.local_count += 1;
        }
        Ok(())
    }

    fn statement(&mut self) -> Result<()> {
        if self.parser.matches(TokenType::PRINT) {
            self.print_stat(self.parser.line)
        } else if self.parser.matches(TokenType::LeftBrace) {
            self.block()
        } else {
            self.expr_stat()
        }
    }

    fn print_stat(&mut self, line: usize) -> Result<()> {
        self.expr()?;
        self.parser
            .consume(TokenType::SEMICOLON)
            .context("Expect ';' after value.")?;
        self.chunk.write(OpCode::Print, line);
        Ok(())
    }

    fn expr_stat(&mut self) -> Result<()> {
        self.expr()?;
        self.parser
            .consume(TokenType::SEMICOLON)
            .context("expect ';' after expr")?;
        self.emit_byte(OpCode::Pop);
        Ok(())
    }

    fn block(&mut self) -> Result<()> {
        self.scope_depth += 1;
        while !self.parser.check(TokenType::RightBrace) {
            self.decl()?;
        }
        self.parser
            .consume(TokenType::RightBrace)
            .context("expect '}' after block")?;
        self.scope_depth -= 1;
        while self.local_count > 0 && self.locals[self.local_count - 1].depth > self.scope_depth {
            self.emit_byte(OpCode::Pop);
            self.local_count -= 1;
        }
        Ok(())
    }

    fn emit_byte(&mut self, code: OpCode) {
        self.chunk.write(code, self.parser.line)
    }
    pub fn expr(&mut self) -> Result<()> {
        self.parse_precedence(Precedence::Start)
    }

    pub fn parse_precedence(&mut self, precedence: Precedence) -> Result<()> {
        let can_assign = precedence <= Precedence::Assignment;
        if let Some(token) = self.parser.next() {
            self.unary(token, can_assign)?;
            loop {
                let next_precedence = self.next_precedence();
                if precedence >= next_precedence {
                    break;
                }
                let token = self.parser.next().unwrap();
                self.binary(token, next_precedence)?;
            }
        }
        if can_assign && self.parser.matches(TokenType::EQUAL) {
            bail!("Invalid assignment target!")
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

    fn unary(&mut self, token: Token, can_assign: bool) -> Result<()> {
        let line = token.line;
        match &token.ttype {
            NIL => {
                self.chunk.write(OpCode::Nil, line);
            }
            TRUE => {
                self.chunk.write(OpCode::True, line);
            }
            FALSE => {
                self.chunk.write(OpCode::False, line);
            }
            BANG => self.chunk.write(OpCode::Not, line),
            NUMBER(num) => {
                self.chunk.write_const(Value::Number(*num), line);
            }
            STRING(str) => {
                let s = self.strings.add(str.into());
                self.chunk.write_const(Value::ObjString(s), line);
            }
            IDENTIFIER(id) => {
                if can_assign && self.parser.matches(TokenType::EQUAL) {
                    self.parse_precedence(Precedence::AssignmentPrev);
                    self.emit_byte(OpCode::SetGlobal);
                } else {
                    self.emit_byte(OpCode::GetGlobal);
                }
                let obj_str = self.strings.add(id.to_string());
                self.chunk
                    .write_const(Value::ObjString(obj_str), self.parser.line);
            }
            LeftParen => {
                self.parse_precedence(Precedence::Start)?;
                self.parser
                    .consume(RightParen)
                    .context("Expect ')' after '(' in line {}")?;
            }
            MINUS => {
                self.parse_precedence(Precedence::Unary)?;
                self.chunk.write(OpCode::Negate, line);
            }
            _ => bail!("unsupport unary {:?}", token.ttype),
        }
        Ok(())
    }

    fn binary(&mut self, token: Token, precedence: Precedence) -> Result<()> {
        self.parse_precedence(precedence)?;
        let code = match &token.ttype {
            PLUS => OpCode::Add,
            MINUS => OpCode::Substract,
            STAR => OpCode::Multiply,
            SLASH => OpCode::Divide,
            _ => bail!("unsupport binary {:?}", token.ttype),
        };
        self.chunk.write(code, token.line);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::OpCode::*;
    use super::*;

    fn compile_assert(source: &str, codes: Vec<u8>) {
        let mut compiler = Compiler::new(source);
        compiler.compile().unwrap();
        assert_eq!(compiler.chunk.codes, codes);
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
