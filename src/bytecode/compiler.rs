use super::{chunk::*, strings::Strings};
use super::{object::ObjFunction, table::Table};
use super::{object::ObjString, value::Value};

use crate::scanner::{
    Scanner, Token,
    TokenType::{self, *},
};

use anyhow::{bail, Context, Result};
use smol_str::SmolStr;
use std::{iter::Peekable, mem, rc::Rc, usize, vec::IntoIter};

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
pub struct Local {
    name: SmolStr,
    depth: i32,
}

pub struct Compiler<'a> {
    pub strings: &'a mut Strings,
    pub parser: &'a mut Parser,
    pub arity: u8,
    pub chunk: Chunk,
    pub name: *const ObjString,
    pub locals: Vec<Local>,
    pub scope_depth: i32,
}

impl<'a> Compiler<'a> {
    pub fn new(strings: &'a mut Strings, parser: &'a mut Parser) -> Self {
        Compiler {
            strings,
            parser,
            arity: 0,
            chunk: Default::default(),
            name: std::ptr::null(),
            locals: vec![],
            scope_depth: 0,
        }
    }

    pub fn compile(&mut self) -> Result<()> {
        while self.parser.peek().is_some() {
            self.decl()?;
        }
        // let fun = ObjFunction {
        //     chunk: self.chunk,
        //     name: self.name,
        //     arity: self.arity,
        // };
        Ok(())
    }

    pub fn decl(&mut self) -> Result<()> {
        if self.parser.matches(TokenType::VAR) {
            self.var_decl()
        } else if self.parser.matches(TokenType::FUN) {
            self.fun_decl()
        } else {
            self.statement()
        }
    }

    fn var_decl(&mut self) -> Result<()> {
        let id = self.consume_identifier()?;
        if self.parser.matches(TokenType::EQUAL) {
            self.parse_precedence(Precedence::AssignmentPrev)?;
        } else {
            self.emit_code(OpCode::Nil);
        }
        self.parser
            .consume(TokenType::SEMICOLON)
            .context("expect ';' after expression!")?;
        self.define_variable(id)?;
        Ok(())
    }

    fn consume_identifier(&mut self) -> Result<SmolStr> {
        if let Some(token) = self.parser.next() {
            if let IDENTIFIER(id) = token.ttype {
                return Ok(id);
            }
        }
        bail!("expect identifier!")
    }

    fn fun_decl(&mut self) -> Result<()> {
        let name = self.consume_identifier()?;
        let name = self.strings.add(name.to_string());
        let mut compiler = Compiler::new(self.strings, self.parser);
        compiler.function()?;
        let fun = ObjFunction {
            arity: compiler.arity,
            chunk: compiler.chunk,
            name,
        };
        self.chunk.write_const(Value::ObjFunction(Rc::new(fun)), self.parser.line);
        Ok(())
    }

    fn function(&mut self) -> Result<()> {
        self.parser
            .consume(TokenType::LeftParen)
            .context("expect ( after function decl")?;
        self.scope_depth = 1;
        if !self.parser.check(TokenType::RightParen) {
            loop {
                self.arity += 1;
                let param = self.consume_identifier()?;
                self.define_variable(param)?;
                if !self.parser.matches(TokenType::COMMA) {
                    break;
                }
            }
        }
        self.parser
            .consume(TokenType::RightParen)
            .context("expect ) after function decl")?;
        self.block()?;
        Ok(())
    }

    fn define_variable(&mut self, id: SmolStr) -> Result<()> {
        if self.scope_depth == 0 {
            let obj_str = self.strings.add(id.to_string());
            self.emit_code(OpCode::DefineGlobal);
            self.chunk
                .write_const(Value::ObjString(obj_str), self.parser.line);
        } else {
            if self.locals.len() >= u8::MAX as usize {
                bail!("too many locals!")
            }
            for local in self.locals.iter().rev() {
                if local.depth != -1 && local.depth < self.scope_depth {
                    break;
                }
                if local.name == id {
                    bail!("Already variable with this name in this scope.")
                }
            }
            self.locals.push(Local {
                name: id.clone(),
                depth: self.scope_depth,
            })
        }
        Ok(())
    }

    fn resolve_local(&self, id: &SmolStr) -> Option<usize> {
        self.locals.iter().rev().position(|local| &local.name == id)
    }

    fn statement(&mut self) -> Result<()> {
        if self.parser.matches(TokenType::PRINT) {
            self.print_stat(self.parser.line)
        } else if self.parser.matches(TokenType::LeftBrace) {
            self.block()
        } else if self.parser.matches(TokenType::IF) {
            self.if_stat()
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

    fn if_stat(&mut self) -> Result<()> {
        self.parser
            .consume(TokenType::LeftParen)
            .context("expect '(' before if condition")?;
        self.expr()?;
        self.parser
            .consume(TokenType::RightParen)
            .context("expect ')' after if contidion")?;
        let (jump_arg_start, jump_arg_end) = self.emit_jump(OpCode::JumpIfFalse);
        self.statement()?;
        self.patch_jump(jump_arg_start, jump_arg_end);
        if self.parser.matches(TokenType::ELSE) {
            let (jump_arg_start, jump_arg_end) = self.emit_jump(OpCode::Jump);
            self.statement()?;
            self.patch_jump(jump_arg_start, jump_arg_end);
        }
        Ok(())
    }

    fn emit_jump(&mut self, code: OpCode) -> (usize, usize) {
        self.emit_code(code);
        let jump_arg_start = self.chunk.codes.len();
        self.emit_byte(0);
        self.emit_byte(0);
        let jump_arg_end = self.chunk.codes.len();
        (jump_arg_start, jump_arg_end)
    }

    fn patch_jump(&mut self, jump_arg_start: usize, jump_arg_end: usize) {
        let jump = self.chunk.codes.len() - jump_arg_end;
        self.chunk.codes[jump_arg_start] = ((jump >> 8) & 0xff) as u8;
        self.chunk.codes[jump_arg_start + 1] = (jump & 0xff) as u8;
    }

    fn expr_stat(&mut self) -> Result<()> {
        self.expr()?;
        self.parser
            .consume(TokenType::SEMICOLON)
            .context("expect ';' after expr")?;
        self.emit_code(OpCode::Pop);
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
        while let Some(local) = self.locals.last() {
            if local.depth > self.scope_depth {
                self.locals.pop();
                self.emit_code(OpCode::Pop);
            } else {
                break;
            }
        }
        Ok(())
    }

    fn emit_byte(&mut self, code: u8) {
        self.chunk.write_byte(code, self.parser.line)
    }

    fn emit_code(&mut self, code: OpCode) {
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
                self.emit_code(OpCode::Nil);
            }
            TRUE => {
                self.emit_code(OpCode::True);
            }
            FALSE => {
                self.emit_code(OpCode::False);
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
                let (arg, get_op, set_op) = if let Some(arg) = self.resolve_local(id) {
                    (arg, OpCode::GetLocal, OpCode::SetLocal)
                } else {
                    let obj_str = self.strings.add(id.to_string());
                    let arg = self.chunk.add_const(Value::ObjString(obj_str));
                    (arg, OpCode::GetGlobal, OpCode::SetGlobal)
                };
                if can_assign && self.parser.matches(TokenType::EQUAL) {
                    self.parse_precedence(Precedence::AssignmentPrev)?;
                    self.emit_code(set_op);
                } else {
                    self.emit_code(get_op);
                }
                self.chunk.write_byte(arg as u8, line)
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
        match &token.ttype {
            LeftParen => {
                let arg_count = self.argument_list()?;
                self.emit_code(OpCode::Call);
                self.emit_byte(arg_count as u8);
                Ok(())
            }
            AND => {
                let (jump_arg_start, jump_arg_end) = self.emit_jump(OpCode::JumpAndFalse);
                self.emit_code(OpCode::Pop);
                self.parse_precedence(precedence)?;
                self.patch_jump(jump_arg_start, jump_arg_end);
                Ok(())
            }
            OR => {
                let (jump_arg_start, jump_arg_end) = self.emit_jump(OpCode::JumpOrTrue);
                self.emit_code(OpCode::Pop);
                self.parse_precedence(precedence)?;
                self.patch_jump(jump_arg_start, jump_arg_end);
                Ok(())
            }
            PLUS => self.handle_binary(OpCode::Add, precedence),
            MINUS => self.handle_binary(OpCode::Substract, precedence),
            STAR => self.handle_binary(OpCode::Multiply, precedence),
            SLASH => self.handle_binary(OpCode::Divide, precedence),
            _ => bail!("unsupport binary {:?}", token.ttype),
        }
    }

    fn argument_list(&mut self) -> Result<usize> {
        let mut arg_count = 0;
        if !self.parser.check(TokenType::RightParen) {
            loop {
                self.expr()?;
                arg_count += 1;
                if !self.parser.matches(TokenType::COMMA) {
                    break;
                }
            }
        }
        self.parser
            .consume(TokenType::RightParen)
            .context("Expect ) after arg list")?;
        Ok(arg_count)
    }

    fn handle_binary(&mut self, code: OpCode, precedence: Precedence) -> Result<()> {
        self.parse_precedence(precedence)?;
        self.emit_code(code);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::OpCode::*;
    use super::*;

    fn compile_assert(source: &str, codes: Vec<u8>) {
        let mut strings = Strings::new();
        let mut parser = Parser::new(source);
        let mut compiler = Compiler::new(&mut strings, &mut parser);
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
