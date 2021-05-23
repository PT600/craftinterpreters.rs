use super::{chunk::*, debug, strings::Strings};
use super::{object::ObjFunction, table::Table};
use super::{object::ObjString, value::Value};

use crate::{
    ast_printer::print,
    scanner::{
        Scanner, Token,
        TokenType::{self, *},
    },
};

use anyhow::{bail, Context, Result};
use smol_str::SmolStr;
use std::{fmt::Display, iter::Peekable, mem, ptr, rc::Rc, usize, vec::IntoIter};

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

pub struct Compiler {
    pub strings: Strings,
    pub parser: Parser,
    pub func: FunCompiler,
    pub enclosings: Vec<FunCompiler>,
}

#[derive(Debug)]
pub struct FunCompiler {
    pub arity: u8,
    pub chunk: Chunk,
    pub name: *const ObjString,
    pub locals: Vec<Local>,
    pub scope_depth: i32,
}

impl FunCompiler {
    pub fn new(name: *const ObjString) -> Self {
        FunCompiler {
            arity: 0,
            chunk: Default::default(),
            name,
            locals: vec![],
            scope_depth: 0,
        }
    }
}

impl Display for FunCompiler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        debug::fmt_func(self, f)
    }
}

pub fn compile(source: &str) -> Result<Compiler> {
    let strings = Strings::new();
    let parser = Parser::new(source);
    let mut compiler = Compiler::new(strings, parser);
    let result = compiler.compile();
    match &result {
        Err(e) => {
            println!("compile err: {:?}", result);
            for enclosing in &compiler.enclosings {
                println!("enclosing: {}", enclosing);
            }
            println!("func: {}", compiler.func);
            bail!("compile error: {}", e)
        }
        _ => Ok(compiler),
    }
}

impl Compiler {
    pub fn new(strings: Strings, parser: Parser) -> Self {
        let func = FunCompiler::new(ptr::null());
        Compiler {
            strings,
            parser,
            func,
            enclosings: vec![],
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
        self.consume(SEMICOLON, "expect ';' after expression!")?;
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
        let id = self.consume_identifier()?;
        let name = self.strings.add(id.to_string());
        let func = FunCompiler::new(name);
        self.func.locals.push(Local {
            name: "".into(),
            depth: 0,
        });
        let enclosing = mem::replace(&mut self.func, func);
        self.enclosings.push(enclosing);
        self.function()?;
        let enclosing = self.enclosings.pop().context("enclosing is missing")?;
        let func = mem::replace(&mut self.func, enclosing);
        let fun = ObjFunction {
            arity: func.arity,
            chunk: func.chunk,
            name,
        };
        self.write_const(Value::ObjFunction(Rc::new(fun)));
        self.define_variable(id)?;
        Ok(())
    }

    fn function(&mut self) -> Result<()> {
        self.consume(LeftParen, "expect ( after function decl")?;
        self.begin_scope();
        if !self.parser.check(TokenType::RightParen) {
            loop {
                self.func.arity += 1;
                let param = self.consume_identifier()?;
                self.define_variable(param)?;
                if !self.parser.matches(TokenType::COMMA) {
                    break;
                }
            }
        }
        self.consume(RightParen, "expect ) after function decl")?;
        self.consume(LeftBrace, "expect { before function body")?;
        self.block()?;
        self.end_scope();
        Ok(())
    }

    fn define_variable(&mut self, id: SmolStr) -> Result<()> {
        println!("define_var, func: {}", self.func);
        if self.func.scope_depth == 0 {
            let obj_str = self.strings.add(id.to_string());
            self.emit_code(OpCode::DefineGlobal);
            let const_idx = self.func.chunk.add_const(Value::ObjString(obj_str));
            self.emit_byte(const_idx as u8);
        } else {
            if self.func.locals.len() >= u8::MAX as usize {
                bail!("too many locals!")
            }
            for local in self.func.locals.iter().rev() {
                if local.depth != -1 && local.depth < self.func.scope_depth {
                    break;
                }
                if local.name == id {
                    bail!("Already variable with this name in this scope.")
                }
            }
            self.func.locals.push(Local {
                name: id.clone(),
                depth: self.func.scope_depth,
            })
        }
        Ok(())
    }
    fn statement(&mut self) -> Result<()> {
        let result = if self.parser.matches(TokenType::PRINT) {
            self.print_stat(self.parser.line)
        } else if self.parser.matches(TokenType::LeftBrace) {
            self.begin_scope();
            let result = self.block();
            self.end_scope();
            result
        } else if self.parser.matches(TokenType::IF) {
            self.if_stat()
        } else if self.parser.matches(TokenType::RETURN) {
            self.return_stat()
        } else {
            self.expr_stat()
        };
        result
    }

    fn print_stat(&mut self, line: usize) -> Result<()> {
        self.expr()?;
        self.consume(SEMICOLON, "Expect ';' after value.")?;
        self.emit_code(OpCode::Print);
        Ok(())
    }
    fn if_stat(&mut self) -> Result<()> {
        self.consume(LeftParen, "expect '(' before if condition")?;
        self.expr()?;
        self.consume(RightParen, "expect ')' after if contidion")?;
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

    fn return_stat(&mut self) -> Result<()> {
        if self.parser.matches(TokenType::SEMICOLON) {
            self.emit_code(OpCode::Nil);
        } else {
            self.expr()?;
            self.consume(TokenType::SEMICOLON, "expect ';' after return")?;
        }
        self.emit_code(OpCode::Return);
        Ok(())
    }

    fn expr_stat(&mut self) -> Result<()> {
        self.expr()?;
        self.consume(TokenType::SEMICOLON, "expect ';' after expr")?;
        self.emit_code(OpCode::Pop);
        Ok(())
    }

    fn block(&mut self) -> Result<()> {
        while !self.parser.check(TokenType::RightBrace) {
            self.decl()?;
        }
        self.consume(TokenType::RightBrace, "expect '}' after block")?;
        Ok(())
    }
    fn begin_scope(&mut self) {
        self.func.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.func.scope_depth -= 1;
        while let Some(local) = self.func.locals.last() {
            if local.depth > self.func.scope_depth {
                self.func.locals.pop();
                self.emit_code(OpCode::Pop);
            } else {
                break;
            }
        }
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
                EqualEqual | BangEqual | GREATER | GreaterEqual | LESS | LessEqual => Precedence::Comparision,
                PLUS | MINUS => Precedence::Term,
                STAR | SLASH => Precedence::Factor,
                LeftParen => Precedence::Call,
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
            BANG => self.chunk().write(OpCode::Not, line),
            NUMBER(num) => {
                self.chunk().write_const(Value::Number(*num), line);
            }
            STRING(str) => {
                let s = self.strings.add(str.into());
                self.chunk().write_const(Value::ObjString(s), line);
            }
            IDENTIFIER(id) => {
                let (arg, get_op, set_op) = if let Some(arg) = self.resolve_local(id) {
                    (arg, OpCode::GetLocal, OpCode::SetLocal)
                } else {
                    let obj_str = self.strings.add(id.to_string());
                    let arg = self.chunk().add_const(Value::ObjString(obj_str));
                    (arg, OpCode::GetGlobal, OpCode::SetGlobal)
                };
                if can_assign && self.parser.matches(TokenType::EQUAL) {
                    self.parse_precedence(Precedence::AssignmentPrev)?;
                    self.emit_code(set_op);
                } else {
                    self.emit_code(get_op);
                }
                self.emit_byte(arg as u8)
            }
            LeftParen => {
                self.parse_precedence(Precedence::Start)?;
                self.parser
                    .consume(RightParen)
                    .context("Expect ')' after '(' in line {}")?;
            }
            MINUS => {
                self.parse_precedence(Precedence::Unary)?;
                self.emit_code(OpCode::Negate);
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
            EqualEqual => self.handle_binary(OpCode::EqualEqual, precedence),
            BangEqual => self.handle_binary(OpCode::BangEqual, precedence),
            GreaterEqual => self.handle_binary(OpCode::GreaterEqual, precedence),
            GREATER => self.handle_binary(OpCode::Greater, precedence),
            LessEqual => self.handle_binary(OpCode::LessEqual, precedence),
            LESS=> self.handle_binary(OpCode::Less, precedence),
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
        self.consume(RightParen, "Expect ) after arg list")?;
        Ok(arg_count)
    }

    fn handle_binary(&mut self, code: OpCode, precedence: Precedence) -> Result<()> {
        self.parse_precedence(precedence)?;
        self.emit_code(code);
        Ok(())
    }
    fn resolve_local(&self, id: &SmolStr) -> Option<usize> {
        self.func
            .locals
            .iter()
            .rev()
            .position(|local| &local.name == id)
            .map(|position| self.func.locals.len() - position)
    }

    fn emit_jump(&mut self, code: OpCode) -> (usize, usize) {
        self.emit_code(code);
        let jump_arg_start = self.chunk().codes.len();
        self.emit_byte(0);
        self.emit_byte(0);
        let jump_arg_end = self.chunk().codes.len();
        (jump_arg_start, jump_arg_end)
    }

    fn patch_jump(&mut self, jump_arg_start: usize, jump_arg_end: usize) {
        let jump = self.chunk().codes.len() - jump_arg_end;
        self.chunk().codes[jump_arg_start] = ((jump >> 8) & 0xff) as u8;
        self.chunk().codes[jump_arg_start + 1] = (jump & 0xff) as u8;
    }

    fn write_const(&mut self, value: Value) {
        self.func.chunk.write_const(value, self.parser.line);
    }

    fn emit_byte(&mut self, code: u8) {
        let line = self.parser.line;
        self.chunk().write_byte(code, line)
    }

    fn emit_code(&mut self, code: OpCode) {
        let line = self.parser.line;
        self.chunk().write(code, line)
    }

    fn chunk(&mut self) -> &mut Chunk {
        &mut self.func.chunk
    }

    fn consume(&mut self, ttype: TokenType, err_msg: &'static str) -> Result<()> {
        self.parser.consume(ttype).context(err_msg)?;
        Ok(())
    }
}
