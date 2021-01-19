use crate::ast::{Expr::*, *};
use crate::scanner::{Token, TokenType::*, *};
use anyhow::{bail, Context, Result};
use smol_str::SmolStr;
use std::iter::Peekable;
use std::vec::IntoIter;

pub struct Parser {
    it: Peekable<IntoIter<Token>>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        let it: Peekable<IntoIter<Token>> = tokens.into_iter().peekable();
        Parser { it }
    }

    pub fn parse(&mut self) -> Vec<Result<Stmt>> {
        let mut stmts = vec![];
        while let Some(token) = self.it.peek() {
            stmts.push(self.declaration());
        }
        stmts
    }

    pub fn parse_expr(tokens: Vec<Token>) -> Expr {
        Self::new(tokens).expression()
    }

    // decl     -> fun_decl | var_decl | statement
    fn declaration(&mut self) -> Result<Stmt> {
        let stmt = if self.next_if_match(FUN) {
            self.fun_decl("function")
        } else if self.next_if_match(VAR) {
            self.var_decl()
        } else {
            self.statement()
        };
        if stmt.is_err() {
            self.synchronize();
        }
        stmt
    }

    // fun_decl    -> "fun" identifier "(" parameters? ")" block ;
    fn fun_decl(&mut self, kind: &'static str) -> Result<Stmt> {
        let name = self.consume_identifier()?;
        self.consume(LeftParen)
            .context(format!("Expect '(' after {} name.", kind))?;
        let mut params = vec![];
        if !self.check(RightParen) {
            loop {
                assert!(params.len() < 255, "Can't have more than 255 parameters.");
                let param = self.consume_identifier()?;
                params.push(param);
                if !self.next_if_match(COMMA) {
                    break;
                }
            }
        }
        self.consume(RightParen)
            .context(format!("Expect ')' after {} name.", kind))?;
        self.consume(LeftBrace)
            .context(format!("Expect 'LeftBrace' before {} body.", kind))?;
        let body = self.block_stmt()?;
        Ok(Stmt::FunDecl(FunDecl { name, params, body }))
    }

    // var_decl   -> "var" identifier ("=" expression)? ";";
    fn var_decl(&mut self) -> Result<Stmt> {
        let var = self.consume_identifier()?;
        let stmt = if self.next_if_match(EQUAL) {
            Stmt::VarDecl(var, Some(self.expression()))
        } else {
            Stmt::VarDecl(var, None)
        };
        self.consume(SEMICOLON)
            .context("Expect ';' after a value")?;
        return Ok(stmt);
    }

    fn consume_identifier(&mut self) -> Result<SmolStr> {
        if let Some(token) = self.it.next() {
            match token.ttype {
                IDENTIFIER(var) => {
                    return Ok(var);
                }
                _ => {}
            }
        }
        bail!("expect identifier!")
    }
    // statement  -> expr_stmt
    //             | for_stmt
    //             | if_stmt
    //             | print_stmt
    //             | return_stmt
    //             | while_stmt
    //             | block;
    fn statement(&mut self) -> Result<Stmt> {
        if self.next_if_match(PRINT) {
            self.print_stmt()
        } else if self.next_if_match(RETURN) {
            self.return_stmt()
        } else if self.next_if_match(FOR) {
            self.for_stmt()
        } else if self.next_if_match(IF) {
            self.if_stmt()
        } else if self.next_if_match(LeftBrace) {
            Ok(Stmt::BlockStmt(self.block_stmt()?))
        } else if self.next_if_match(WHILE) {
            self.while_stmt()
        } else if self.next_if_match(Break) {
            self.break_stmt()
        } else {
            self.expr_stmt()
        }
    }

    fn print_stmt(&mut self) -> Result<Stmt> {
        let expr = self.expression();
        self.next_if_match(SEMICOLON);
        // .context("Expect ';' after a value")?;
        Ok(Stmt::PrintStmt(expr))
    }

    // return       -> "return" expression? ";";
    fn return_stmt(&mut self) -> Result<Stmt> {
        let expr = if self.check(SEMICOLON){
            None
        }else {
            Some(self.expression())
        };
        self.consume(SEMICOLON); //.context("Expect ';' after return")?;
        Ok(Stmt::ReturnStmt(expr))
    }


    // forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
    //                  expression? ";"
    //                  expression? ")" statement ;
    fn for_stmt(&mut self) -> Result<Stmt> {
        self.consume(TokenType::LeftParen)
            .context("Expect '(' after for.")?;
        let init = if self.check(SEMICOLON) {
            None
        } else if self.next_if_match(VAR) {
            Some(self.var_decl()?)
        } else {
            Some(self.expr_stmt()?)
        };
        let cond = if self.check(SEMICOLON) {
            Expr::Literal(LiteralKind::Boolean(true))
        } else {
            self.expression()
        };
        self.consume(SEMICOLON)
            .context("Expect ';' after loop condition.")?;

        let increment = if self.check(RightParen) {
            None
        } else {
            Some(self.expression())
        };
        self.consume(RightParen)
            .context("Expect ')' after for clause.")?;
        let mut body = self.statement()?;

        if let Some(increment) = increment {
            body = Stmt::BlockStmt(vec![body, Stmt::ExprStmt(increment)]);
        }

        let mut stmt = Stmt::While(Box::new(WhileStmt { cond, body }));
        if let Some(init) = init {
            stmt = Stmt::BlockStmt(vec![init, stmt]);
        }
        Ok(stmt)
    }

    fn if_stmt(&mut self) -> Result<Stmt> {
        self.consume(TokenType::LeftBrace)
            .context("Expect '(' after if.")?;
        let cond = self.expression();
        self.consume(TokenType::RightBrace)
            .context("Expect ')' after if.")?;
        let then = self.statement()?;
        let els = if self.next_if_match(ELSE) {
            Some(self.statement()?)
        } else {
            None
        };
        Ok(Stmt::IF(Box::new(IfStmt { cond, then, els })))
    }

    fn while_stmt(&mut self) -> Result<Stmt> {
        self.consume(TokenType::LeftBrace)
            .context("Expect '(' after while.")?;
        let cond = self.expression();
        self.consume(TokenType::RightBrace)
            .context("Expect ')' after while.")?;
        let body = self.statement()?;
        Ok(Stmt::While(Box::new(WhileStmt { cond, body })))
    }

    fn break_stmt(&mut self) -> Result<Stmt> {
        self.consume(SEMICOLON).context("expect ';' after break")?;
        Ok(Stmt::Break)
    }

    fn expr_stmt(&mut self) -> Result<Stmt> {
        let expr = self.expression();
        self.next_if_match(SEMICOLON);
        // .context("Expect ';' after a value")?;
        Ok(Stmt::ExprStmt(expr))
    }

    // block    -> "{" declaration* "}"
    fn block_stmt(&mut self) -> Result<Vec<Stmt>> {
        let mut stmts = vec![];
        while !self.next_if_match(RightBrace) {
            stmts.push(self.declaration()?);
        }
        Ok(stmts)
    }

    fn expression(&mut self) -> Expr {
        self.assignment()
    }

    fn assignment(&mut self) -> Expr {
        let mut expr = self.ternary();
        if self.next_if_match(EQUAL) {
            let value = self.assignment();
            if let Expr::Variable(var) = expr {
                expr = Expr::Assign(var, Box::new(value))
            } else {
                panic!("Invalid assigment target!")
            }
        }
        expr
    }

    // ternary        -> logic_or "?" logic_or ":" logic_or
    fn ternary(&mut self) -> Expr {
        let cond = self.logic_or();
        if self.next_if_match(QUESTION) {
            let left = self.logic_or();
            self.it
                .next()
                .filter(|token| token.ttype == COLON)
                .expect("expect ':'");
            let right = self.logic_or();
            let ternary = TernaryExpr { cond, left, right };
            Expr::Ternary(Box::new(ternary))
        } else {
            cond
        }
    }

    // logic_or      -> logic_and ( "or" logic_and)* ;
    fn logic_or(&mut self) -> Expr {
        let mut expr = self.logic_and();
        while self.next_if_match(OR) {
            let right = self.logic_and();
            expr = Expr::Logic(Box::new(LogicExpr {
                is_and: false,
                left: expr,
                right,
            }));
        }
        expr
    }

    // logic_and     -> equality ( "and" equality)* ;
    fn logic_and(&mut self) -> Expr {
        let mut expr = self.equality();
        while self.next_if_match(AND) {
            let right = self.equality();
            expr = Expr::Logic(Box::new(LogicExpr {
                is_and: true,
                left: expr,
                right,
            }));
        }
        expr
    }

    // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();
        while let Some(token) = self.it.peek() {
            match &token.ttype {
                BangEqual | EqualEqual => {
                    let operator = self.next_token();
                    let right = self.comparison();
                    expr = Expr::Binary(Box::new(BinaryExpr {
                        left: expr,
                        operator,
                        right,
                    }));
                }
                _ => break,
            }
        }
        expr
    }

    // comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> Expr {
        let mut expr = self.term();
        while let Some(token) = self.it.peek() {
            match &token.ttype {
                GREATER | GreaterEqual | LESS | LessEqual => {
                    let operator = self.next_token();
                    let right = self.term();
                    expr = Expr::Binary(Box::new(BinaryExpr {
                        left: expr,
                        operator,
                        right,
                    }));
                }
                _ => break,
            }
        }
        expr
    }

    // term           → factor ( ( "-" | "+" | "-=" | "+=" ) factor )* ;
    fn term(&mut self) -> Expr {
        let mut expr = self.factor();
        while let Some(token) = self.it.peek() {
            match &token.ttype {
                MINUS | MinusEqual | PLUS | PlusEqual => {
                    let operator = self.next_token();
                    let right = self.factor();
                    expr = Expr::Binary(Box::new(BinaryExpr {
                        left: expr,
                        operator,
                        right,
                    }));
                }
                _ => break,
            }
        }
        expr
    }

    // factor         → unary ( ( "/" | "/=" | "*" | "*=") unary )* ;
    fn factor(&mut self) -> Expr {
        let mut expr = self.unary();
        while let Some(token) = self.it.peek() {
            match &token.ttype {
                STAR | StarEqual | SLASH | SlashEqual => {
                    let operator = self.next_token();
                    let right = self.factor();
                    expr = Expr::Binary(Box::new(BinaryExpr {
                        left: expr,
                        operator,
                        right,
                    }));
                }
                _ => break,
            }
        }
        expr
    }

    // unary          → ( "!" | "-" ) unary | call ;
    fn unary(&mut self) -> Expr {
        if let Some(token) = self.it.peek() {
            match &token.ttype {
                BANG | MINUS => {
                    let operator = self.next_token();
                    let right = self.unary();
                    return Expr::Unary(Box::new(UnaryExpr { operator, right }));
                }
                _ => {}
            }
        }
        self.call()
    }

    // call         -> primary ( "(" arguments? ")" )* ;
    fn call(&mut self) -> Expr {
        let mut expr = self.primary();
        loop {
            if self.next_if_match(LeftParen) {
                expr = self.finish_call(expr);
            } else {
                break;
            }
        }
        expr
    }

    fn finish_call(&mut self, callee: Expr) -> Expr {
        let mut arguments = vec![];
        if !self.check(RightParen) {
            loop {
                assert!(arguments.len() < 255, "Can't have more than 255 arguments.");
                arguments.push(self.expression());
                if !self.next_if_match(COMMA) {
                    break;
                }
            }
        }
        let paren = self
            .consume(RightParen)
            .expect("Expect ')' after arguments.");
        Expr::Call(Box::new(CallExpr {
            callee,
            paren,
            arguments,
        }))
    }

    //primary        → NUMBER | STRING | "true" | "false" | "nil"
    //                | "(" expression ")" | identifier;
    fn primary(&mut self) -> Expr {
        if let Some(token) = self.it.next() {
            return match token.ttype {
                NIL => Literal(LiteralKind::Nil),
                TRUE => Literal(LiteralKind::Boolean(true)),
                FALSE => Literal(LiteralKind::Boolean(false)),
                NUMBER(num) => Literal(LiteralKind::Num(num)),
                STRING(content) => Literal(LiteralKind::String(content)),
                IDENTIFIER(ident) => Variable(ident),
                LeftParen => {
                    let expr = self.expression();
                    self.it
                        .next()
                        .filter(|t| t.ttype == RightParen)
                        .expect("expect ')' after expression.");
                    Expr::Grouping(Box::new(expr))
                }
                _ => {
                    panic!("Expected primary, found: {:?}", token)
                }
            };
        }
        panic!("Expected primary")
    }

    fn check(&mut self, ttype: TokenType) -> bool {
        self.it
            .peek()
            .map(|token| token.ttype == ttype)
            .unwrap_or(false)
    }

    fn next_if_match(&mut self, ttype: TokenType) -> bool {
        let is_match = self.check(ttype);
        if is_match {
            self.it.next();
        }
        is_match
    }

    fn next_token(&mut self) -> Token {
        self.it.next().unwrap()
    }

    fn consume(&mut self, target: TokenType) -> Option<Token> {
        self.it
            .next()
            .filter(|token| matches!(&token.ttype, target))
    }

    // start of a statement
    fn synchronize(&mut self) {
        while let Some(token) = self.it.next() {
            if matches!(&token.ttype, SEMICOLON) {
                return;
            }
            if let Some(token) = self.it.peek() {
                match &token.ttype {
                    CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN => return,
                    _ => {}
                }
            }
        }
    }
}

#[cfg(test)]
#[path = "./parser_test.rs"]
mod parser_test;
