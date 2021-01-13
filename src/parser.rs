use crate::ast::Expr;
use crate::ast::{Expr::*, TokenType::*, *};
use anyhow::{bail, Context, Result};
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

    // decl     -> var_decl | statement
    fn declaration(&mut self) -> Result<Stmt> {
        let stmt = if self.next_if_match(VAR) {
            self.var_decl()
        } else {
            self.statement()
        };
        if stmt.is_err() {
            self.synchronize();
        }
        stmt
    }

    // var_decl   -> "var" identifier ("=" expression)? ";";
    fn var_decl(&mut self) -> Result<Stmt> {
        if let Some(token) = self.it.next() {
            match token.ttype {
                IDENTIFIER(var) => {
                    let stmt = if self.next_if_match(EQUAL) {
                        Stmt::VarDecl(var, Some(self.expression()))
                    } else {
                        Stmt::VarDecl(var, None)
                    };
                    self.consume(SEMICOLON)
                        .context("Expect ';' after a value")?;
                    return Ok(stmt);
                }
                _ => {}
            }
        }
        bail!("expect identifier!")
    }

    // statement  ->  expr stmt |  print stmt | block;
    fn statement(&mut self) -> Result<Stmt> {
        if self.next_if_match(PRINT) {
            self.print_stmt()
        } else if self.next_if_match(LeftBrace) {
            self.block_stmt()
        } else {
            self.expr_stmt()
        }
    }

    fn print_stmt(&mut self) -> Result<Stmt> {
        let expr = self.expression();
        self.consume(SEMICOLON)
            .context("Expect ';' after a value")?;
        Ok(Stmt::PrintStmt(expr))
    }

    fn expr_stmt(&mut self) -> Result<Stmt> {
        let expr = self.expression();
        self.consume(SEMICOLON)
            .context("Expect ';' after a value")?;
        Ok(Stmt::ExprStmt(expr))
    }

    // block    -> "{" declaration* "}"
    fn block_stmt(&mut self) -> Result<Stmt> {
        let mut stmts = vec![];
        while !self.next_if_match(RightBrace) {
            stmts.push(self.declaration()?);
        }
        Ok(Stmt::BlockStmt(stmts))
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

    // ternary        -> equality "?" equality ":" equality
    fn ternary(&mut self) -> Expr {
        let cond = self.equality();
        if self.next_if_match(QUESTION) {
            let left = self.equality();
            self.it
                .next()
                .filter(|token| token.ttype == COLON)
                .expect("expect ':'");
            let right = self.equality();
            let ternary = TernaryExpr { cond, left, right };
            Expr::Ternary(Box::new(ternary))
        } else {
            cond
        }
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

    // term           → factor ( ( "-" | "+" ) factor )* ;
    fn term(&mut self) -> Expr {
        let mut expr = self.factor();
        while let Some(token) = self.it.peek() {
            match &token.ttype {
                MINUS | PLUS => {
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

    // factor         → unary ( ( "/" | "*" ) unary )* ;
    fn factor(&mut self) -> Expr {
        let mut expr = self.unary();
        while let Some(token) = self.it.peek() {
            match &token.ttype {
                STAR | SLASH => {
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

    // unary          → ( "!" | "-" ) unary | primary ;
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
        self.primary()
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
