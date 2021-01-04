use crate::ast::{TokenType::*, *};
use std::{iter::Peekable};
use std::vec::IntoIter;

pub struct Parser {
    it: Peekable<IntoIter<Token>>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        let it: Peekable<IntoIter<Token>> = tokens.into_iter().peekable();
        Parser { it }
    }

    pub fn parse(tokens: Vec<Token>) -> Expr {
        Self::new(tokens).expression()
    }

    fn expression(&mut self) -> Expr {
        self.equality()
    }

    // ternary        -> equality "?" equality ":" equality
    fn ternary(&mut self) -> Expr {
        let cond = self.equality();
        self.it.next().filter(|token| token.ttype == QUESTION).expect("expect '?'");
        let left = self.equality();
        self.it.next().filter(|token| token.ttype == COMMA).expect("expect ':'");
        let right = self.equality();
        let ternary = TernaryExpr {cond, left, right };
        Expr::Ternary(Box::new(ternary))
    }

    // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();
        while let Some(token) = self.it.peek() {
            match &token.ttype {
                BangEqual | EqualEqual => {
                    let operator = self.consume();
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
                    let operator = self.consume();
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
                    let operator = self.consume();
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
                    let operator = self.consume();
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
                    let operator = self.consume();
                    let right = self.unary();
                    return Expr::Unary(Box::new(UnaryExpr { operator, right }));
                }
                _ => {}
            }
        }
        self.primary()
    }

    //primary        → NUMBER | STRING | "true" | "false" | "nil"
    //                | "(" expression ")" ;
    fn primary(&mut self) -> Expr {
        if let Some(token) = self.it.next() {
            return match &token.ttype {
                FALSE | TRUE | NIL => Expr::Literal(token),
                NUMBER(num) => Expr::Literal(token),
                STRING(content) => Expr::Literal(token),
                IDENTIFIER(ident) => Expr::Literal(token),
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

    fn consume(&mut self) -> Token {
        self.it.next().unwrap()
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
