use crate::ast::{TokenType::*, *};
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

    pub fn parse(tokens: Vec<Token>) -> Expr {
        Self::new(tokens).expression()
    }

    pub fn expression(&mut self) -> Expr {
        self.equality()
    }

    // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    pub fn equality(&mut self) -> Expr {
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
    pub fn comparison(&mut self) -> Expr {
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
    pub fn term(&mut self) -> Expr {
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
    pub fn factor(&mut self) -> Expr {
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
    pub fn unary(&mut self) -> Expr {
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
    pub fn primary(&mut self) -> Expr {
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
}