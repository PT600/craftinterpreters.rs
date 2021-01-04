use crate::ast::{Expr::*, TokenType::*, *};
use super::*;

macro_rules! parser {
        ($($x:expr),+ $(,)?) => (
            Parser::new(vec![$(Token{ttype: $x, line: 0}),+])
        );
    }

fn token(ttype: TokenType) -> Token {
    Token { ttype, line: 0 }
}

fn literal(ttype: TokenType) -> Expr {
    Literal(token(ttype))
}

#[test]
fn primary() {
    let mut parser = parser!(TRUE);
    let expr = parser.primary();
    assert_eq!(expr, literal(TRUE));

    let mut parser = parser!(IDENTIFIER("a".into()));
    let expr = parser.primary();
    assert_eq!(expr, literal(IDENTIFIER("a".into())));
}

#[test]
fn unary() {
    let mut parser = parser!(BANG, IDENTIFIER("a".into()));
    let expr = parser.unary();
    let target = UnaryExpr {
        operator: token(BANG),
        right: literal(IDENTIFIER("a".into())),
    };
    assert_eq!(expr, Unary(Box::new(target)));
}

#[test]
fn factor(){
    let mut parser = parser!(MINUS, IDENTIFIER("a".into()), STAR, NUMBER(5f64));
    let expr = parser.factor();
    let left = UnaryExpr {
        operator: token(MINUS),
        right: literal(IDENTIFIER("a".into())),
    };
    let target = BinaryExpr {
        left: Unary(Box::new(left)),
        operator: token(STAR),
        right: literal(NUMBER(5f64)),
    };
    assert_eq!(expr, Binary(Box::new(target)));
}

#[test]
fn term(){
    let mut parser = parser!(NUMBER(10f64), MINUS, IDENTIFIER("a".into()), STAR, NUMBER(5f64));
    let expr = parser.term();
    let right = BinaryExpr {
        left: literal(IDENTIFIER("a".into())),
        operator: token(STAR),
        right: literal(NUMBER(5f64)),
    };
    let target = BinaryExpr {
        left: literal(NUMBER(10f64)),
        operator: token(MINUS),
        right: Binary(Box::new(right)),
    };
    assert_eq!(expr, Binary(Box::new(target)));
}

#[test]
fn comparison(){
    let mut parser = parser!(IDENTIFIER("a".into()), GreaterEqual, NUMBER(5f64));
    let expr = parser.comparison();
    let target = BinaryExpr {
        left: literal(IDENTIFIER("a".into())),
        operator: token(GreaterEqual),
        right: literal(NUMBER(5f64)),
    };
    assert_eq!(expr, Binary(Box::new(target)));
}

#[test]
fn equality(){
    let mut parser = parser!(IDENTIFIER("a".into()), BangEqual, NUMBER(5f64));
    let expr = parser.equality();
    let target = BinaryExpr {
        left: literal(IDENTIFIER("a".into())),
        operator: token(BangEqual),
        right: literal(NUMBER(5f64)),
    };
    assert_eq!(expr, Binary(Box::new(target)));
}