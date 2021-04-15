use anyhow::{bail, Context, Result};
use std::iter::Peekable;
use std::vec::IntoIter;

use crate::scanner::{
    Scanner, Token,
    TokenType::{self, *},
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Start,
    AssignmentPrev,
    Assignment, // =
    Conditional,// ?:
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparision,// < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Exponent,   // ^
    Postfix,
    Call,       // . ()
}
#[derive(Debug)]
enum Expr {
    Prefix(TokenType, Box<Expr>),
    Number(f64),
    Identifier(String),
    Grouping(Box<Expr>),
    Binary(Box<BinaryExpr>),
}

#[derive(Debug)]
struct BinaryExpr {
    left: Expr,
    ttype: TokenType,
    right: Expr,
}

impl Expr {
    fn format(&self) -> String {
        use Expr::*;
        match self {
            Prefix(ttype, expr) => match ttype {
                MINUS => format!("-({})", expr.format()),
                _ => todo!(),
            },
            Number(num) => format!("{}", *num),
            Identifier(id) => id.clone(),
            Grouping(expr) => format!("({})", expr.format()),
            Binary(expr) => match &expr.ttype {
                MINUS => format!("-({},{})", expr.left.format(), expr.right.format()),
                PLUS => format!("+({},{})", expr.left.format(), expr.right.format()),
                STAR => format!("*({},{})", expr.left.format(), expr.right.format()),
                SLASH => format!("/({},{})", expr.left.format(), expr.right.format()),
                Exponent => format!("^({},{})", expr.left.format(), expr.right.format()),
                EQUAL => format!("=({},{})", expr.left.format(), expr.right.format()),
                _ => todo!(),
            },
        }
    }
}

pub struct Parser {
    it: Peekable<IntoIter<Token>>,
}

impl Parser {
    fn parse(&mut self) -> Result<Expr> {
        self.parse_expr(Precedence::Start)
    }

    // parse expr with larger precedence
    fn parse_expr(&mut self, precedence: Precedence) -> Result<Expr> {
        let token = self.it.next().context("empty")?;
        let mut left = self.parse_prefix(token)?;
        while precedence < self.next_precedence() {
            let token = self.it.next().unwrap();
            left = self.parse_infix(left, token)?;
        }
        Ok(left)
    }

    fn parse_prefix(&mut self, token: Token) -> Result<Expr> {
        let result = match token.ttype {
            NUMBER(num) => Expr::Number(num),
            IDENTIFIER(id) => Expr::Identifier(id.into()),
            MINUS | PLUS => {
                Expr::Prefix(token.ttype, Box::new(self.parse_expr(Precedence::Unary)?))
            }
            LeftParen => {
                let expr = self.parse()?;
                self.consume(RightParen)
                    .context("expect ')' for grouping")?;
                Expr::Grouping(Box::new(expr))
            }
            _ => return bail!("not support token: {:?}", token),
        };
        Ok(result)
    }

    fn parse_infix(&mut self, left: Expr, token: Token) -> Result<Expr> {
        let result = match &token.ttype {
            PLUS | MINUS => {
                let ttype = token.ttype;
                let right = self.parse_expr(Precedence::Term)?;
                Expr::Binary(Box::new(BinaryExpr { left, ttype, right }))
            }
            STAR | SLASH => {
                let ttype = token.ttype;
                let right = self.parse_expr(Precedence::Factor)?;
                Expr::Binary(Box::new(BinaryExpr { left, ttype, right }))
            }
            Exponent => {
                let ttype = token.ttype;
                let right = self.parse_expr(Precedence::Exponent)?;
                Expr::Binary(Box::new(BinaryExpr { left, ttype, right }))
            }
            EQUAL => {
                let ttype = token.ttype;
                let right = self.parse_expr(Precedence::AssignmentPrev)?;
                Expr::Binary(Box::new(BinaryExpr { left, ttype, right }))
            }
            _ => bail!("unexpected token: {:?}", token),
        };
        Ok(result)
    }

    fn consume(&mut self, target: TokenType) -> Option<Token> {
        self.it
            .next()
            .filter(|token| matches!(&token.ttype, target))
    }

    fn next_precedence(&mut self) -> Precedence {
        self.it
            .peek()
            .map(|token| match token.ttype {
                MINUS | PLUS => Precedence::Term,
                STAR | SLASH => Precedence::Factor,
                Exponent => Precedence::Exponent,
                EQUAL => Precedence::Assignment,
                _ => Precedence::Start,
            })
            .unwrap_or(Precedence::Start)
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    fn assert_match(source: &str, expr: &str) {
        let tokens = Scanner::scan(source);
        println!("tokens: {:?}", tokens);
        let it = tokens.into_iter().peekable();
        let mut parser = Parser { it };
        let result = parser.parse();
        match result {
            Ok(ex) => assert_eq!(ex.format(), expr),
            Err(e) => panic!("{:?}", e),
        }
    }
    #[test]
    fn test() {
        assert_match("2 + 3", "+(2,3)");
        assert_match("2 + 3 - 4", "-(+(2,3),4)");
        assert_match("--2 + 3", "+(-(-(2)),3)");
        assert_match("2 + 3 * 4", "+(2,*(3,4))");
        assert_match("-2^2 + 3", "+(-(^(2,2)),3)");
    }

    #[test]
    fn test_assign() {
        assert_match("a = b", "=(a,b)");
        assert_match("a = b + c", "=(a,+(b,c))");
        assert_match("a = b = c", "=(a,=(b,c))");
    }
}
