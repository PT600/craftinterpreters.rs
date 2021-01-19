use std::collections::HashMap;

use crate::scanner::Scanner;
use crate::ast::*;
use crate::scanner::*;


#[test]
fn test_scanner() {
    let source = "var a = 1\n var b = a + 1";
    let tokens = Scanner::scan(source);
    println!("tokens: {:?}", tokens);
    assert_eq!(tokens[0], Token{ttype: TokenType::VAR, line: 0});
    assert_eq!(tokens[1], Token{ttype: TokenType::IDENTIFIER("a".into()), line: 0});
    assert_eq!(tokens[2], Token{ttype: TokenType::EQUAL, line: 0});
    assert_eq!(tokens[3], Token{ttype: TokenType::NUMBER(1f64), line: 0});
    assert_eq!(tokens[4], Token{ttype: TokenType::VAR, line: 1});
    assert_eq!(tokens[5], Token{ttype: TokenType::IDENTIFIER("b".into()), line: 1});
    assert_eq!(tokens[6], Token{ttype: TokenType::EQUAL, line: 1});
    assert_eq!(tokens[7], Token{ttype: TokenType::IDENTIFIER("a".into()), line: 1});
    assert_eq!(tokens[8], Token{ttype: TokenType::PLUS, line: 1});
    assert_eq!(tokens[9], Token{ttype: TokenType::NUMBER(1f64), line: 1});
    // assert_eq!(tokens[10], Token{ttype: TokenType::EOF, line: 1});
}