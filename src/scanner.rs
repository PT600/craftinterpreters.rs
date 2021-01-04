use crate::ast::{TokenType::*, *};
use anyhow::{bail, Result};
use std::collections::HashSet;
use std::iter::{FromIterator, Peekable};
use std::str::Chars;

pub struct Scanner<'a> {
    it: Peekable<Chars<'a>>,
    line: usize,
    keywords: HashSet<&'static str>,
    pub tokens: Vec<Token>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        let keywords = vec![
            "and", "class", "else", "false", "for", "fun", "if", "nil", "or", "print", "return",
            "super", "this", "true", "var", "while",
        ];
        let keywords = HashSet::from_iter(keywords.into_iter());
        Scanner {
            it: source.chars().peekable(),
            line: 0,
            keywords,
            tokens: vec![],
        }
    }

    pub fn scan(source: &'a str) -> Vec<Token> {
        let mut scanner = Self::new(source);
        scanner.scan_tokens();
        scanner.tokens
    }

    pub fn scan_tokens(&mut self) -> Result<()> {
        while let Some(c) = self.it.next() {
            match c {
                '(' => self.add_token(LeftParen),
                ')' => self.add_token(RightParen),
                '{' => self.add_token(LeftBrace),
                '}' => self.add_token(RightBrace),
                ',' => self.add_token(COMMA),
                '.' => self.add_token(DOT),
                '-' => self.add_token(MINUS),
                '+' => self.add_token(PLUS),
                ';' => self.add_token(SEMICOLON),
                '*' => self.add_token(STAR),
                '?' => self.add_token(QUESTION),
                '!' => self.add_token_if_match('=', BangEqual, BANG),
                '=' => self.add_token_if_match('=', EqualEqual, EQUAL),
                '<' => self.add_token_if_match('=', LessEqual, LESS),
                '>' => self.add_token_if_match('=', GreaterEqual, GREATER),
                '/' => {
                    if self.next_if_match('/') {
                        while self.next_ifnot_match('\n') {}
                    } else if self.next_if_match('*') {
                        self.parse_comment()?;
                    } else {
                        self.add_token(SLASH);
                    }
                }
                ' ' | '\r' | '\t' => {}
                '\n' => self.line += 1,
                '"' => self.parse_str()?,
                '0'..='9' => self.parse_num(c)?,
                'a'..='z' | 'A'..='Z' | '_' => self.parse_id(c)?,
                _ => todo!("Unexpected char!"),
            }
        }
        Ok(())
    }

    fn add_token(&mut self, ttype: TokenType) {
        let token = Token {
            ttype,
            line: self.line,
        };
        self.tokens.push(token);
    }

    fn add_token_if_match(&mut self, target: char, yes: TokenType, no: TokenType) {
        let ttype = if self.next_if_match(target) { yes } else { no };
        self.add_token(ttype);
    }

    fn parse_str(&mut self) -> Result<()> {
        let mut content = String::new();
        while let Some(c) = self.it.next() {
            if c == '"' {
                break;
            } else if c == '\n' {
                self.line += 1;
            }
            content.push(c);
        }
        self.add_token(STRING(content));
        Ok(())
    }

    fn parse_num(&mut self, c: char) -> Result<()> {
        let mut content = String::new();
        content.push(c);
        while let Some(c) = self.it.peek() {
            match *c {
                '0'..='9' | '.' => {
                    content.push(*c);
                    self.it.next();
                }
                _ => break,
            }
        }
        let ttype = NUMBER(content.parse()?);
        self.add_token(ttype);
        Ok(())
    }

    fn parse_id(&mut self, c: char) -> Result<()> {
        let mut content = String::new();
        content.push(c);
        while let Some(c) = self.it.peek() {
            match *c {
                '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => {
                    content.push(*c);
                    self.it.next();
                }
                _ => break,
            }
        }
        let token = if self.keywords.contains(content.as_str()) {
            KEYWORD(content)
        } else {
            IDENTIFIER(content)
        };
        self.add_token(token);
        Ok(())
    }

    fn parse_comment(&mut self) -> Result<()> {
        while let Some(c) = self.it.next() {
            if c == '*' {
                if let Some(c) = self.it.peek() {
                    if *c == '/' {
                        self.it.next();
                        return Ok(());
                    }
                }
            } else if c == '\n' {
                self.line += 1;
            }
        }
        bail!("Un terminated comment!")
    }

    fn peek_ifnot_match(&mut self, target: char) -> bool {
        self.it.peek().map(|c| *c != target).unwrap_or(false)
    }

    fn next_ifnot_match(&mut self, target: char) -> bool {
        let not_match = self.peek_ifnot_match(target);
        if not_match {
            self.it.next();
        }
        not_match
    }

    fn peek_if_match(&mut self, target: char) -> bool {
        self.it.peek().map(|c| *c == target).unwrap_or(false)
    }

    fn next_if_match(&mut self, target: char) -> bool {
        let is_match = self.peek_if_match(target);
        if is_match {
            self.it.next();
        }
        is_match
    }
}

#[cfg(test)]
#[path="./scanner_test.rs"]
mod scanner_test;