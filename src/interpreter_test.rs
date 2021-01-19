use std::{cell::RefCell, rc::Rc};

use crate::{ast::{Expr, Value}, enviorment::Env, parser::{self, Parser}, scanner::Scanner};
use anyhow::{Result, Context};

use super::Interpreter;


fn eval(interpreter: &mut Interpreter, source: &'static str) -> Result<()> {
    let tokens = Scanner::scan(source);
    let mut parser = Parser::new(tokens);
    let result = parser.parse();
    assert_eq!(result.len(), 1);
    let result = &result[0];
    let result = result.as_ref().unwrap();
    interpreter.eval(result)
}

#[test]
fn varirable(){
    let mut it = Interpreter::new();
    let result = eval(&mut it, "var a = \"abc\";");
    assert!(result.is_ok());
    let val = it.lookup(&"a".into());
    assert!(val.is_ok());
    let result = val.unwrap();
    assert!(result.is_equals(&Value::String("abc".into())));

    // assign
    let result = eval(&mut it, "a = 5;");
    assert!(result.is_ok());

    // get
    let result = it.eval_expr(&Expr::Variable("a".into()), &it.globals.clone());
    assert!(result.is_ok());
    assert!(result.unwrap().is_equals(&Value::Num(5f64)));

    let result = eval(&mut it, "b = 5;");
    assert!(result.is_err());

}

