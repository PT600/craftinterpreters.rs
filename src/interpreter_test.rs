use crate::{ast::{Expr, Value}, parser::{self, Parser}, scanner::Scanner};
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
    let val = it.env(&"a".to_string());
    assert!(val.is_ok());
    let result = val.unwrap();
    assert_eq!(result, &Value::String("abc".into()));

    // assign
    let result = eval(&mut it, "a = 5;");
    assert!(result.is_ok());

    // get
    let result = it.evaluate(&Expr::Variable("a".to_string()));
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Num(5f64));

    let result = eval(&mut it, "b = 5;");
    assert!(result.is_err());

}

