use crate::ast::{*, Expr::*};

pub fn print(expr: &Expr){
    println!("{}", format(expr));
}

fn format(expr: &Expr) -> String {
    match expr {
        Literal(ttype) => format!("Literal: {:?}", ttype),
        Unary(unary) => format!("Unary, operator: {:?}, right: {:?}", unary.operator, format(&unary.right)),
        Binary(expr) => format!("Binary, left: {:?}, operator: {:?}, right: {:?}", format(&expr.left), expr.operator, format(&expr.right)),
        Grouping(expr) => format!("Grouping, expr: {:?}", format(&*expr)),
        Ternary(expr) => format!("Ternary, cond: {:?}, left: {:?}, right: {:?}", format(&expr.cond), format(&expr.left), format(&expr.right)),
    }
}