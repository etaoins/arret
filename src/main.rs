mod syntax;
mod ty;
mod hir;

use syntax::parser;
use hir::lowering::LoweringContext;

fn main() {
    let data = parser::data_from_str("2").unwrap();
    let mut lcx = LoweringContext::new();
    let exprs = lcx.lower_module(data).unwrap();

    println!("{:?}", exprs);
}
