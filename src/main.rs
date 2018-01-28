mod syntax;
mod ty;

use syntax::parser;

fn main() {
    println!(
        "{:?}",
        parser::data_from_str(
            "(define x 1)
             (define y 2)
             (write (+ x y))"
        )
    );

    println!("{:?}", parser::datum_from_str("2"));
}
