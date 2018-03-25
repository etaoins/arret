use hir::scope::{Binding, NsDatum, Prim, Scope};

/// Splits data in to fixed arguments and an optional rest argument
///
/// The rest argument is denoted by using ... at the end of the data
pub fn split_into_fixed_and_rest(
    scope: &Scope,
    mut vs: Vec<NsDatum>,
) -> (Vec<NsDatum>, Option<NsDatum>) {
    let has_rest = if vs.len() >= 2 {
        match &vs[vs.len() - 1] {
            &NsDatum::Ident(_, ref ident) => {
                scope.get(ident) == Some(Binding::Prim(Prim::Ellipsis))
            }
            _ => false,
        }
    } else {
        false
    };

    let rest_datum = if has_rest {
        // Remove the ellipsis completely
        vs.pop();
        Some(vs.pop().unwrap())
    } else {
        None
    };

    (vs, rest_datum)
}
