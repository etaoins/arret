use hir::scope::{NsDatum, Scope};
use ty::{NonFun, PTy};
use hir::error::Result;

pub fn lower_pty(_scope: &Scope, datum: NsDatum) -> Result<PTy> {
    match datum {
        NsDatum::Bool(_, v) => Ok(NonFun::Bool(v).into()),
        _ => {
            unimplemented!("HERE!");
        }
    }
}
