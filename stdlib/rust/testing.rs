use runtime::abitype::*;
use runtime::binding::*;

use runtime::boxed;
use runtime::boxed::refs::Gc;

define_rust_fn! {
    #[arret_type="(All #{T} T -> T)"]
    pub BLACK_BOX = fn arret_stdlib_black_box(value: Gc<boxed::Any>) -> Gc<boxed::Any> {
        value
    }
}

define_rust_fn! {
    #[arret_type="(All #{T} T ->! T)"]
    pub BLACK_BOX_IMPURE = fn arret_stdlib_black_box_impure(value: Gc<boxed::Any>) -> Gc<boxed::Any> {
        value
    }
}
