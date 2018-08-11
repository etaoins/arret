use crate::abitype::{ABIType, RetABIType};

#[derive(Debug)]
pub struct RustFun {
    pub arret_type: &'static str,
    pub takes_task: bool,
    pub params: &'static [ABIType],
    pub ret: RetABIType,
    pub entry_point: &'static str,
}

pub type RustExports = &'static [(&'static str, &'static RustFun)];

#[macro_export]
macro_rules! define_rust_fn {
    (#[arret-type=$type:expr] $desc_name:ident = fn $func_name:ident($task_name:ident : &mut Task, $($param_name:ident : $rust_ty:ty),*) -> $ret:ty $body:block) => {
        use crate::abitype::{EncodeABIType, EncodeRetABIType};

        #[no_mangle]
        pub extern "C" fn $func_name($task_name: &mut Task, $($param_name: $rust_ty),*) -> $ret {
            $body
        }

        const $desc_name: RustFun = RustFun {
            arret_type: $type,
            takes_task: true,
            params: &[
                $(<$rust_ty as EncodeABIType>::ABI_TYPE),*
            ],
            ret: <$ret as EncodeRetABIType>::RET_ABI_TYPE,
            entry_point: stringify!($func_name),
        };
    };

    (#[arret-type=$type:expr] $desc_name:ident = fn $func_name:ident($($param_name:ident : $rust_ty:ty),*) -> $ret:ty $body:block) => {
        #[no_mangle]
        pub extern "C" fn $func_name($($param_name: $rust_ty),*) -> $ret {
            $body
        }

        const $desc_name: RustFun = RustFun {
            arret_type: $type,
            takes_task: false,
            params: &[
                $(<$rust_ty as EncodeABIType>::ABI_TYPE),*
            ],
            ret: <$ret as EncodeRetABIType>::RET_ABI_TYPE,
            entry_point: stringify!($func_name),
        };
    };
}

#[macro_export]
macro_rules! define_rust_module {
    ($($export_name:expr => $desc_name:ident),*) => {
        #[no_mangle]
        pub static ARRET_ABI_VERSION: u32 = 1;

        #[no_mangle]
        pub static ARRET_RUST_EXPORTS: RustExports = &[
            $(
                ($export_name, &$desc_name)
            ),*
        ];
    };
}

// This needs to be pub because we export functions
#[cfg(test)]
pub mod test {
    use crate::abitype::BoxedABIType;
    use crate::boxed;
    use crate::boxed::prelude::*;
    use crate::boxed::refs::Gc;
    use crate::task::Task;

    use super::*;

    define_rust_fn! {
        #[arret-type="(-> Int)"]
        RETURN_42 = fn return_42() -> i64 {
            42
        }
    }

    #[test]
    fn return_42_fn() {
        assert_eq!("return_42", RETURN_42.entry_point);
        assert_eq!(false, RETURN_42.takes_task);

        assert_eq!(true, RETURN_42.params.is_empty());
        assert_eq!(RetABIType::Inhabited(ABIType::Int), RETURN_42.ret);

        let number = return_42();
        assert_eq!(42, number);
    }

    define_rust_fn! {
    #[arret-type="(Int Float -> Num)"]
        ADD_INT_FLOAT = fn add_int_float(
            task: &mut Task,
            int_box: Gc<boxed::Int>,
            native_float: f64
        ) -> Gc<boxed::Num> {
            boxed::Int::new(task, int_box.value() + native_float as i64).as_num_ref()
        }
    }

    define_rust_module! {
        "length" => LENGTH,
        "return-42" => RETURN_42
    }

    #[test]
    fn add_int_float_fn() {
        let mut task = Task::new();

        assert_eq!("add_int_float", ADD_INT_FLOAT.entry_point);
        assert_eq!(true, ADD_INT_FLOAT.takes_task);
        assert_eq!("(Int Float -> Num)", ADD_INT_FLOAT.arret_type);

        assert_eq!(
            vec![
                ABIType::Boxed(BoxedABIType::DirectTagged(boxed::TypeTag::Int)),
                ABIType::Float,
            ],
            ADD_INT_FLOAT.params
        );
        assert_eq!(
            RetABIType::Inhabited(ABIType::Boxed(BoxedABIType::Union(
                "Num",
                &[boxed::TypeTag::Int, boxed::TypeTag::Float,]
            ))),
            ADD_INT_FLOAT.ret
        );

        let fourteen_int = boxed::Int::new(&mut task, 14);
        let twenty_num = add_int_float(&mut task, fourteen_int, 6.0);

        let twenty_int = twenty_num.downcast_ref::<boxed::Int>().unwrap();
        assert_eq!(20, twenty_int.value());
    }

    define_rust_fn! {
        #[arret-type="((Listof Any) -> Int)"]
        LENGTH = fn length(input: Gc<boxed::List<boxed::Any>>) -> i64 {
            input.len() as i64
        }
    }

    #[test]
    fn length_fn() {
        let mut task = Task::new();

        assert_eq!("length", LENGTH.entry_point);
        assert_eq!(false, LENGTH.takes_task);
        assert_eq!("((Listof Any) -> Int)", LENGTH.arret_type);

        assert_eq!(
            vec![ABIType::Boxed(BoxedABIType::List(&BoxedABIType::Any))],
            LENGTH.params
        );
        assert_eq!(RetABIType::Inhabited(ABIType::Int), LENGTH.ret);

        let boxed_ints = [1, 2, 3]
            .iter()
            .map(|num| boxed::Int::new(&mut task, *num).as_any_ref())
            .collect::<Vec<Gc<boxed::Any>>>();

        let boxed_list = boxed::List::new(&mut task, boxed_ints.into_iter());
        assert_eq!(3, length(boxed_list));
    }

    define_rust_fn! {
        #[arret-type="(->! ())"]
        EMPTY_IMPURE = fn empty_impure() -> () {}
    }

    #[test]
    fn empty_impure_fn() {
        assert_eq!("empty_impure", EMPTY_IMPURE.entry_point);
        assert_eq!(false, EMPTY_IMPURE.takes_task);
        assert_eq!("(->! ())", EMPTY_IMPURE.arret_type);

        assert_eq!(true, EMPTY_IMPURE.params.is_empty());
        assert_eq!(RetABIType::Void, EMPTY_IMPURE.ret);

        empty_impure();
    }

    #[test]
    fn rust_module() {
        assert_eq!(2, ARRET_RUST_EXPORTS.len());
    }
}
