use arret_syntax::span::Span;

use arret_runtime::boxed;

use crate::mir::builder::Builder;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::value::to_const::value_to_const;
use crate::mir::value::Value;

pub enum PartialPrint {
    Constant(String),
    SimplifiedArgs(Box<[Value]>),
}

impl PartialPrint {
    /// Converts the partial print to an arg list suitable for passing to a stdlib print function
    pub fn into_arg_list_value(self, ehx: &mut EvalHirCtx) -> Value {
        match self {
            PartialPrint::Constant(string) => {
                Value::List(Box::new([boxed::Str::new(ehx, &string).into()]), None)
            }
            PartialPrint::SimplifiedArgs(args) => Value::List(args, None),
        }
    }
}

/// Partially pretty prints an arg list value
///
/// If partial printing isn't possible or isn't an improvement over the original arg list, `None`
/// will be returned.
pub fn partial_pretty_print(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Option<PartialPrint> {
    let mut list_iter = arg_list_value.try_sized_list_iter()?;
    let original_arg_count = list_iter.len();

    if original_arg_count < 2 {
        // Nothing we can do to simplify this further
        return None;
    }

    // Accumulates our string literal
    let mut literal_acc = String::new();
    let mut simplified_args: Vec<Value> = vec![];

    while let Some(value) = list_iter.next(b, span) {
        match value_to_const(ehx, &value) {
            Some(boxed) => {
                let mut output: Vec<u8> = vec![];
                arret_runtime_syntax::writer::pretty_print_boxed(&mut output, ehx, boxed);

                literal_acc.push_str(
                    std::str::from_utf8(&output)
                        .expect("pretty printed invalid UTF-8 during partial print"),
                );
            }
            None => {
                if !literal_acc.is_empty() {
                    simplified_args.push(boxed::Str::new(ehx, &literal_acc).into());
                    literal_acc.clear();
                }

                simplified_args.push(value);
            }
        };
    }

    if simplified_args.is_empty() {
        Some(PartialPrint::Constant(literal_acc))
    } else {
        // Push on the end of the accumulator
        if !literal_acc.is_empty() {
            simplified_args.push(boxed::Str::new(ehx, &literal_acc).into());
        }

        if simplified_args.len() < original_arg_count {
            Some(PartialPrint::SimplifiedArgs(
                simplified_args.into_boxed_slice(),
            ))
        } else {
            // Didn't improve anything
            None
        }
    }
}
