use std::collections::HashMap;
use std::io::{Result, Write};
use std::iter;

use arret_syntax::span::Span;

use crate::codegen::GenABI;
use crate::mir::ops;
use crate::mir::BuiltProgram;
use crate::source::SourceLoader;
use crate::ty::conv_abi::ConvertableABIType;

fn span_to_human_location(source_loader: Option<&SourceLoader>, span: Span) -> Option<String> {
    let span_start = span.start();

    let source_loader = source_loader?;
    let code_map = source_loader.code_map();
    let file_map = code_map.find_file(span_start)?;

    let (line, column) = file_map.location(span_start).ok()?;

    Some(format!(
        "{}:{}:{}",
        file_map.name(),
        line.number(),
        column.number()
    ))
}

fn private_fun_to_string(
    private_funs: &HashMap<ops::PrivateFunId, ops::Fun>,
    private_fun_id: ops::PrivateFunId,
) -> String {
    private_funs[&private_fun_id]
        .source_name
        .clone()
        .map(|s| format!("%{}", s))
        .unwrap_or_else(|| format!("[private-{}]", private_fun_id.to_u32()))
}

fn callee_to_string(
    private_funs: &HashMap<ops::PrivateFunId, ops::Fun>,
    callee: &ops::Callee,
) -> String {
    match callee {
        ops::Callee::StaticSymbol(static_symbol) => format!("@{}", static_symbol.symbol),
        ops::Callee::PrivateFun(private_fun_id) => {
            private_fun_to_string(private_funs, *private_fun_id)
        }
        ops::Callee::BoxedFunThunk(thunk_reg) => format!("%{}", thunk_reg.to_usize()),
    }
}
fn callee_to_gen_abi(
    private_funs: &HashMap<ops::PrivateFunId, ops::Fun>,
    callee: &ops::Callee,
) -> GenABI {
    match callee {
        ops::Callee::StaticSymbol(static_symbol) => static_symbol.abi.clone(),
        ops::Callee::PrivateFun(private_fun_id) => (&private_funs[&private_fun_id].abi).into(),
        ops::Callee::BoxedFunThunk(_) => GenABI::thunk_abi(),
    }
}

fn box_pair_op_to_string(
    ops::BoxPairOp {
        head_reg,
        rest_reg,
        length_reg,
    }: &ops::BoxPairOp,
) -> String {
    format!(
        "boxed::Pair {{ head: %{}, rest: %{}, length: %{} }}",
        head_reg.to_usize(),
        rest_reg.to_usize(),
        length_reg.to_usize()
    )
}

fn box_fun_thunk_op_to_string(
    private_funs: &HashMap<ops::PrivateFunId, ops::Fun>,
    ops::BoxFunThunkOp {
        closure_reg,
        callee,
    }: &ops::BoxFunThunkOp,
) -> String {
    format!(
        "boxed::FunThunk {{ closure: %{}, entry: {} }}",
        closure_reg.to_usize(),
        callee_to_string(private_funs, callee)
    )
}

fn box_record_op_to_string(
    ops::BoxRecordOp {
        record_struct,
        field_regs,
    }: &ops::BoxRecordOp,
) -> String {
    let field_strings = field_regs
        .iter()
        .zip(record_struct.field_abi_types.iter())
        .map(|(field_reg, field_abi_type)| {
            format!(
                "%{}: {}",
                field_reg.to_usize(),
                field_abi_type.to_rust_str()
            )
        })
        .collect::<Vec<String>>()
        .join(", ");

    format!(
        "record::{} {{ {} }}",
        record_struct.source_name, field_strings
    )
}

fn print_branch(
    w: &mut dyn Write,
    private_funs: &HashMap<ops::PrivateFunId, ops::Fun>,
    ident_level: usize,
    ops: &[ops::Op],
) -> Result<()> {
    for op in ops.iter() {
        for _ in 0..ident_level {
            write!(w, "  ")?;
        }

        match &op.kind {
            ops::OpKind::ConstBoxedNil(reg, _) => {
                writeln!(w, "%{} = const boxed::NIL_INSTANCE;", reg.to_usize())?;
            }
            ops::OpKind::ConstInt64(reg, value) => {
                writeln!(w, "%{} = const {}: i64;", reg.to_usize(), value)?
            }
            ops::OpKind::CastBoxed(reg, ops::CastBoxedOp { from_reg, to_type }) => writeln!(
                w,
                "%{} = %{} as {};",
                reg.to_usize(),
                from_reg.to_usize(),
                to_type.to_rust_str()
            )?,
            ops::OpKind::ConstCastBoxed(reg, ops::CastBoxedOp { from_reg, to_type }) => writeln!(
                w,
                "%{} = const %{} as {};",
                reg.to_usize(),
                from_reg.to_usize(),
                to_type.to_rust_str()
            )?,
            ops::OpKind::ConstBoxedPair(reg, box_pair_op) => {
                writeln!(
                    w,
                    "%{} = const {};",
                    reg.to_usize(),
                    box_pair_op_to_string(box_pair_op)
                )?;
            }
            ops::OpKind::AllocBoxedPair(reg, box_pair_op) => {
                writeln!(
                    w,
                    "%{} = alloc {};",
                    reg.to_usize(),
                    box_pair_op_to_string(box_pair_op)
                )?;
            }
            ops::OpKind::ConstBoxedFunThunk(reg, box_fun_thunk_op) => {
                writeln!(
                    w,
                    "%{} = const {};",
                    reg.to_usize(),
                    box_fun_thunk_op_to_string(private_funs, box_fun_thunk_op)
                )?;
            }
            ops::OpKind::AllocBoxedFunThunk(reg, box_fun_thunk_op) => {
                writeln!(
                    w,
                    "%{} = alloc {};",
                    reg.to_usize(),
                    box_fun_thunk_op_to_string(private_funs, box_fun_thunk_op)
                )?;
            }
            ops::OpKind::ConstBoxedRecord(reg, box_record_op) => {
                writeln!(
                    w,
                    "%{} = const {};",
                    reg.to_usize(),
                    box_record_op_to_string(box_record_op)
                )?;
            }
            ops::OpKind::AllocBoxedRecord(reg, box_record_op) => {
                writeln!(
                    w,
                    "%{} = alloc {};",
                    reg.to_usize(),
                    box_record_op_to_string(box_record_op)
                )?;
            }
            ops::OpKind::LoadBoxedRecordField(
                reg,
                ops::LoadBoxedRecordFieldOp {
                    record_reg,
                    record_struct,
                    field_index,
                },
            ) => {
                writeln!(
                    w,
                    "%{} = <%{} as record::{}>.{}: {};",
                    reg.to_usize(),
                    record_reg.to_usize(),
                    record_struct.source_name,
                    field_index,
                    record_struct.field_abi_types[*field_index].to_rust_str()
                )?;
            }
            ops::OpKind::LoadBoxedPairHead(reg, pair_reg) => {
                writeln!(
                    w,
                    "%{} = <%{} as boxed::Pair>.head();",
                    reg.to_usize(),
                    pair_reg.to_usize()
                )?;
            }
            ops::OpKind::LoadBoxedPairRest(reg, pair_reg) => {
                writeln!(
                    w,
                    "%{} = <%{} as boxed::Pair>.rest();",
                    reg.to_usize(),
                    pair_reg.to_usize()
                )?;
            }
            ops::OpKind::Call(
                reg,
                ops::CallOp {
                    callee,
                    impure,
                    args,
                },
            ) => {
                let purity = if *impure { "impure" } else { "pure" };

                let callee_abi = callee_to_gen_abi(private_funs, callee);

                let args = Some("%task".to_owned())
                    .into_iter()
                    .filter(|_| callee_abi.takes_task)
                    .chain(callee_abi.params.iter().zip(args.iter()).map(
                        |(param_abi_type, arg_reg)| {
                            format!("%{}: {}", arg_reg.to_usize(), param_abi_type.to_rust_str())
                        },
                    ))
                    .collect::<Vec<String>>()
                    .join(", ");

                writeln!(
                    w,
                    "%{} = {} {}({}): {};",
                    reg.to_usize(),
                    purity,
                    callee_to_string(private_funs, callee),
                    args,
                    callee_abi.ret.to_rust_str()
                )?;
            }
            ops::OpKind::TailCall(reg, ops::TailCallOp { impure, args }) => {
                let purity = if *impure { "impure" } else { "pure" };

                let args = iter::once("%task".to_owned())
                    .chain(
                        args.iter()
                            .map(|arg_reg| format!(", %{}", arg_reg.to_usize())),
                    )
                    .collect::<String>();

                writeln!(w, "%{} = {} recur({});", reg.to_usize(), purity, args,)?;
            }
            ops::OpKind::Ret(reg) => {
                writeln!(w, "return %{};", reg.to_usize())?;
            }
            ops::OpKind::RetVoid => {
                writeln!(w, "return;")?;
            }
            _ => writeln!(w, "{:?}", op.kind)?,
        }
    }

    Ok(())
}

/// Prints a textual representation of a function's MIR to to `w`
pub fn print_fun(
    w: &mut dyn Write,
    private_funs: &HashMap<ops::PrivateFunId, ops::Fun>,
    ops_fun: &ops::Fun,
    private_fun_id: Option<ops::PrivateFunId>,
) -> Result<()> {
    let fun_name = ops_fun
        .source_name
        .clone()
        .map(|s| s.to_string())
        .or_else(|| private_fun_id.map(|pfi| private_fun_to_string(private_funs, pfi)))
        .unwrap_or_else(|| "[anonymous]".into());

    let call_conv_name = match ops_fun.abi.call_conv {
        ops::CallConv::CCC => "extern \"C\" ",
        ops::CallConv::FastCC => "",
    };

    let params = ops_fun
        .abi
        .params
        .iter()
        .zip(ops_fun.param_regs.iter())
        .map(|(abi_type, param_reg)| {
            format!(", %{}: {}", param_reg.to_usize(), abi_type.to_rust_str())
        })
        .collect::<String>();

    writeln!(
        w,
        "{}fn {}(%task{}) -> {} {{",
        call_conv_name,
        fun_name,
        params,
        ops_fun.abi.ret.to_rust_str()
    )?;

    print_branch(w, private_funs, 1, &ops_fun.ops)?;
    writeln!(w, "}}")?;

    Ok(())
}

/// Prints a textual representation of a program's MIR to to `w`
///
/// This is an internal, undocumented and unstable format that has no equivalent parser. It's
/// intended to aid human debugging an optimisation.
pub fn print_program(
    w: &mut dyn Write,
    program: &BuiltProgram,
    source_loader: Option<&SourceLoader>,
) -> Result<()> {
    for (private_fun_id, private_fun) in &program.private_funs {
        if private_fun.source_name.is_none() {
            if let Some(human_location) = span_to_human_location(source_loader, private_fun.span) {
                writeln!(w, "// Anonymous function defined at {}", human_location)?;
            }
        }

        print_fun(w, &program.private_funs, private_fun, Some(*private_fun_id))?;
        writeln!(w)?;
    }

    print_fun(w, &program.private_funs, &program.main, None)
}
