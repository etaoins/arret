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

fn comparison_to_str(comparison: ops::Comparison) -> &'static str {
    match comparison {
        ops::Comparison::Lt => "<",
        ops::Comparison::Le => "<=",
        ops::Comparison::Eq => "==",
        ops::Comparison::Gt => ">",
        ops::Comparison::Ge => ">=",
    }
}

fn print_cond_branch(
    w: &mut dyn Write,
    private_funs: &HashMap<ops::PrivateFunId, ops::Fun>,
    ident_level: usize,
    ops: &[ops::Op],
    result_reg: Option<ops::RegId>,
) -> Result<()> {
    let ident_level = ident_level + 1;
    print_branch(w, private_funs, ident_level, ops)?;

    if let Some(result_reg) = result_reg {
        for _ in 0..ident_level {
            write!(w, "  ")?;
        }
        writeln!(w, "%{}", result_reg.to_usize())?;
    }

    Ok(())
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
                writeln!(w, "%{} = const {}i64;", reg.to_usize(), value)?
            }
            ops::OpKind::ConstFloat(reg, value) => {
                writeln!(w, "%{} = const {}f64;", reg.to_usize(), value)?
            }
            ops::OpKind::ConstTypeTag(reg, type_tag) => {
                writeln!(w, "%{} = const TypeTag::{:?};", reg.to_usize(), type_tag)?
            }
            ops::OpKind::ConstBool(reg, value) => {
                writeln!(w, "%{} = const {};", reg.to_usize(), value)?
            }
            ops::OpKind::ConstBoxedTrue(reg, ()) => {
                writeln!(w, "%{} = const boxed::TRUE_INSTANCE;", reg.to_usize())?
            }
            ops::OpKind::ConstBoxedFalse(reg, ()) => {
                writeln!(w, "%{} = const boxed::FALSE_INSTANCE;", reg.to_usize())?
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
            ops::OpKind::Alias(reg, from_reg) => {
                writeln!(w, "%{} = %{};", reg.to_usize(), from_reg.to_usize(),)?
            }
            ops::OpKind::Int64ToFloat(reg, from_reg) => writeln!(
                w,
                "%{} = (%{}: i64) as f64;",
                reg.to_usize(),
                from_reg.to_usize(),
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
            ops::OpKind::ConstBoxedInt(reg, value) => {
                writeln!(
                    w,
                    "%{} = const boxed::Int {{ value: {}i64 }};",
                    reg.to_usize(),
                    value
                )?;
            }
            ops::OpKind::AllocBoxedInt(reg, value_reg) => {
                writeln!(
                    w,
                    "%{} = alloc boxed::Int {{ value: %{}i64 }};",
                    reg.to_usize(),
                    value_reg.to_usize()
                )?;
            }
            ops::OpKind::ConstBoxedFloat(reg, value) => {
                writeln!(
                    w,
                    "%{} = const boxed::Float {{ value: {}f64 }};",
                    reg.to_usize(),
                    value
                )?;
            }
            ops::OpKind::AllocBoxedFloat(reg, value_reg) => {
                writeln!(
                    w,
                    "%{} = alloc boxed::Float {{ value: %{}f64 }};",
                    reg.to_usize(),
                    value_reg.to_usize()
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
            ops::OpKind::MakeCallback(
                reg,
                ops::MakeCallbackOp {
                    closure_reg,
                    callee,
                },
            ) => {
                writeln!(
                    w,
                    "%{} = callback::Callback {{ closure: %{}, entry_point: {} }};",
                    reg.to_usize(),
                    closure_reg.to_usize(),
                    callee_to_string(private_funs, callee)
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
            ops::OpKind::ConstBoxedSym(reg, name) => {
                writeln!(
                    w,
                    "%{} = const boxed::Sym {{ name: {:?} }};",
                    reg.to_usize(),
                    name
                )?;
            }
            ops::OpKind::ConstBoxedStr(reg, name) => {
                writeln!(
                    w,
                    "%{} = const boxed::Str {{ value: {:?} }};",
                    reg.to_usize(),
                    name
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
            ops::OpKind::LoadBoxedTypeTag(
                reg,
                ops::LoadBoxedTypeTagOp {
                    subject_reg,
                    possible_type_tags,
                },
            ) => {
                let type_tags_string = possible_type_tags
                    .into_iter()
                    .map(|type_tag| format!("{:?}", type_tag))
                    .collect::<Vec<String>>()
                    .join(", ");

                writeln!(
                    w,
                    "%{} = <%{} as boxed::Any>.type_tag in [{}];",
                    reg.to_usize(),
                    subject_reg.to_usize(),
                    type_tags_string
                )?;
            }
            ops::OpKind::LoadBoxedPairHead(reg, pair_reg) => {
                writeln!(
                    w,
                    "%{} = <%{} as boxed::Pair>.head;",
                    reg.to_usize(),
                    pair_reg.to_usize()
                )?;
            }
            ops::OpKind::LoadBoxedPairRest(reg, pair_reg) => {
                writeln!(
                    w,
                    "%{} = <%{} as boxed::Pair>.rest;",
                    reg.to_usize(),
                    pair_reg.to_usize()
                )?;
            }
            ops::OpKind::LoadBoxedSymInterned(reg, sym_reg) => {
                writeln!(
                    w,
                    "%{} = <%{} as boxed::Sym>.interned;",
                    reg.to_usize(),
                    sym_reg.to_usize()
                )?;
            }
            ops::OpKind::LoadBoxedIntValue(reg, int_reg) => {
                writeln!(
                    w,
                    "%{} = <%{} as boxed::Int>.value;",
                    reg.to_usize(),
                    int_reg.to_usize()
                )?;
            }
            ops::OpKind::LoadBoxedFloatValue(reg, float_reg) => {
                writeln!(
                    w,
                    "%{} = <%{} as boxed::Float>.value;",
                    reg.to_usize(),
                    float_reg.to_usize()
                )?;
            }
            ops::OpKind::LoadBoxedFunThunkClosure(reg, fun_thunk_reg) => {
                writeln!(
                    w,
                    "%{} = <%{} as boxed::FunThunk>.closure;",
                    reg.to_usize(),
                    fun_thunk_reg.to_usize()
                )?;
            }
            ops::OpKind::IntCompare(
                reg,
                ops::CompareOp {
                    lhs_reg,
                    rhs_reg,
                    comparison,
                },
            ) => {
                writeln!(
                    w,
                    "%{} = (%{}: i64) {} (%{}: i64);",
                    reg.to_usize(),
                    lhs_reg.to_usize(),
                    comparison_to_str(*comparison),
                    rhs_reg.to_usize(),
                )?;
            }
            ops::OpKind::FloatCompare(
                reg,
                ops::CompareOp {
                    lhs_reg,
                    rhs_reg,
                    comparison,
                },
            ) => {
                writeln!(
                    w,
                    "%{} = (%{}: f64) {} (%{}: f64);",
                    reg.to_usize(),
                    lhs_reg.to_usize(),
                    comparison_to_str(*comparison),
                    rhs_reg.to_usize(),
                )?;
            }
            ops::OpKind::FloatAdd(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = (%{}: f64) + (%{}: f64);",
                    reg.to_usize(),
                    lhs_reg.to_usize(),
                    rhs_reg.to_usize(),
                )?;
            }
            ops::OpKind::FloatSub(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = (%{}: f64) - (%{}: f64);",
                    reg.to_usize(),
                    lhs_reg.to_usize(),
                    rhs_reg.to_usize(),
                )?;
            }
            ops::OpKind::FloatMul(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = (%{}: f64) * (%{}: f64);",
                    reg.to_usize(),
                    lhs_reg.to_usize(),
                    rhs_reg.to_usize(),
                )?;
            }
            ops::OpKind::FloatDiv(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = (%{}: f64) / (%{}: f64);",
                    reg.to_usize(),
                    lhs_reg.to_usize(),
                    rhs_reg.to_usize(),
                )?;
            }
            ops::OpKind::Int64CheckedAdd(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = checked (%{}: i64) + (%{}: i64);",
                    reg.to_usize(),
                    lhs_reg.to_usize(),
                    rhs_reg.to_usize(),
                )?;
            }
            ops::OpKind::Int64CheckedSub(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = checked (%{}: i64) - (%{}: i64);",
                    reg.to_usize(),
                    lhs_reg.to_usize(),
                    rhs_reg.to_usize(),
                )?;
            }
            ops::OpKind::Int64CheckedMul(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = checked (%{}: i64) * (%{}: i64);",
                    reg.to_usize(),
                    lhs_reg.to_usize(),
                    rhs_reg.to_usize(),
                )?;
            }
            ops::OpKind::Int64CheckedDiv(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = checked (%{}: i64) / (%{}: i64);",
                    reg.to_usize(),
                    lhs_reg.to_usize(),
                    rhs_reg.to_usize(),
                )?;
            }
            ops::OpKind::Int64CheckedRem(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = checked (%{}: i64) % (%{}: i64);",
                    reg.to_usize(),
                    lhs_reg.to_usize(),
                    rhs_reg.to_usize(),
                )?;
            }
            ops::OpKind::TypeTagEqual(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = (%{}: TypeTag) == (%{}: TypeTag);",
                    reg.to_usize(),
                    lhs_reg.to_usize(),
                    rhs_reg.to_usize(),
                )?;
            }
            ops::OpKind::BoolEqual(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = (%{}: bool) == (%{}: bool);",
                    reg.to_usize(),
                    lhs_reg.to_usize(),
                    rhs_reg.to_usize(),
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
            ops::OpKind::Cond(cond_op) => {
                if let Some(reg_phi) = &cond_op.reg_phi {
                    write!(w, "%{} = ", reg_phi.output_reg.to_usize())?;
                }
                writeln!(w, "if %{} {{", cond_op.test_reg.to_usize())?;

                print_cond_branch(
                    w,
                    private_funs,
                    ident_level,
                    &cond_op.true_ops,
                    cond_op.reg_phi.as_ref().map(|rp| rp.true_result_reg),
                )?;

                if !cond_op.false_ops.is_empty() {
                    for _ in 0..ident_level {
                        write!(w, "  ")?;
                    }
                    writeln!(w, "}} else {{")?;

                    print_cond_branch(
                        w,
                        private_funs,
                        ident_level,
                        &cond_op.false_ops,
                        cond_op.reg_phi.as_ref().map(|rp| rp.false_result_reg),
                    )?;
                }

                for _ in 0..ident_level {
                    write!(w, "  ")?;
                }
                if cond_op.reg_phi.is_some() {
                    writeln!(w, "}};")?;
                } else {
                    writeln!(w, "}}")?;
                }
            }
            ops::OpKind::Ret(reg) => {
                writeln!(w, "return %{};", reg.to_usize())?;
            }
            ops::OpKind::RetVoid => {
                writeln!(w, "return;")?;
            }
            ops::OpKind::Unreachable => {
                writeln!(w, "unreachable;")?;
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
