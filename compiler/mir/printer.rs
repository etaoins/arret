use std::collections::HashMap;
use std::io::{Result, Write};
use std::iter;

use codespan_reporting::files::Files as _;

use arret_syntax::span::Span;

use crate::codegen::GenAbi;
use crate::mir::ops;
use crate::mir::BuiltProgram;
use crate::source::SourceLoader;
use crate::ty::conv_abi::ConvertableAbiType;

fn span_to_human_location(source_loader: Option<&SourceLoader>, span: Span) -> Option<String> {
    let source_loader = source_loader?;
    let file_id = span.file_id()?;

    let files = source_loader.files();
    let location = files.location(file_id, span.start() as usize).ok()?;

    Some(format!(
        "{}:{}:{}",
        files.name(file_id).ok()?,
        location.line_number,
        location.column_number
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
        ops::Callee::BoxedFunThunk(thunk_reg) => {
            format!("<%{} as boxed::FunThunk>.entry", thunk_reg.get())
        }
    }
}
fn callee_to_gen_abi(
    private_funs: &HashMap<ops::PrivateFunId, ops::Fun>,
    callee: &ops::Callee,
) -> GenAbi {
    match callee {
        ops::Callee::StaticSymbol(static_symbol) => static_symbol.abi.clone(),
        ops::Callee::PrivateFun(private_fun_id) => (&private_funs[private_fun_id].abi).into(),
        ops::Callee::BoxedFunThunk(_) => GenAbi::thunk_abi(),
    }
}

fn box_pair_op_to_string(
    ops::BoxPairOp {
        head_reg,
        rest_reg,
        list_len_reg,
    }: &ops::BoxPairOp,
) -> String {
    format!(
        "boxed::Pair {{ head: %{}, rest: %{}, list_len: %{} }}",
        head_reg.get(),
        rest_reg.get(),
        list_len_reg.get()
    )
}

fn box_fun_thunk_op_to_string(
    private_funs: &HashMap<ops::PrivateFunId, ops::Fun>,
    ops::BoxFunThunkOp {
        captures_reg,
        callee,
    }: &ops::BoxFunThunkOp,
) -> String {
    format!(
        "boxed::FunThunk {{ captures: %{captures_reg}, entry: {entry} }}",
        captures_reg = captures_reg.get(),
        entry = callee_to_string(private_funs, callee)
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
            format!("%{}: {}", field_reg.get(), field_abi_type.to_rust_str())
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
        writeln!(w, "%{}", result_reg.get())?;
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
                writeln!(w, "%{} = const boxed::NIL_INSTANCE;", reg.get())?;
            }
            ops::OpKind::ConstInt64(reg, value) => {
                writeln!(w, "%{} = const {}i64;", reg.get(), value)?
            }
            ops::OpKind::ConstChar(reg, value) => {
                writeln!(w, "%{} = const {:?}", reg.get(), value)?
            }
            ops::OpKind::ConstFloat(reg, value) => {
                writeln!(w, "%{} = const {}f64;", reg.get(), value)?
            }
            ops::OpKind::ConstInternedSym(reg, name) => {
                writeln!(
                    w,
                    "%{} = const interned::InternedSym {{ name: {:?} }};",
                    reg.get(),
                    name
                )?;
            }
            ops::OpKind::ConstTypeTag(reg, type_tag) => {
                writeln!(w, "%{} = const TypeTag::{:?};", reg.get(), type_tag)?
            }
            ops::OpKind::ConstBool(reg, value) => writeln!(w, "%{} = const {};", reg.get(), value)?,
            ops::OpKind::ConstBoxedTrue(reg, ()) => {
                writeln!(w, "%{} = const boxed::TRUE_INSTANCE;", reg.get())?
            }
            ops::OpKind::ConstBoxedFalse(reg, ()) => {
                writeln!(w, "%{} = const boxed::FALSE_INSTANCE;", reg.get())?
            }
            ops::OpKind::ConstBoxedVector(reg, element_regs) => writeln!(
                w,
                "%{} = const boxed::Vector {{ elements: [{}] }};",
                reg.get(),
                element_regs
                    .iter()
                    .map(|element_reg| format!("%{}", element_reg.get()))
                    .collect::<Vec<String>>()
                    .join(", ")
            )?,
            ops::OpKind::ConstBoxedSet(reg, element_regs) => writeln!(
                w,
                "%{} = const boxed::Set {{ elements: [{}] }};",
                reg.get(),
                element_regs
                    .iter()
                    .map(|element_reg| format!("%{}", element_reg.get()))
                    .collect::<Vec<String>>()
                    .join(", ")
            )?,
            ops::OpKind::ConstBoxedMap(reg, entry_regs) => writeln!(
                w,
                "%{} = const boxed::Map {{ elements: [{}] }};",
                reg.get(),
                entry_regs
                    .iter()
                    .map(|(key_reg, value_reg)| format!(
                        "(%{}, %{})",
                        key_reg.get(),
                        value_reg.get()
                    ))
                    .collect::<Vec<String>>()
                    .join(", ")
            )?,
            ops::OpKind::ConstRecordClassId(reg, record_class_id) => writeln!(
                w,
                "%{} = const record::{}::CLASS_ID;",
                reg.get(),
                record_class_id.source_name
            )?,
            ops::OpKind::CastBoxed(reg, ops::CastBoxedOp { from_reg, to_type }) => writeln!(
                w,
                "%{} = %{} as {};",
                reg.get(),
                from_reg.get(),
                to_type.to_rust_str()
            )?,
            ops::OpKind::ConstCastBoxed(reg, ops::CastBoxedOp { from_reg, to_type }) => writeln!(
                w,
                "%{} = const %{} as {};",
                reg.get(),
                from_reg.get(),
                to_type.to_rust_str()
            )?,
            ops::OpKind::Alias(reg, from_reg) => {
                writeln!(w, "%{} = %{};", reg.get(), from_reg.get(),)?
            }
            ops::OpKind::Int64ToFloat(reg, from_reg) => {
                writeln!(w, "%{} = (%{}: i64) as f64;", reg.get(), from_reg.get(),)?
            }
            ops::OpKind::ConstBoxedPair(reg, box_pair_op) => {
                writeln!(
                    w,
                    "%{} = const {};",
                    reg.get(),
                    box_pair_op_to_string(box_pair_op)
                )?;
            }
            ops::OpKind::AllocBoxedPair(reg, box_pair_op) => {
                writeln!(
                    w,
                    "%{} = alloc {};",
                    reg.get(),
                    box_pair_op_to_string(box_pair_op)
                )?;
            }
            ops::OpKind::ConstBoxedInt(reg, value) => {
                writeln!(
                    w,
                    "%{} = const boxed::Int {{ value: {}i64 }};",
                    reg.get(),
                    value
                )?;
            }
            ops::OpKind::AllocBoxedInt(reg, value_reg) => {
                writeln!(
                    w,
                    "%{} = alloc boxed::Int {{ value: %{} }};",
                    reg.get(),
                    value_reg.get()
                )?;
            }
            ops::OpKind::ConstBoxedChar(reg, value) => {
                writeln!(
                    w,
                    "%{} = const boxed::Char {{ value: {:?} }};",
                    reg.get(),
                    value
                )?;
            }
            ops::OpKind::AllocBoxedChar(reg, value_reg) => {
                writeln!(
                    w,
                    "%{} = alloc boxed::Char {{ value: %{} }};",
                    reg.get(),
                    value_reg.get()
                )?;
            }
            ops::OpKind::ConstBoxedFloat(reg, value) => {
                writeln!(
                    w,
                    "%{} = const boxed::Float {{ value: {}f64 }};",
                    reg.get(),
                    value
                )?;
            }
            ops::OpKind::AllocBoxedFloat(reg, value_reg) => {
                writeln!(
                    w,
                    "%{} = alloc boxed::Float {{ value: %{} }};",
                    reg.get(),
                    value_reg.get()
                )?;
            }
            ops::OpKind::ConstBoxedFunThunk(reg, box_fun_thunk_op) => {
                writeln!(
                    w,
                    "%{} = const {};",
                    reg.get(),
                    box_fun_thunk_op_to_string(private_funs, box_fun_thunk_op)
                )?;
            }
            ops::OpKind::AllocBoxedFunThunk(reg, box_fun_thunk_op) => {
                writeln!(
                    w,
                    "%{} = alloc {};",
                    reg.get(),
                    box_fun_thunk_op_to_string(private_funs, box_fun_thunk_op)
                )?;
            }
            ops::OpKind::MakeCallback(
                reg,
                ops::MakeCallbackOp {
                    captures_reg,
                    callee,
                },
            ) => {
                writeln!(
                    w,
                    "%{} = callback::Callback {{ captures: %{captures_reg}, entry_point: {entry_point} }};",
                    reg.get(),
                    captures_reg = captures_reg.get(),
                    entry_point = callee_to_string(private_funs, callee)
                )?;
            }
            ops::OpKind::ConstBoxedRecord(reg, box_record_op) => {
                writeln!(
                    w,
                    "%{} = const {};",
                    reg.get(),
                    box_record_op_to_string(box_record_op)
                )?;
            }
            ops::OpKind::AllocBoxedRecord(reg, box_record_op) => {
                writeln!(
                    w,
                    "%{} = alloc {};",
                    reg.get(),
                    box_record_op_to_string(box_record_op)
                )?;
            }
            ops::OpKind::ConstBoxedSym(reg, name) => {
                writeln!(
                    w,
                    "%{} = const boxed::Sym {{ name: {:?} }};",
                    reg.get(),
                    name
                )?;
            }
            ops::OpKind::AllocBoxedSym(reg, interned_sym_reg) => {
                writeln!(
                    w,
                    "%{} = alloc boxed::Sym {{ interned: %{} }};",
                    reg.get(),
                    interned_sym_reg.get()
                )?;
            }
            ops::OpKind::ConstBoxedStr(reg, name) => {
                writeln!(
                    w,
                    "%{} = const boxed::Str {{ value: {:?} }};",
                    reg.get(),
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
                    reg.get(),
                    record_reg.get(),
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
                    reg.get(),
                    subject_reg.get(),
                    type_tags_string
                )?;
            }
            ops::OpKind::LoadBoxedPairHead(reg, pair_reg) => {
                writeln!(
                    w,
                    "%{} = <%{} as boxed::Pair>.head;",
                    reg.get(),
                    pair_reg.get()
                )?;
            }
            ops::OpKind::LoadBoxedPairRest(reg, pair_reg) => {
                writeln!(
                    w,
                    "%{} = <%{} as boxed::Pair>.rest;",
                    reg.get(),
                    pair_reg.get()
                )?;
            }
            ops::OpKind::LoadBoxedVectorMember(
                reg,
                ops::LoadBoxedVectorMemberOp {
                    vector_reg,
                    known_vector_len,
                    member_index,
                },
            ) => {
                writeln!(
                    w,
                    "%{reg} = <%{vector_reg} as boxed::Vector>[{member_index}] where <${vector_reg} as boxed::Vector>.len == {known_vector_len};",
                    reg = reg.get(),
                    vector_reg = vector_reg.get(),
                    known_vector_len = known_vector_len,
                    member_index = member_index
                )?;
            }
            ops::OpKind::LoadBoxedListLen(
                reg,
                ops::LoadBoxedListLenOp {
                    list_reg,
                    min_list_len,
                },
            ) => {
                writeln!(
                    w,
                    "%{} = <%{} as boxed::List>.list_len where > {};",
                    reg.get(),
                    list_reg.get(),
                    min_list_len
                )?;
            }
            ops::OpKind::LoadBoxedVectorLen(reg, list_reg) => {
                writeln!(
                    w,
                    "%{} = <%{} as boxed::Vector>.len;",
                    reg.get(),
                    list_reg.get(),
                )?;
            }
            ops::OpKind::LoadBoxedSymInterned(reg, sym_reg) => {
                writeln!(
                    w,
                    "%{} = <%{} as boxed::Sym>.interned;",
                    reg.get(),
                    sym_reg.get()
                )?;
            }
            ops::OpKind::LoadBoxedIntValue(reg, int_reg) => {
                writeln!(
                    w,
                    "%{} = <%{} as boxed::Int>.value;",
                    reg.get(),
                    int_reg.get()
                )?;
            }
            ops::OpKind::LoadBoxedFloatValue(reg, float_reg) => {
                writeln!(
                    w,
                    "%{} = <%{} as boxed::Float>.value;",
                    reg.get(),
                    float_reg.get()
                )?;
            }
            ops::OpKind::LoadBoxedCharValue(reg, float_reg) => {
                writeln!(
                    w,
                    "%{} = <%{} as boxed::Char>.value;",
                    reg.get(),
                    float_reg.get()
                )?;
            }
            ops::OpKind::LoadBoxedFunThunkCaptures(reg, fun_thunk_reg) => {
                writeln!(
                    w,
                    "%{} = <%{} as boxed::FunThunk>.env;",
                    reg.get(),
                    fun_thunk_reg.get()
                )?;
            }
            ops::OpKind::LoadBoxedRecordClassId(reg, record_reg) => {
                writeln!(
                    w,
                    "%{} = <%{} as boxed::Record>.class_id;",
                    reg.get(),
                    record_reg.get()
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
                    reg.get(),
                    lhs_reg.get(),
                    comparison_to_str(*comparison),
                    rhs_reg.get(),
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
                    reg.get(),
                    lhs_reg.get(),
                    comparison_to_str(*comparison),
                    rhs_reg.get(),
                )?;
            }
            ops::OpKind::FloatAdd(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = (%{}: f64) + (%{}: f64);",
                    reg.get(),
                    lhs_reg.get(),
                    rhs_reg.get(),
                )?;
            }
            ops::OpKind::FloatSub(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = (%{}: f64) - (%{}: f64);",
                    reg.get(),
                    lhs_reg.get(),
                    rhs_reg.get(),
                )?;
            }
            ops::OpKind::FloatMul(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = (%{}: f64) * (%{}: f64);",
                    reg.get(),
                    lhs_reg.get(),
                    rhs_reg.get(),
                )?;
            }
            ops::OpKind::FloatDiv(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = (%{}: f64) / (%{}: f64);",
                    reg.get(),
                    lhs_reg.get(),
                    rhs_reg.get(),
                )?;
            }
            ops::OpKind::Int64Add(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = unchecked (%{}: i64) + (%{}: i64);",
                    reg.get(),
                    lhs_reg.get(),
                    rhs_reg.get(),
                )?;
            }
            ops::OpKind::Int64CheckedAdd(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = checked (%{}: i64) + (%{}: i64);",
                    reg.get(),
                    lhs_reg.get(),
                    rhs_reg.get(),
                )?;
            }
            ops::OpKind::Int64CheckedSub(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = checked (%{}: i64) - (%{}: i64);",
                    reg.get(),
                    lhs_reg.get(),
                    rhs_reg.get(),
                )?;
            }
            ops::OpKind::Int64CheckedMul(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = checked (%{}: i64) * (%{}: i64);",
                    reg.get(),
                    lhs_reg.get(),
                    rhs_reg.get(),
                )?;
            }
            ops::OpKind::Int64Div(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = unchecked (%{}: i64) / (%{}: i64);",
                    reg.get(),
                    lhs_reg.get(),
                    rhs_reg.get(),
                )?;
            }
            ops::OpKind::Int64CheckedDiv(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = checked (%{}: i64) / (%{}: i64);",
                    reg.get(),
                    lhs_reg.get(),
                    rhs_reg.get(),
                )?;
            }
            ops::OpKind::Int64Rem(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = unchecked (%{}: i64) % (%{}: i64);",
                    reg.get(),
                    lhs_reg.get(),
                    rhs_reg.get(),
                )?;
            }
            ops::OpKind::Int64CheckedRem(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = checked (%{}: i64) % (%{}: i64);",
                    reg.get(),
                    lhs_reg.get(),
                    rhs_reg.get(),
                )?;
            }
            ops::OpKind::Int64BitwiseAnd(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = (%{}: u64) & (%{}: u64);",
                    reg.get(),
                    lhs_reg.get(),
                    rhs_reg.get(),
                )?;
            }
            ops::OpKind::Int64BitwiseOr(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = (%{}: u64) | (%{}: u64);",
                    reg.get(),
                    lhs_reg.get(),
                    rhs_reg.get(),
                )?;
            }
            ops::OpKind::Int64BitwiseXor(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = (%{}: u64) ^ (%{}: u64);",
                    reg.get(),
                    lhs_reg.get(),
                    rhs_reg.get(),
                )?;
            }
            ops::OpKind::Int64BitwiseNot(reg, int_reg) => {
                writeln!(w, "%{} = ~(%{}: u64);", reg.get(), int_reg.get())?;
            }
            ops::OpKind::Int64ShiftLeft(reg, ops::ShiftOp { int_reg, bit_count }) => {
                writeln!(
                    w,
                    "%{} = (%{}: u64) << {};",
                    reg.get(),
                    int_reg.get(),
                    bit_count
                )?;
            }
            ops::OpKind::Int64ArithmeticShiftRight(reg, ops::ShiftOp { int_reg, bit_count }) => {
                writeln!(
                    w,
                    "%{} = (%{}: i64) >> {};",
                    reg.get(),
                    int_reg.get(),
                    bit_count
                )?;
            }
            ops::OpKind::Int64LogicalShiftRight(reg, ops::ShiftOp { int_reg, bit_count }) => {
                writeln!(
                    w,
                    "%{} = (%{}: u64) >> {};",
                    reg.get(),
                    int_reg.get(),
                    bit_count
                )?;
            }
            ops::OpKind::FloatSqrt(reg, radicand_reg) => {
                writeln!(w, "%{} = sqrt(%{}: f64);", reg.get(), radicand_reg.get(),)?;
            }
            ops::OpKind::TypeTagEqual(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = (%{}: TypeTag) == (%{}: TypeTag);",
                    reg.get(),
                    lhs_reg.get(),
                    rhs_reg.get(),
                )?;
            }
            ops::OpKind::BoxIdentical(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = &(%{}: boxed::Any) == &(%{}: boxed::Any);",
                    reg.get(),
                    lhs_reg.get(),
                    rhs_reg.get(),
                )?;
            }
            ops::OpKind::BoolEqual(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = (%{}: bool) == (%{}: bool);",
                    reg.get(),
                    lhs_reg.get(),
                    rhs_reg.get(),
                )?;
            }
            ops::OpKind::CharEqual(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = (%{}: char) == (%{}: char);",
                    reg.get(),
                    lhs_reg.get(),
                    rhs_reg.get(),
                )?;
            }
            ops::OpKind::InternedSymEqual(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = (%{}: interned::InternedSym) == (%{}: interned::InternedSym);",
                    reg.get(),
                    lhs_reg.get(),
                    rhs_reg.get(),
                )?;
            }
            ops::OpKind::RecordClassIdEqual(reg, ops::BinaryOp { lhs_reg, rhs_reg }) => {
                writeln!(
                    w,
                    "%{} = (%{}: boxed::RecordClassId) == (%{}: boxed::RecordClassId);",
                    reg.get(),
                    lhs_reg.get(),
                    rhs_reg.get(),
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
                            format!("%{}: {}", arg_reg.get(), param_abi_type.to_rust_str())
                        },
                    ))
                    .collect::<Vec<String>>()
                    .join(", ");

                writeln!(
                    w,
                    "%{} = {} {}({}): {};",
                    reg.get(),
                    purity,
                    callee_to_string(private_funs, callee),
                    args,
                    callee_abi.ret.to_rust_str()
                )?;
            }
            ops::OpKind::TailCall(reg, ops::TailCallOp { impure, args }) => {
                let purity = if *impure { "impure" } else { "pure" };

                let args = iter::once("%task".to_owned())
                    .chain(args.iter().map(|arg_reg| format!(", %{}", arg_reg.get())))
                    .collect::<String>();

                writeln!(w, "%{} = {} recur({});", reg.get(), purity, args,)?;
            }
            ops::OpKind::Cond(cond_op) => {
                if let Some(reg_phi) = &cond_op.reg_phi {
                    write!(w, "%{} = ", reg_phi.output_reg.get())?;
                }
                writeln!(w, "if %{} {{", cond_op.test_reg.get())?;

                print_cond_branch(
                    w,
                    private_funs,
                    ident_level,
                    &cond_op.true_ops,
                    cond_op.reg_phi.as_ref().map(|rp| rp.true_result_reg),
                )?;

                if !(cond_op.false_ops.is_empty() && cond_op.reg_phi.is_none()) {
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
                writeln!(w, "return %{};", reg.get())?;
            }
            ops::OpKind::RetVoid => {
                writeln!(w, "return;")?;
            }
            ops::OpKind::Unreachable => {
                writeln!(w, "unreachable;")?;
            }
            ops::OpKind::Panic(message) => {
                writeln!(w, "panic({:?});", message)?;
            }
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
        ops::CallConv::Ccc => "extern \"C\" ",
        ops::CallConv::FastCc => "",
    };

    let params = ops_fun
        .abi
        .params
        .iter()
        .zip(ops_fun.param_regs.iter())
        .map(|(abi_type, param_reg)| format!(", %{}: {}", param_reg.get(), abi_type.to_rust_str()))
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
