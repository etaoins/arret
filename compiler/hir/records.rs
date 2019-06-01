use arret_syntax::span::Span;

use crate::hir::error::{Error, ErrorKind, Result};
use crate::hir::ns::Ident;
use crate::hir::ns::{NsDataIter, NsDatum};
use crate::hir::scope::{Binding, Scope};
use crate::hir::types::lower_poly;
use crate::hir::types::lower_polymorphic_var_list;
use crate::hir::util::{expect_ident, expect_spanned_ident};
use crate::ty::record;
use crate::ty::Ty;

pub enum LoweredRecordCons {
    Parameterised(Span, Ident, NsDataIter),
    Singleton(Span, Ident),
}

fn lower_record_field_decl(scope: &Scope<'_>, field_datum: NsDatum) -> Result<record::Field> {
    let datum_span = field_datum.span();
    let datum_description = field_datum.description();

    let (ident, poly) = match field_datum {
        NsDatum::Ident(_, ident) => (ident, Ty::Any.into()),
        NsDatum::Vector(span, vs) => {
            let mut data = vs.into_vec();

            if data.len() != 2 {
                return Err(Error::new(
                    span,
                    ErrorKind::ExpectedRecordFieldDecl(datum_description),
                ));
            }

            let poly = lower_poly(scope, data.pop().unwrap())?;
            let ident = expect_ident(data.pop().unwrap(), "new record field name")?;

            (ident, poly)
        }
        other => {
            return Err(Error::new(
                other.span(),
                ErrorKind::ExpectedRecordFieldDecl(datum_description),
            ));
        }
    };

    Ok(record::Field::new(datum_span, ident.into_name(), poly))
}

/// Lowers either the type or value constructor for a `(defrecord)`
fn lower_record_cons_decl<F>(cons_datum: NsDatum, error_kind_cons: F) -> Result<LoweredRecordCons>
where
    F: Fn(&'static str) -> ErrorKind,
{
    let datum_description = cons_datum.description();

    match cons_datum {
        NsDatum::Ident(span, ident) => Ok(LoweredRecordCons::Singleton(span, ident)),
        NsDatum::List(span, vs) => {
            let mut param_data_iter = vs.into_vec().into_iter();

            if let Some(name_datum) = param_data_iter.next() {
                let (ident_span, ident) =
                    expect_spanned_ident(name_datum, "new record constructor name")?;

                Ok(LoweredRecordCons::Parameterised(
                    ident_span,
                    ident,
                    param_data_iter,
                ))
            } else {
                Err(Error::new(span, error_kind_cons(datum_description)))
            }
        }
        other => Err(Error::new(other.span(), error_kind_cons(datum_description))),
    }
}

pub fn lower_record(
    outer_scope: &mut Scope<'_>,
    ty_cons_datum: NsDatum,
    value_cons_datum: NsDatum,
) -> Result<()> {
    use crate::hir::types::PolymorphicVar;
    use crate::ty::ty_args::TyArgs;

    let mut inner_scope = Scope::new_child(outer_scope);

    // Lower our type constructor
    let ty_cons_span = ty_cons_datum.span();
    let ty_cons_decl = lower_record_cons_decl(ty_cons_datum, ErrorKind::ExpectedRecordTyConsDecl)?;

    let (ty_ident_span, ty_ident, poly_vars) = match ty_cons_decl {
        LoweredRecordCons::Singleton(span, ident) => (span, ident, None),
        LoweredRecordCons::Parameterised(span, ident, param_data_iter) => {
            let poly_params =
                lower_polymorphic_var_list(outer_scope, &mut inner_scope, param_data_iter)?;

            (span, ident, Some(poly_params))
        }
    };

    // Lower our value destructor
    let value_cons_decl =
        lower_record_cons_decl(value_cons_datum, ErrorKind::ExpectedRecordValueConsDecl)?;

    let fields: Box<[record::Field]>;
    let (value_cons_ident_span, value_cons_ident) = match value_cons_decl {
        LoweredRecordCons::Singleton(_, _) => {
            unimplemented!("singleton record values");
        }
        LoweredRecordCons::Parameterised(span, ident, param_data_iter) => {
            fields = param_data_iter
                .map(|field_datum| lower_record_field_decl(&inner_scope, field_datum))
                .collect::<Result<Box<_>>>()?;

            (span, ident)
        }
    };

    // Convert our lowered polymorphic vars to polymorphic parameters
    let poly_params_list = match poly_vars {
        Some(poly_vars) => {
            use crate::ty::var_usage::VarUsages;

            let mut var_usages = VarUsages::new();
            for field in fields.iter() {
                var_usages.add_poly_usages(field.ty_ref());
            }

            let poly_params_list = poly_vars
                .into_vec()
                .into_iter()
                .map(|poly_var| {
                    match poly_var {
                        PolymorphicVar::PVar(pvar) => {
                            if let Some(variance) = var_usages.pvar_variance(&pvar) {
                                Ok(record::PolyParam::PVar(variance, pvar))
                            } else {
                                Err(Error::new(
                                    pvar.span(),
                                    ErrorKind::UnusedPolyPurityParam(pvar),
                                ))
                            }
                        }
                        // It'd be nice to check if the param was used but it's been erased to
                        // `Pure` by this point
                        PolymorphicVar::Pure(span) => Ok(record::PolyParam::Pure(span)),
                        PolymorphicVar::TVar(tvar) => {
                            if let Some(variance) = var_usages.tvar_variance(&tvar) {
                                Ok(record::PolyParam::TVar(variance, tvar))
                            } else {
                                Err(Error::new(tvar.span(), ErrorKind::UnusedPolyTyParam(tvar)))
                            }
                        }
                        PolymorphicVar::TFixed(span, fixed_poly) => {
                            Ok(record::PolyParam::TFixed(span, fixed_poly))
                        }
                    }
                })
                .collect::<Result<Box<[record::PolyParam]>>>()?;

            Some(poly_params_list)
        }
        None => None,
    };

    let record_ty_cons = record::Cons::new(
        ty_cons_span,
        ty_ident.name().clone(),
        poly_params_list,
        fields,
    );

    for (idx, field) in record_ty_cons.fields().iter().enumerate() {
        if field.name().as_ref() != "_" {
            let accessor_name = format!("{}-{}", value_cons_ident.name(), field.name());
            let accessor_ident = Ident::new(value_cons_ident.ns_id(), accessor_name.into());

            outer_scope.insert_binding(
                field.span(),
                accessor_ident,
                Binding::FieldAccessor(record_ty_cons.clone(), idx),
            )?;
        }
    }

    outer_scope.insert_binding(
        value_cons_ident_span,
        value_cons_ident,
        Binding::RecordValueCons(record_ty_cons.clone()),
    )?;

    if record_ty_cons.is_singleton() {
        // We were used as a singleton; bind a type
        let record_instance = record::Instance::new(record_ty_cons.clone(), TyArgs::empty());
        outer_scope.insert_binding(ty_ident_span, ty_ident, Binding::Ty(record_instance.into()))?;
    } else {
        // We were used as a type constructor; bind a type constructor
        outer_scope.insert_binding(
            ty_ident_span,
            ty_ident,
            Binding::RecordTyCons(record_ty_cons),
        )?;
    };

    Ok(())
}
