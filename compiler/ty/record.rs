use arret_syntax::datum::DataStr;
use arret_syntax::span::Span;

use crate::id_type::ArcId;
use crate::ty;
use crate::ty::purity;
use crate::ty::purity::Purity;
use crate::ty::ty_args::TyArgs;
use crate::ty::var_usage::Variance;
use crate::ty::Ty;

/// Record field of a record constructor
#[derive(PartialEq, Debug, Clone)]
pub struct Field {
    span: Span,
    name: DataStr,
    ty_ref: ty::Ref<ty::Poly>,
}

impl Field {
    pub fn new(span: Span, name: DataStr, ty_ref: ty::Ref<ty::Poly>) -> Self {
        Self { span, name, ty_ref }
    }

    /// Returns the span where the constructor was defined
    pub fn span(&self) -> Span {
        self.span
    }

    /// Returns the name of the record field
    ///
    /// Unlike other source names that are used for diagnostics, this is semantically meaningful.
    /// It's used for keyword-based field access syntax.
    pub fn name(&self) -> &DataStr {
        &self.name
    }

    /// Returns the type of the record field
    pub fn ty_ref(&self) -> &ty::Ref<ty::Poly> {
        &self.ty_ref
    }

    /// Returns the type of the field accessor function
    pub fn accessor_fun_type(&self, cons_id: &ConsId) -> ty::Fun {
        let ty_args = cons_id.identity_ty_args();
        let pvars: purity::PVars = ty_args.pvar_purities().keys().cloned().collect();
        let tvars: ty::TVars = ty_args.tvar_types().keys().cloned().collect();

        let top_fun = ty::TopFun::new(Purity::Pure.into(), self.ty_ref().clone());
        let params =
            ty::List::new_tuple(Box::new([Instance::new(cons_id.clone(), ty_args).into()]));

        ty::Fun::new(pvars, tvars, top_fun, params)
    }
}

/// Polymorphic parameter to a record constructor
///
/// This doesn't use separate [`TVar`](ty::TVar) and [`PVar`](purity::PVar) vectors because they
/// appear in the same parameter list and their ordering is important.
#[derive(PartialEq, Debug, Clone)]
pub enum PolyParam {
    /// Polymorphic purity variable
    PVar(Variance, purity::PVarId),
    /// Declared polymorphic purity fixed to `Pure`
    Pure(Span),

    /// Polymorphic type variable
    TVar(Variance, ty::TVarId),
    /// Declared polymorphic type fixed to a known type
    TFixed(Span, ty::Ref<ty::Poly>),
}

impl PolyParam {
    pub fn variance(&self) -> Variance {
        match self {
            PolyParam::PVar(variance, _) => *variance,
            PolyParam::TVar(variance, _) => *variance,
            // This is arbitrary as every instance will have the same value. `Invariant` is
            // probably the least confusing thing to return.
            PolyParam::Pure(_) | PolyParam::TFixed(_, _) => Variance::Invariant,
        }
    }
}

/// Record type constructor
///
/// This is a collection of fields and polymorphic parameters that can be used to construct
/// [instance types](Instance). This should not be confused with the record constructor function
/// used to build record values.
#[derive(PartialEq, Debug, Clone)]
pub struct Cons {
    span: Span,
    ty_cons_name: DataStr,
    value_cons_name: DataStr,
    poly_params_list: Option<Box<[PolyParam]>>,
    fields: Box<[Field]>,
}

impl Cons {
    pub fn new(
        span: Span,
        ty_cons_name: DataStr,
        value_cons_name: DataStr,
        poly_params_list: Option<Box<[PolyParam]>>,
        fields: Box<[Field]>,
    ) -> ConsId {
        ConsId::new(Self {
            span,
            ty_cons_name,
            value_cons_name,
            poly_params_list,
            fields,
        })
    }

    /// Returns the span where the constructor was defined
    pub fn span(&self) -> Span {
        self.span
    }

    /// Returns the name of the type constructor
    ///
    /// Unlike other source names that are used for diagnostics, this is semantically meaningful.
    /// It's used to define a type constructor.
    pub fn ty_cons_name(&self) -> &DataStr {
        &self.ty_cons_name
    }

    /// Returns the name of the value constructor
    pub fn value_cons_name(&self) -> &DataStr {
        &self.value_cons_name
    }

    /// Returns the polymorphic parameters this constructor accepts
    pub fn poly_params(&self) -> &[PolyParam] {
        match self.poly_params_list {
            Some(ref poly_params) => poly_params.as_ref(),
            None => &[],
        }
    }

    /// Returns true if the constructor was declared as a singleton
    ///
    /// This has no effect on the type system; it's only used to accurately print the type.
    pub fn is_singleton(&self) -> bool {
        self.poly_params_list.is_none()
    }

    /// Returns an ordered list of fields of every record type instance
    pub fn fields(&self) -> &[Field] {
        self.fields.as_ref()
    }

    /// Returns an identity map of polymorphic variables associated with the constructor
    pub fn identity_ty_args(&self) -> TyArgs<ty::Poly> {
        use std::collections::HashMap;

        let mut pvar_purities = HashMap::new();
        let mut tvar_types = HashMap::new();

        // Create an identity map of our polymorphic variables. When we substitute in the selected
        // types the keys will stay the same while the values will be replaced.
        for poly_param in self.poly_params() {
            match poly_param {
                PolyParam::PVar(_, pvar) => {
                    pvar_purities.insert(pvar.clone(), pvar.clone().into());
                }
                PolyParam::TVar(_, tvar) => {
                    tvar_types.insert(tvar.clone(), tvar.clone().into());
                }
                PolyParam::Pure(_) | PolyParam::TFixed(_, _) => {}
            }
        }

        TyArgs::new(pvar_purities, tvar_types)
    }

    /// Returns the type of the value constructor function
    pub fn value_cons_fun_type(cons_id: &ConsId) -> ty::Fun {
        let ty_args = cons_id.identity_ty_args();
        let pvars: purity::PVars = ty_args.pvar_purities().keys().cloned().collect();
        let tvars: ty::TVars = ty_args.tvar_types().keys().cloned().collect();

        let ret_type = Instance::new(cons_id.clone(), ty_args).into();
        let top_fun = ty::TopFun::new(Purity::Pure.into(), ret_type);

        let params = ty::List::new_tuple(
            cons_id
                .fields
                .iter()
                .map(|field| field.ty_ref.clone())
                .collect(),
        );
        ty::Fun::new(pvars, tvars, top_fun, params)
    }
}

pub type ConsId = ArcId<Cons>;

impl<M: ty::PM> From<ConsId> for Ty<M> {
    fn from(cons_id: ConsId) -> Self {
        Ty::RecordClass(cons_id)
    }
}

impl<M: ty::PM> From<ConsId> for ty::Ref<M> {
    fn from(cons_id: ConsId) -> Self {
        ty::Ref::Fixed(Ty::RecordClass(cons_id))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Instance<M: ty::PM> {
    cons: ConsId,
    ty_args: TyArgs<M>,
}

impl<M: ty::PM> Instance<M> {
    pub fn new(cons: ConsId, ty_args: TyArgs<M>) -> Self {
        Self { cons, ty_args }
    }

    /// Returns the record constructor this instance was constructed from
    pub fn cons(&self) -> &ConsId {
        &self.cons
    }

    /// Returns the type arguments to the record type constructor
    ///
    /// Every [polymorphic parameter](Cons::poly_params) must be specified in the type arguments.
    pub fn ty_args(&self) -> &TyArgs<M> {
        &self.ty_args
    }
}

impl<M: ty::PM> From<Instance<M>> for Ty<M> {
    fn from(instance: Instance<M>) -> Self {
        Ty::Record(Box::new(instance))
    }
}

impl<M: ty::PM> From<Instance<M>> for ty::Ref<M> {
    fn from(instance: Instance<M>) -> Self {
        ty::Ref::Fixed(Ty::Record(Box::new(instance)))
    }
}
