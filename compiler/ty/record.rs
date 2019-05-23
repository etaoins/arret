use arret_syntax::datum::DataStr;
use arret_syntax::span::Span;

use crate::id_type::ArcId;
use crate::ty;
use crate::ty::purity;
use crate::ty::purity::Purity;
use crate::ty::ty_args::TyArgs;

/// Record field of a record constructor
#[derive(PartialEq, Debug, Clone)]
pub struct Field {
    name: DataStr,
    ty_ref: ty::Ref<ty::Poly>,
}

impl Field {
    pub fn new(name: DataStr, ty_ref: ty::Ref<ty::Poly>) -> Self {
        Self { name, ty_ref }
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
}

/// Indicates the variance of a record constructor's polymorphic parameter
///
/// By default variables are covariant. Whenever a variable appears inside a function's parameter
/// list it becomes contravariant, flipping again for each nested function type. If a variable
/// appears in both a covariant and contravariant position then it becomes invariant.
#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Variance {
    /// Subtypes of the record have subtypes of this variable
    Covariant,
    /// Subtypes of the record have supertypes of this variable
    Contravariant,
    /// No subtype relationship exists between records with different types of this variable
    Invariant,
}

/// Polymorphic parameter to a record constructor
///
/// This doesn't use separate [`TVar`](ty::TVar) and [`PVar`](purity::PVar) vectors because they
/// appear in the same parameter list and their ordering is important.
#[derive(PartialEq, Debug, Clone)]
pub enum PolyParam {
    PVar(Variance, purity::PVarId),
    TVar(Variance, ty::TVarId),
}

impl PolyParam {
    pub fn variance(&self) -> Variance {
        match self {
            PolyParam::PVar(variance, _) => *variance,
            PolyParam::TVar(variance, _) => *variance,
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
    name: DataStr,
    poly_params: Box<[PolyParam]>,
    fields: Box<[Field]>,
}

impl Cons {
    pub fn new(
        span: Span,
        name: DataStr,
        poly_params: Box<[PolyParam]>,
        fields: Box<[Field]>,
    ) -> Self {
        Self {
            span,
            name,
            poly_params,
            fields,
        }
    }

    /// Returns the span where the constructor was defined
    pub fn span(&self) -> Span {
        self.span
    }

    /// Returns the name of the constructor
    ///
    /// Unlike other source names that are used for diagnostics, this is semantically meaningful.
    /// It's used to define a constructor function.
    pub fn name(&self) -> &DataStr {
        &self.name
    }

    /// Returns the polymorphic parameters this constructor accepts
    pub fn poly_params(&self) -> &[PolyParam] {
        &self.poly_params
    }

    /// Returns an ordered list of fields of every record type instance
    pub fn fields(&self) -> &[Field] {
        self.fields.as_ref()
    }

    /// Returns the type of the value constructor function
    pub fn value_cons_fun_type(cons_id: &ConsId) -> ty::Fun {
        if !cons_id.poly_params().is_empty() {
            unimplemented!("polymorphic record value constructors");
        }

        let ret_type = Instance::new(cons_id.clone(), TyArgs::empty()).into();
        let top_fun = ty::TopFun::new(Purity::Pure.into(), ret_type);

        let params = ty::List::new(
            cons_id
                .fields
                .iter()
                .map(|field| field.ty_ref.clone())
                .collect(),
            ty::Ty::never().into(),
        );
        ty::Fun::new(vec![], vec![], top_fun, params)
    }
}

pub type ConsId = ArcId<Cons>;

impl<M: ty::PM> From<ConsId> for ty::Ty<M> {
    fn from(cons_id: ConsId) -> Self {
        ty::Ty::TopRecord(cons_id)
    }
}

impl<M: ty::PM> From<ConsId> for ty::Ref<M> {
    fn from(cons_id: ConsId) -> Self {
        ty::Ref::Fixed(ty::Ty::TopRecord(cons_id))
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

impl<M: ty::PM> From<Instance<M>> for ty::Ty<M> {
    fn from(instance: Instance<M>) -> Self {
        ty::Ty::Record(Box::new(instance))
    }
}

impl<M: ty::PM> From<Instance<M>> for ty::Ref<M> {
    fn from(instance: Instance<M>) -> Self {
        ty::Ref::Fixed(ty::Ty::Record(Box::new(instance)))
    }
}
