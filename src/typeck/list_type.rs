use std::result;
use ty;

type Result<T> = result::Result<T, Error>;

#[derive(PartialEq, Debug)]
pub enum Error {
    NonList(ty::Poly),
    PolyStructure(ty::Poly),
    PolyConflict(ty::Poly, ty::Poly),
}

pub trait FromListMembers {
    fn from_list_members<I>(fixed: I, rest: Option<ty::Poly>) -> Self
    where
        I: DoubleEndedIterator<Item = ty::Poly>;
}

impl FromListMembers for ty::Poly {
    fn from_list_members<I>(fixed: I, rest: Option<ty::Poly>) -> Self
    where
        I: DoubleEndedIterator<Item = ty::Poly>,
    {
        ty::Ty::new_simple_list_type(fixed, rest).into_poly()
    }
}

impl FromListMembers for ty::Params<ty::Poly> {
    fn from_list_members<I>(fixed: I, rest: Option<ty::Poly>) -> Self
    where
        I: DoubleEndedIterator<Item = ty::Poly>,
    {
        ty::Params::new(fixed.collect(), rest)
    }
}

pub trait ListTypeIterator: Sized {
    fn next(&mut self) -> Result<Option<&ty::Poly>>;

    fn tail_type(self) -> ty::Poly;
    fn collect_rest(self, tvars: &[ty::TVar]) -> Result<Option<ty::Poly>>;
}

pub struct PolyIterator<'a> {
    head: &'a ty::Poly,
}

impl<'a> PolyIterator<'a> {
    pub fn new(head: &ty::Poly) -> PolyIterator {
        PolyIterator { head }
    }
}

impl<'a> ListTypeIterator for PolyIterator<'a> {
    fn next(&mut self) -> Result<Option<&ty::Poly>> {
        match self.head {
            ty::Poly::Fixed(ty::Ty::Cons(car, cdr)) => {
                self.head = cdr;
                Ok(Some(car))
            }
            ty::Poly::Fixed(ty::Ty::Listof(member)) => Ok(Some(member)),
            ty::Poly::Fixed(ty::Ty::Nil) => Ok(None),
            ty::Poly::Fixed(_) => {
                // Not a list
                Err(Error::NonList(self.head.clone()))
            }
            ty::Poly::Var(_) => {
                // Too polymorphic
                Err(Error::PolyStructure(self.head.clone()))
            }
        }
    }

    fn tail_type(self) -> ty::Poly {
        self.head.clone()
    }

    fn collect_rest(mut self, tvars: &[ty::TVar]) -> Result<Option<ty::Poly>> {
        match self.head {
            ty::Poly::Fixed(ty::Ty::Listof(member)) => {
                return Ok(Some(member.as_ref().clone()));
            }
            ty::Poly::Fixed(ty::Ty::Nil) => {
                return Ok(None);
            }
            _ => {}
        }

        let mut tail_types: Vec<ty::Poly> = vec![];
        while let ty::Poly::Fixed(ty::Ty::Cons(car, cdr)) = self.head {
            tail_types.push(car.as_ref().clone());
            self.head = cdr;
        }

        match self.head {
            ty::Poly::Fixed(ty::Ty::Listof(member)) => tail_types.push(member.as_ref().clone()),
            ty::Poly::Fixed(ty::Ty::Nil) => {}
            ty::Poly::Fixed(_) => {
                // Not a list
                return Err(Error::NonList(self.head.clone()));
            }
            ty::Poly::Var(_) => {
                // Too polymorphic
                return Err(Error::PolyStructure(self.head.clone()));
            }
        }

        ty::unify::poly_unify_iter(tvars, tail_types.into_iter())
            .map(Some)
            .map_err(|ty::unify::PolyError::PolyConflict(left, right)| {
                Error::PolyConflict(left, right)
            })
    }
}

pub struct ParamsIterator<'a> {
    fixed: &'a [ty::Poly],
    rest: &'a Option<ty::Poly>,
}

impl<'a> ParamsIterator<'a> {
    pub fn new(params: &ty::Params<ty::Poly>) -> ParamsIterator {
        ParamsIterator {
            fixed: params.fixed(),
            rest: params.rest(),
        }
    }
}

impl<'a> ListTypeIterator for ParamsIterator<'a> {
    fn next(&mut self) -> Result<Option<&ty::Poly>> {
        if self.fixed.is_empty() {
            Ok(self.rest.as_ref())
        } else {
            let next = self.fixed.first();
            self.fixed = &self.fixed[1..];
            Ok(next)
        }
    }

    fn tail_type(self) -> ty::Poly {
        ty::Ty::new_simple_list_type(self.fixed.iter().cloned(), self.rest.clone()).into_poly()
    }

    fn collect_rest(self, tvars: &[ty::TVar]) -> Result<Option<ty::Poly>> {
        if self.fixed.is_empty() {
            return Ok(self.rest.clone());
        }

        // TODO: Span
        ty::unify::poly_unify_iter(
            tvars,
            self.fixed
                .into_iter()
                .cloned()
                .chain(self.rest.clone().into_iter()),
        ).map(Some)
            .map_err(|ty::unify::PolyError::PolyConflict(left, right)| {
                Error::PolyConflict(left, right)
            })
    }
}
