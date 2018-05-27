use ty;

/// Iterates through the member types of a list
pub struct ListIterator<'a, S>
where
    S: ty::TyRef + 'a,
{
    fixed: &'a [S],
    rest: Option<&'a S>,
}

impl<'a, S> ListIterator<'a, S>
where
    S: ty::TyRef + 'a,
{
    pub fn new(list: &'a ty::List<S>) -> ListIterator<'a, S> {
        ListIterator {
            fixed: list.fixed(),
            rest: list.rest(),
        }
    }

    pub fn try_new_from_ty_ref(ty_ref: &'a S) -> Option<ListIterator<'a, S>> {
        match ty_ref.try_to_fixed() {
            Some(ty::Ty::List(list)) => Some(Self::new(list)),
            _ => None,
        }
    }

    pub fn fixed_len(&self) -> usize {
        self.fixed.len()
    }

    pub fn next(&mut self) -> Option<&'a S> {
        if self.fixed.is_empty() {
            self.rest
        } else {
            let next = self.fixed.first();
            self.fixed = &self.fixed[1..];
            next
        }
    }

    pub fn tail_type(self) -> ty::List<S> {
        ty::List::new(self.fixed.to_vec(), self.rest.cloned())
    }
}

impl<'a> ListIterator<'a, ty::Poly> {
    pub fn collect_rest(
        self,
        tvars: &[ty::TVar],
    ) -> Result<Option<ty::Poly>, ty::unify::PolyError> {
        if self.fixed.is_empty() {
            return Ok(self.rest.cloned());
        }

        Ok(Some(ty::unify::poly_unify_iter(
            tvars,
            self.fixed
                .into_iter()
                .cloned()
                .chain(self.rest.cloned().into_iter()),
        )?))
    }
}
