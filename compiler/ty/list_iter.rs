use crate::ty;

/// Iterates through the member types of a list
#[derive(Clone)]
pub struct ListIterator<'list, M: ty::PM> {
    fixed: &'list [ty::Ref<M>],
    rest: Option<&'list ty::Ref<M>>,
}

impl<'list, M: ty::PM> ListIterator<'list, M> {
    pub fn new(list: &'list ty::List<M>) -> ListIterator<'list, M> {
        ListIterator {
            fixed: list.fixed(),
            rest: list.rest(),
        }
    }

    pub fn try_new_from_ty_ref(ty_ref: &'list ty::Ref<M>) -> Option<ListIterator<'list, M>> {
        match ty_ref.try_to_fixed() {
            Some(ty::Ty::List(list)) => Some(Self::new(list)),
            _ => None,
        }
    }

    pub fn fixed_len(&self) -> usize {
        self.fixed.len()
    }

    pub fn has_rest(&self) -> bool {
        self.rest.is_some()
    }

    pub fn next(&mut self) -> Option<&'list ty::Ref<M>> {
        if self.fixed.is_empty() {
            self.rest
        } else {
            let next = self.fixed.first();
            self.fixed = &self.fixed[1..];
            next
        }
    }

    pub fn tail_type(self) -> ty::List<M> {
        ty::List::new(self.fixed.to_vec().into_boxed_slice(), self.rest.cloned())
    }
}

impl<'list> ListIterator<'list, ty::Poly> {
    pub fn collect_rest(self, tvars: &ty::TVars) -> Option<ty::Ref<ty::Poly>> {
        if self.fixed.is_empty() {
            return self.rest.cloned();
        }

        Some(ty::unify::unify_ty_ref_iter(
            tvars,
            self.fixed
                .iter()
                .cloned()
                .chain(self.rest.cloned().into_iter()),
        ))
    }
}
