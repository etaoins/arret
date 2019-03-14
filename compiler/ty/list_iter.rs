use crate::ty;

/// Iterates through the member types of a list
#[derive(Clone)]
pub struct ListIterator<'list, M: ty::PM> {
    fixed: &'list [ty::Ref<M>],
    rest: &'list ty::Ref<M>,
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
        !self.rest.is_never()
    }

    pub fn tail_type(self) -> ty::List<M> {
        ty::List::new(self.fixed.to_vec().into_boxed_slice(), self.rest.clone())
    }

    pub fn collect_rest(self) -> ty::Ref<M> {
        use std::iter;

        if self.fixed.is_empty() {
            self.rest.clone()
        } else {
            ty::unify::unify_ty_ref_iter(self.fixed.iter().chain(iter::once(self.rest)).cloned())
        }
    }
}

impl<'list, M: ty::PM> Iterator for ListIterator<'list, M> {
    type Item = &'list ty::Ref<M>;

    fn next(&mut self) -> Option<&'list ty::Ref<M>> {
        if self.fixed.is_empty() {
            if self.rest.is_never() {
                None
            } else {
                Some(self.rest)
            }
        } else {
            let next = self.fixed.first();
            self.fixed = &self.fixed[1..];
            next
        }
    }
}
