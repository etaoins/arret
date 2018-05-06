use ty;

/// Iterates through the member types of a parameters list
pub struct ParamsIterator<'a, S>
where
    S: ty::TyRef + 'a,
{
    fixed: &'a [S],
    rest: &'a Option<S>,
}

impl<'a, S> ParamsIterator<'a, S>
where
    S: ty::TyRef + 'a,
{
    pub fn new(params: &'a ty::Params<S>) -> ParamsIterator<'a, S> {
        ParamsIterator {
            fixed: params.fixed(),
            rest: params.rest(),
        }
    }

    pub fn fixed_len(&self) -> usize {
        self.fixed.len()
    }

    pub fn next(&mut self) -> Option<&'a S> {
        if self.fixed.is_empty() {
            self.rest.as_ref()
        } else {
            let next = self.fixed.first();
            self.fixed = &self.fixed[1..];
            next
        }
    }
}
