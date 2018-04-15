use std::ops::Range;

pub trait SeqTyIterator<'a, S> {
    /// Returns the remaining number of fixed member types
    fn fixed_len(&self) -> usize;

    /// Returns true if we have a rest/begin type
    fn is_infinite(&self) -> bool;

    /// Returns the next member type or None if there are no more types
    fn next(&mut self) -> Option<&'a S>;

    fn size_range(&self) -> Range<usize> {
        if self.is_infinite() {
            (self.fixed_len()..usize::max_value())
        } else {
            (self.fixed_len()..self.fixed_len())
        }
    }
}

/// Iterates through the member types of a list in forward order
pub struct ListTyIterator<'a, S: 'a> {
    fixed: &'a [S],
    rest: &'a Option<Box<S>>,
}

impl<'a, S> ListTyIterator<'a, S> {
    pub fn new(fixed: &'a [S], rest: &'a Option<Box<S>>) -> ListTyIterator<'a, S> {
        ListTyIterator { fixed, rest }
    }
}

impl<'a, S> SeqTyIterator<'a, S> for ListTyIterator<'a, S> {
    fn fixed_len(&self) -> usize {
        self.fixed.len()
    }

    fn is_infinite(&self) -> bool {
        self.rest.is_some()
    }

    fn next(&mut self) -> Option<&'a S> {
        if self.fixed.is_empty() {
            match *self.rest {
                Some(ref boxed) => Some(boxed.as_ref()),
                None => None,
            }
        } else {
            let next = self.fixed.first();
            self.fixed = &self.fixed[1..];
            next
        }
    }
}

/// Iterates through the member types of a vector in reverse order
pub struct RevVecTyIterator<'a, S: 'a> {
    begin: &'a Option<Box<S>>,
    fixed: &'a [S],
}

impl<'a, S> RevVecTyIterator<'a, S> {
    pub fn new(begin: &'a Option<Box<S>>, fixed: &'a [S]) -> RevVecTyIterator<'a, S> {
        RevVecTyIterator { begin, fixed }
    }
}

impl<'a, S> SeqTyIterator<'a, S> for RevVecTyIterator<'a, S> {
    fn fixed_len(&self) -> usize {
        self.fixed.len()
    }

    fn is_infinite(&self) -> bool {
        self.begin.is_some()
    }

    fn next(&mut self) -> Option<&'a S> {
        if self.fixed.is_empty() {
            match *self.begin {
                Some(ref boxed) => Some(boxed.as_ref()),
                None => None,
            }
        } else {
            let next = self.fixed.last();
            self.fixed = &self.fixed[..self.fixed.len() - 1];
            next
        }
    }
}
