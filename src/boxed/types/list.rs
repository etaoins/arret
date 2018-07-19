use std::marker::PhantomData;

use abitype::{BoxedABIType, EncodeBoxedABIType};
use boxed::heap::Heap;
use boxed::refs::Gc;
use boxed::{
    AllocType, Any, BoxSize, Boxed, ConstructableFrom, Header, Nil, TypeTag, NIL_INSTANCE,
};

#[repr(C, align(16))]
pub struct Pair<T>
where
    T: Boxed,
{
    pub header: Header,
    pub head: Gc<T>,
    pub rest: Gc<List<T>>,
    pub list_length: usize,
}

impl<T> Boxed for Pair<T> where T: Boxed {}

impl<T> EncodeBoxedABIType for Pair<T>
where
    T: EncodeBoxedABIType + Boxed,
{
    const BOXED_ABI_TYPE: BoxedABIType = BoxedABIType::Pair(&T::BOXED_ABI_TYPE);
}

impl<T> Pair<T>
where
    T: Boxed,
{
    fn as_list_ref(&self) -> Gc<List<T>> {
        unsafe { Gc::new(self as *const Self as *const List<T>) }
    }
}

type PairInput<T> = (Gc<T>, Gc<List<T>>);

impl<T> ConstructableFrom<PairInput<T>> for Pair<T>
where
    T: Boxed,
{
    fn size_for_value(_: &PairInput<T>) -> BoxSize {
        BoxSize::Size32
    }

    fn new_with_alloc_type(value: PairInput<T>, alloc_type: AllocType) -> Pair<T> {
        Pair {
            header: Header {
                type_tag: TypeTag::TopPair,
                alloc_type,
            },
            head: value.0,
            rest: value.1,
            list_length: value.1.len() + 1,
        }
    }
}

#[repr(C, align(16))]
pub struct List<T>
where
    T: Boxed,
{
    pub header: Header,
    phantom: PhantomData<T>,
}

pub enum ListSubtype<'a, T>
where
    T: Boxed + 'a,
{
    Pair(&'a Pair<T>),
    Nil,
}

impl<T> List<T>
where
    T: Boxed,
{
    pub fn new(heap: &mut Heap, elems: impl DoubleEndedIterator<Item = Gc<T>>) -> Gc<List<T>> {
        let initial_tail = Self::empty();

        // TODO: This is naive; we could use a single multi-cell allocation instead
        elems.rfold(initial_tail, |tail, elem| {
            Pair::new(heap, (elem, tail)).as_list_ref()
        })
    }

    pub fn empty() -> Gc<List<T>> {
        unsafe { Gc::new(&NIL_INSTANCE as *const Nil as *const List<T>) }
    }

    pub fn as_subtype(&self) -> ListSubtype<T> {
        match self.header.type_tag {
            TypeTag::TopPair => {
                ListSubtype::Pair(unsafe { &*(self as *const List<T> as *const Pair<T>) })
            }
            TypeTag::Nil => ListSubtype::Nil,
            other => {
                unreachable!("Unexpected type tag: {:?}", other);
            }
        }
    }

    pub fn len(&self) -> usize {
        match self.as_subtype() {
            ListSubtype::Pair(pair) => pair.list_length,
            ListSubtype::Nil => 0,
        }
    }

    pub fn as_any_ref(&self) -> Gc<Any> {
        unsafe { Gc::new(&*(self as *const Self as *const Any)) }
    }

    pub fn iter(&self) -> ListIterator<T> {
        ListIterator {
            head: unsafe { Gc::new(self as *const Self) },
        }
    }
}

impl<T> Boxed for List<T> where T: Boxed {}

impl<T> EncodeBoxedABIType for List<T>
where
    T: EncodeBoxedABIType + Boxed,
{
    const BOXED_ABI_TYPE: BoxedABIType = BoxedABIType::List(&T::BOXED_ABI_TYPE);
}

pub struct ListIterator<T>
where
    T: Boxed,
{
    head: Gc<List<T>>,
}

impl<T> Iterator for ListIterator<T>
where
    T: Boxed,
{
    type Item = Gc<T>;

    fn next(&mut self) -> Option<Gc<T>> {
        // If we use `head` directy the borrow checker gets suspicious
        let head = unsafe { &*(self.head.as_ptr()) };

        match head.as_subtype() {
            ListSubtype::Pair(pair) => {
                self.head = pair.rest;
                Some(pair.head)
            }
            ListSubtype::Nil => None,
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.head.len(), Some(self.head.len()))
    }
}

impl<T> ExactSizeIterator for ListIterator<T> where T: Boxed {}

#[repr(C, align(16))]
pub struct TopPair {
    pub header: Header,
}

impl TopPair {
    fn as_pair(&self) -> Gc<Pair<Any>> {
        unsafe { Gc::new(&*(self as *const TopPair as *const Pair<Any>)) }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use boxed::Int;
    use std::mem;

    #[test]
    fn sizes() {
        assert_eq!(32, mem::size_of::<Pair<Any>>());
    }

    #[test]
    fn construct_and_iter() {
        let mut heap = Heap::new();

        let boxed_ints = [1, 2, 3]
            .iter()
            .map(|num| Int::new(&mut heap, *num))
            .collect::<Vec<Gc<Int>>>();

        let boxed_list = List::new(&mut heap, boxed_ints.into_iter());

        let mut boxed_list_iter = boxed_list.iter();
        assert_eq!(3, boxed_list_iter.len());

        for expected_num in &[1, 2, 3] {
            if let Some(boxed_int) = boxed_list_iter.next() {
                assert_eq!(*expected_num, boxed_int.value);
            } else {
                panic!("Iterator unexpectedly ended");
            }
        }

        assert_eq!(0, boxed_list_iter.len());
        assert_eq!(false, boxed_list_iter.next().is_some());
    }
}
