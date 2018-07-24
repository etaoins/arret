use std::marker::PhantomData;
use std::{fmt, mem};

use abitype::{BoxedABIType, EncodeBoxedABIType};
use boxed::refs::Gc;
use boxed::{
    AllocType, Any, AsHeap, BoxSize, Boxed, ConstructableFrom, Header, Nil, TypeTag, NIL_INSTANCE,
};
use intern::Interner;

#[repr(C, align(16))]
pub struct Pair<T: Boxed> {
    header: Header,
    pub(crate) head: Gc<T>,
    pub(crate) rest: Gc<List<T>>,
    list_length: usize,
}

impl<T: Boxed> Boxed for Pair<T> {}

impl<T: Boxed> EncodeBoxedABIType for Pair<T>
where
    T: EncodeBoxedABIType,
{
    const BOXED_ABI_TYPE: BoxedABIType = BoxedABIType::Pair(&T::BOXED_ABI_TYPE);
}

impl<T: Boxed> Pair<T> {
    fn as_list_ref(&self) -> Gc<List<T>> {
        unsafe { Gc::new(self as *const Self as *const List<T>) }
    }
}

type PairInput<T> = (Gc<T>, Gc<List<T>>);

impl<T: Boxed> ConstructableFrom<PairInput<T>> for Pair<T> {
    fn size_for_value(_: &PairInput<T>) -> BoxSize {
        // TODO: It'd be nice to expose this as const BOX_SIZE: BoxSize once `if` is allowed in
        // const contexts
        if mem::size_of::<Self>() == 16 {
            BoxSize::Size16
        } else if mem::size_of::<Self>() == 32 {
            BoxSize::Size32
        } else {
            unreachable!("Unsupported pair size!")
        }
    }

    fn construct(value: PairInput<T>, alloc_type: AllocType, _: &mut Interner) -> Pair<T> {
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
pub struct List<T: Boxed> {
    header: Header,
    phantom: PhantomData<T>,
}

pub enum ListSubtype<'a, T: Boxed>
where
    T: 'a,
{
    Pair(&'a Pair<T>),
    Nil,
}

impl<T: Boxed> List<T> {
    pub fn new(
        heap: &mut impl AsHeap,
        elems: impl DoubleEndedIterator<Item = Gc<T>>,
    ) -> Gc<List<T>> {
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

    pub fn is_empty(&self) -> bool {
        self.header.type_tag == TypeTag::Nil
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

impl<T: Boxed> PartialEq for List<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &List<T>) -> bool {
        self.iter().eq(other.iter())
    }
}

impl<T: Boxed> fmt::Debug for List<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        formatter.write_str("List(")?;
        formatter.debug_list().entries(self.iter()).finish()?;
        formatter.write_str(")")
    }
}

impl<T: Boxed> Boxed for List<T> {}

impl<T: Boxed> EncodeBoxedABIType for List<T>
where
    T: EncodeBoxedABIType,
{
    const BOXED_ABI_TYPE: BoxedABIType = BoxedABIType::List(&T::BOXED_ABI_TYPE);
}

pub struct ListIterator<T: Boxed> {
    head: Gc<List<T>>,
}

impl<T: Boxed> Iterator for ListIterator<T> {
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

impl<T: Boxed> ExactSizeIterator for ListIterator<T> {}

#[repr(C, align(16))]
pub struct TopPair {
    header: Header,
}

impl TopPair {
    pub fn as_pair(&self) -> Gc<Pair<Any>> {
        unsafe { Gc::new(&*(self as *const TopPair as *const Pair<Any>)) }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use boxed::heap::Heap;
    use boxed::Int;
    use std::mem;

    #[test]
    fn sizes() {
        assert!([16, 32].contains(&mem::size_of::<Pair<Any>>()));
    }

    #[test]
    fn equality() {
        use boxed::Int;

        let mut heap = Heap::new();

        let boxed1 = Int::new(&mut heap, 1);
        let boxed2 = Int::new(&mut heap, 2);
        let boxed3 = Int::new(&mut heap, 3);

        let forward_list1 = List::new(&mut heap, vec![boxed1, boxed2, boxed3].into_iter());
        let forward_list2 = List::new(&mut heap, vec![boxed1, boxed2, boxed3].into_iter());
        let reverse_list = List::new(&mut heap, vec![boxed3, boxed2, boxed1].into_iter());

        assert_ne!(forward_list1, reverse_list);
        assert_eq!(forward_list1, forward_list2);
    }

    #[test]
    fn fmt_debug() {
        use boxed::Int;

        let mut heap = Heap::new();

        let boxed1 = Int::new(&mut heap, 1);
        let boxed2 = Int::new(&mut heap, 2);
        let boxed3 = Int::new(&mut heap, 3);

        let forward_list = List::new(&mut heap, vec![boxed1, boxed2, boxed3].into_iter());

        assert_eq!(
            "List([Int(1), Int(2), Int(3)])",
            format!("{:?}", forward_list)
        );
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
                assert_eq!(*expected_num, boxed_int.value());
            } else {
                panic!("Iterator unexpectedly ended");
            }
        }

        assert_eq!(0, boxed_list_iter.len());
        assert_eq!(false, boxed_list_iter.next().is_some());
    }
}
