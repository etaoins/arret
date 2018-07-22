use std::{marker, mem};

use abitype::{BoxedABIType, EncodeBoxedABIType};
use boxed::refs::Gc;
use boxed::{AllocType, Any, BoxSize, Boxed, ConstructableFrom, Header, TypeTag};
use intern::Interner;

const MAX_16BYTE_INLINE_SIZE: usize = ((16 - 8) / mem::size_of::<Gc<Any>>());
const MAX_32BYTE_INLINE_SIZE: usize = ((32 - 8) / mem::size_of::<Gc<Any>>());

const MAX_INLINE_SIZE: usize = MAX_32BYTE_INLINE_SIZE;

#[repr(C, align(16))]
pub struct Vector<T>
where
    T: Boxed,
{
    pub header: Header,
    pub size: u32,
    pub padding: [u8; 24],
    pub phantom: marker::PhantomData<T>,
}

impl<T> Boxed for Vector<T> where T: Boxed {}

impl<'a, T> ConstructableFrom<&'a [Gc<T>]> for Vector<T>
where
    T: Boxed,
{
    fn size_for_value(values: &&[Gc<T>]) -> BoxSize {
        if values.len() <= MAX_16BYTE_INLINE_SIZE {
            // 1 cell inline
            BoxSize::Size16
        } else {
            // 2 cell inline or large
            BoxSize::Size32
        }
    }

    fn construct(values: &[Gc<T>], alloc_type: AllocType, _: &mut Interner) -> Vector<T> {
        let header = Header {
            type_tag: TypeTag::TopVector,
            alloc_type,
        };

        unsafe {
            if values.len() <= MAX_INLINE_SIZE {
                let mut inline_vec: InlineVector<T> = InlineVector {
                    header,
                    size: values.len() as u32,
                    values: mem::uninitialized(),
                };
                inline_vec.values[0..values.len()].copy_from_slice(values);

                mem::transmute(inline_vec)
            } else {
                let large_vec = LargeVector {
                    header,
                    size: values.len() as u32,
                    values: values.into(),
                };

                mem::transmute(large_vec)
            }
        }
    }
}

impl<T> Vector<T>
where
    T: Boxed,
{
    fn is_inline(&self) -> bool {
        self.size <= (MAX_INLINE_SIZE as u32)
    }

    fn as_repr(&self) -> Repr<T> {
        if self.is_inline() {
            Repr::Inline(unsafe { &*(self as *const Vector<T> as *const InlineVector<T>) })
        } else {
            Repr::Large(unsafe { &*(self as *const Vector<T> as *const LargeVector<T>) })
        }
    }

    pub fn len(&self) -> usize {
        self.size as usize
    }

    pub fn values(&self) -> &[Gc<T>] {
        match self.as_repr() {
            Repr::Inline(inline) => &inline.values[0..self.len()],
            Repr::Large(large) => &large.values,
        }
    }

    pub(crate) fn values_mut(&mut self) -> &mut [Gc<T>] {
        unsafe {
            if self.is_inline() {
                &mut (*(self as *mut Vector<T> as *mut InlineVector<T>)).values[0..self.len()]
            } else {
                &mut (*(self as *mut Vector<T> as *mut LargeVector<T>)).values
            }
        }
    }
}

impl<T> EncodeBoxedABIType for Vector<T>
where
    T: EncodeBoxedABIType + Boxed,
{
    const BOXED_ABI_TYPE: BoxedABIType = BoxedABIType::Vector(&T::BOXED_ABI_TYPE);
}

#[repr(C, align(16))]
pub struct InlineVector<T>
where
    T: Boxed,
{
    pub header: Header,
    pub size: u32,
    pub values: [Gc<T>; MAX_INLINE_SIZE],
}

#[repr(C, align(16))]
pub struct LargeVector<T>
where
    T: Boxed,
{
    pub header: Header,
    pub size: u32,
    pub values: Vec<Gc<T>>,
}

enum Repr<'a, T>
where
    T: Boxed + 'a,
{
    Inline(&'a InlineVector<T>),
    Large(&'a LargeVector<T>),
}

#[repr(C, align(16))]
pub struct TopVector {
    pub header: Header,
}

impl TopVector {
    fn as_vector(&self) -> Gc<Vector<Any>> {
        unsafe { Gc::new(&*(self as *const TopVector as *const Vector<Any>)) }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::mem;

    #[test]
    fn sizes() {
        assert_eq!(32, mem::size_of::<Vector<Any>>());
        assert_eq!(32, mem::size_of::<InlineVector<Any>>());
        assert_eq!(32, mem::size_of::<LargeVector<Any>>());
    }
}
