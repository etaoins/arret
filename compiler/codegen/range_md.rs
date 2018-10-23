use std::iter;
use std::ops::Range;

use llvm_sys::core::*;
use llvm_sys::prelude::*;

type Int = i64;

struct IntRangeIter<I>
where
    I: Iterator<Item = Int>,
{
    inner_iter: iter::Peekable<I>,
}

impl<I> Iterator for IntRangeIter<I>
where
    I: Iterator<Item = Int>,
{
    type Item = Range<Int>;

    fn next(&mut self) -> Option<Range<Int>> {
        let range_start = if let Some(range_start) = self.inner_iter.next() {
            range_start
        } else {
            return None;
        };

        let mut range_length: Int = 1;
        while self.inner_iter.peek() == Some(&(range_start + range_length)) {
            self.inner_iter.next();
            range_length += 1;
        }

        Some(range_start..range_start + (range_length as Int))
    }
}

/// Finds ranges of consecutive integers from a sorted iterator
fn find_int_ranges(input: impl Iterator<Item = Int>) -> impl Iterator<Item = Range<Int>> {
    IntRangeIter {
        inner_iter: input.peekable(),
    }
}

/// Generates a range metadata node from a sorted iterator of possible values
pub fn int_range_metadata_node(
    llx: LLVMContextRef,
    llvm_int_type: LLVMTypeRef,
    input: impl Iterator<Item = Int>,
) -> LLVMValueRef {
    unsafe {
        let mut llvm_range_values: Vec<LLVMValueRef> = find_int_ranges(input)
            .flat_map(|range| iter::once(range.start).chain(iter::once(range.end)))
            .map(|value| LLVMConstInt(llvm_int_type, value as u64, 0))
            .collect();

        LLVMMDNodeInContext(
            llx,
            llvm_range_values.as_mut_ptr(),
            llvm_range_values.len() as u32,
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn empty() {
        let iter = find_int_ranges([].iter().cloned());
        assert_eq!(0, iter.count());
    }

    #[test]
    fn single_value() {
        let mut iter = find_int_ranges([-5].iter().cloned());

        assert_eq!(Some(-5..-4), iter.next());
        assert_eq!(None, iter.next());
    }

    #[test]
    fn single_range() {
        let mut iter = find_int_ranges([-1, 0, 1].iter().cloned());

        assert_eq!(Some(-1..2), iter.next());
        assert_eq!(None, iter.next());
    }

    #[test]
    fn multi_range() {
        let mut iter = find_int_ranges([-5, -1, 0, 1, 90, 91, 92].iter().cloned());

        assert_eq!(Some(-5..-4), iter.next());
        assert_eq!(Some(-1..2), iter.next());
        assert_eq!(Some(90..93), iter.next());
        assert_eq!(None, iter.next());
    }
}
