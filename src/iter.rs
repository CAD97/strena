use {
    crate::{Interner, Span},
    core::{iter, slice},
};

/// An iterator over all interned strings from an [`Interner`].
#[derive(Debug, Clone)]
pub struct Iter<'a> {
    pub(super) spans: slice::Iter<'a, Span>,
    pub(super) span_to_string: &'a str,
}

impl<'a, S> IntoIterator for &'a Interner<S> {
    type Item = &'a str;
    type IntoIter = Iter<'a>;

    #[cfg_attr(feature = "inline-more", inline)]
    fn into_iter(self) -> Self::IntoIter {
        Iter {
            spans: self.symbol_to_span.iter(),
            span_to_string: &self.span_to_string,
        }
    }
}

// The same impls as iter::Map
impl<'a> Iterator for Iter<'a> {
    type Item = &'a str;

    #[inline]
    fn next(&mut self) -> Option<&'a str> {
        let span = self.spans.next()?;
        Some(unsafe { index_unchecked!(self.span_to_string, span.ix()) })
    }

    #[cfg_attr(feature = "inline-more", inline)]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.spans.size_hint()
    }

    #[cfg_attr(feature = "inline-more", inline)]
    fn fold<Acc, G>(self, init: Acc, mut g: G) -> Acc
    where
        G: FnMut(Acc, &'a str) -> Acc,
    {
        let Iter {
            spans,
            span_to_string,
        } = self;
        spans.fold(init, |acc, span| {
            g(acc, unsafe { index_unchecked!(span_to_string, span.ix()) })
        })
    }
}

impl<'a> DoubleEndedIterator for Iter<'a> {
    #[inline]
    fn next_back(&mut self) -> Option<&'a str> {
        let span = self.spans.next_back()?;
        Some(unsafe { index_unchecked!(self.span_to_string, span.ix()) })
    }

    #[cfg_attr(feature = "inline-more", inline)]
    fn rfold<Acc, G>(self, init: Acc, mut g: G) -> Acc
    where
        G: FnMut(Acc, Self::Item) -> Acc,
    {
        let Iter {
            spans,
            span_to_string,
        } = self;
        spans.rfold(init, |acc, span| {
            g(acc, unsafe { index_unchecked!(span_to_string, span.ix()) })
        })
    }
}

impl ExactSizeIterator for Iter<'_> {
    #[cfg_attr(feature = "inline-more", inline)]
    fn len(&self) -> usize {
        self.spans.len()
    }
}

impl iter::FusedIterator for Iter<'_> {}
