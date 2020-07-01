use {
    crate::{Interner, Iter, Span},
    rayon::{iter::plumbing::*, prelude::*, slice},
};

/// A parallel iterator over all interned strings from an [`Interner`].
#[derive(Debug, Clone)]
#[cfg_attr(docs, doc(cfg(feature = "rayon")))]
pub struct ParIter<'a> {
    pub(super) spans: slice::Iter<'a, Span>,
    pub(super) span_to_string: &'a str,
}

impl<'a, S> IntoParallelIterator for &'a Interner<S> {
    type Iter = ParIter<'a>;
    type Item = &'a str;

    #[cfg_attr(feature = "inline-more", inline)]
    fn into_par_iter(self) -> ParIter<'a> {
        ParIter {
            spans: self.symbol_to_span.par_iter(),
            span_to_string: &self.span_to_string,
        }
    }
}

#[cfg_attr(docs, doc(cfg(feature = "rayon")))]
impl<'a> IntoParallelIterator for Iter<'a> {
    type Iter = ParIter<'a>;
    type Item = &'a str;

    #[cfg_attr(feature = "inline-more", inline)]
    fn into_par_iter(self) -> ParIter<'a> {
        ParIter {
            spans: self.spans.as_slice().par_iter(),
            span_to_string: self.span_to_string,
        }
    }
}

impl<'a> IndexedParallelIterator for ParIter<'a> {
    fn drive<C>(self, consumer: C) -> C::Result
    where
        C: Consumer<&'a str>,
    {
        bridge(self, consumer)
    }

    fn len(&self) -> usize {
        self.spans.len()
    }

    fn with_producer<CB>(self, callback: CB) -> CB::Output
    where
        CB: ProducerCallback<&'a str>,
    {
        let ParIter {
            spans,
            span_to_string,
        } = self;
        spans
            .map(|span| unsafe { index_unchecked!(span_to_string, span.ix()) })
            .with_producer(callback)
    }
}

impl<'a> ParallelIterator for ParIter<'a> {
    type Item = &'a str;

    fn drive_unindexed<C>(self, consumer: C) -> C::Result
    where
        C: UnindexedConsumer<&'a str>,
    {
        bridge(self, consumer)
    }

    fn opt_len(&self) -> Option<usize> {
        Some(self.spans.len())
    }
}
