//! An [arena]-inspired [interner][interning] for strings.
//!
//! The interner allows you to cache strings and instead deal in a simple
//! symbol, which is trivial to copy around and compare for equality.
//!
//! As opposed to most other interners, this interner stores all of the
//! interned strings in a single concatenated string. This reduces allocation
//! space required for the interned strings, as well as fragmentation of the
//! memory held by the interner.
//!
//! # Features
//!
//! - `compile-time-rng`: Enable ahash's compile-time RNG feature,
//!   so that RNG seeds are impacted by build-time RNG. This makes hash
//!   seeds even harder to determine, but prevents reproducable builds.
//!
//! - `inline-more`: Aggressively inline functions defined in this crate.
//!   Note that 99% of this crate is already monomorphized into consumers,
//!   and thus this just further inlines the functionality into every CGU
//!   of dependent crates, moving work from thin-crate LTO to codegen.
//!   This can cause a significant increase in compile time for some slight
//!   (unmeasured) performance benefits.
//!
//! - `inline-even-more`:
//!
//!   [arena]: <https://stackoverflow.com/q/12825148/3019990>
//!   [interning]: <https://en.wikipedia.org/wiki/String_interning>

#![cfg_attr(docs, feature(doc_cfg))]
#![no_std]
extern crate alloc;

macro_rules! index_unchecked {
    ($place:expr, $index:expr) => {
        if cfg!(debug_assertions) {
            &$place[$index]
        } else {
            $place.get_unchecked($index)
        }
    };
}

mod iter;
#[cfg(feature = "rayon")]
mod par_iter;

pub use iter::Iter;
#[cfg(feature = "rayon")]
pub use par_iter::ParIter;
use {
    alloc::{string::String, vec::Vec},
    core::{
        fmt,
        hash::{BuildHasher, Hash, Hasher},
        iter::{Extend, FromIterator},
        ops::{Index, Range},
    },
    hashbrown::hash_map::{HashMap, RawEntryMut},
};

#[cfg_attr(feature = "inline-more", inline)]
fn make_hash(builder: &impl BuildHasher, hashee: &(impl ?Sized + Hash)) -> u64 {
    let state = &mut builder.build_hasher();
    hashee.hash(state);
    state.finish()
}

#[repr(transparent)]
#[derive(Debug, Copy, Clone)]
struct Opaque<T>(T);

#[cfg(not(target_pointer_width = "16"))]
type Idx = u32;
#[cfg(not(target_pointer_width = "16"))]
type NonZeroIdx = core::num::NonZeroU32;

#[cfg(target_pointer_width = "16")]
type Idx = u16;
#[cfg(target_pointer_width = "16")]
type NonZeroIdx = core::num::NonZeroU16;

#[derive(Debug, Copy, Clone)]
/// A span of an interned string in the backing string.
///
/// This is stored as (start, length) rather than (start, end)
/// because this matches the storage of &str.
struct Span {
    base: Idx,
    len: Idx,
}

impl Span {
    // `string.get_unchecked(span.ix())` optimizes to just a pointer offset.
    // I would implement Index<Span> but we also need the unchecked indexing,
    // and StrIndex is unstable to implement.
    #[inline(always)]
    fn ix(self) -> Range<usize> {
        self.base as usize..self.base as usize + self.len as usize
    }
}

/// An interned string.
///
/// A `Symbol` is only as big as `u32` or `usize`, whichever is smaller.
/// For most platforms, that means `Symbol` is a `u32`. `Symbol` can
/// represent at least `i32::MAX` or `isize::MAX` distinct symbols,
/// whichever is smaller. The exact limit is unspecified.
///
/// Additionally, `Option<Symbol>` and `Symbol` have the same size.
/// (`Symbol` has at least one niche for `Option` to use.)
///
/// Symbols are ordered based on their side table index.
/// Symbols from different interners compare stably, but arbitrarily.
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct Symbol {
    raw: NonZeroIdx, // FUTURE: make the niche be the top bit, not 0?
}

impl Symbol {
    /// Convert the symbol into a `usize` appropriate for indexing side tables.
    ///
    /// The indexes are continuous, starting at 0.
    #[inline(always)]
    pub fn ix(self) -> usize {
        self.raw.get() as usize - 1
    }
}

/// An [interner][interning] for strings.
///
/// The interner allows you to cache strings and instead deal in a simple
/// symbol, which is trivial to copy around and compare for equality.
///
/// As opposed to most other interners, this interner stores all of the
/// interned strings in a single concatenated string. This reduces allocation
/// space required for the interned strings, as well as fragmentation of the
/// memory held by the interner.
///
/// In effect, this works as a handle-based [arena] specialized for strings.
/// The main drawback of this approach is that references to the actual
/// interned strings cannot live accross calls that may intern new symbols.
/// (This can be made possible by non-moving arena techniques.)
///
/// The default hashing algorithm is [aHash][ahash], though this is subject
/// to change at any point in the future. The hash function is very fast for
/// all lengths of strings, but the algorithm is _not_ necessarily guaranteed
/// to protect against attacks such as HashDoS.
///
///   [ahash]: <https://docs.rs/ahash/0.3/ahash>
///   [interning]: <https://en.wikipedia.org/wiki/String_interning>
///   [arena]: <https://stackoverflow.com/q/12825148/3019990>
#[derive(Clone)]
pub struct Interner<S = ahash::RandomState> {
    hasher: S,
    // SAFETY: symbols must be valid indexes into `symbol_to_span`.
    // `Opaque<Symbol>` is treated in the hashmap as the string it represents.
    string_to_symbol: HashMap<Opaque<Symbol>, (), ()>, // not HashSet so we can use raw_entry API
    // SAFETY: spans must be valid spans in `span_to_string`.
    symbol_to_span: Vec<Span>,
    span_to_string: String,
}

impl<S> fmt::Debug for Interner<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            f.debug_struct("Interner")
                .field("string_to_symbol", &self.string_to_symbol)
                .field("symbol_to_span", &self.symbol_to_span)
                .field("span_to_string", &self.span_to_string)
                .finish()
        } else {
            f.debug_set().entries(self.iter()).finish()
        }
    }
}

impl<S: Default> Default for Interner<S> {
    #[cfg_attr(feature = "inline-more", inline)]
    fn default() -> Self {
        Self::with_hasher(Default::default())
    }
}

/// Capacity specification for an [`Interner`].
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Capacity {
    /// The approximate number of symbols the interner can cache
    /// before requiring reallocation of the symbol map.
    pub symbols: usize,
    /// The approximate number of bytes the interner can cache
    /// before requiring reallocation of the backing string.
    pub bytes: usize,
}

impl Interner {
    /// Creates a new empty interner.
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn new() -> Self {
        Default::default()
    }

    /// Creates an empty interner with the specified capacity.
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn with_capacity(capacity: Capacity) -> Self {
        Self::with_hasher_and_capacity(Default::default(), capacity)
    }
}

impl<S> Interner<S> {
    /// Creates an empty interner which will use the given hash builder.
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn with_hasher(hasher: S) -> Self {
        Interner::with_hasher_and_capacity(hasher, Default::default())
    }

    /// Creates an empty interner with the specified capacity and hash builder.
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn with_hasher_and_capacity(hasher: S, capacity: Capacity) -> Self {
        Interner {
            hasher,
            string_to_symbol: HashMap::with_capacity_and_hasher(capacity.symbols, ()),
            symbol_to_span: Vec::with_capacity(capacity.symbols),
            span_to_string: String::with_capacity(capacity.bytes),
        }
    }

    /// The number of uniquely interned strings.
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn len(&self) -> usize {
        self.symbol_to_span.len()
    }

    /// Returns true if the interner has no elements.
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn is_empty(&self) -> bool {
        self.symbol_to_span.is_empty()
    }

    /// The number of interned bytes.
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn size(&self) -> usize {
        self.span_to_string.len()
    }

    /// Returns the string associated with the given symbol.
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn resolve(&self, s: Symbol) -> Option<&str> {
        let span = *self.symbol_to_span.get(s.ix())?;
        unsafe { Some(index_unchecked!(self.span_to_string, span.ix())) }
    }

    /// Iterate all interned strings in insertion order.
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn iter(&self) -> Iter<'_> {
        <&Self>::into_iter(self)
    }
}

impl<S> Index<Symbol> for Interner<S> {
    type Output = str;
    #[cfg_attr(feature = "inline-more", inline)]
    fn index(&self, s: Symbol) -> &str {
        self.resolve(s).expect("no entry found for symbol")
    }
}

#[inline]
fn insert_substring(string: &mut String, s: &str) -> Span {
    if string
        .len()
        .checked_add(s.len())
        .map(|end| end > Idx::MAX as usize)
        .unwrap_or(true)
    {
        panic!("Interner overflowed")
    }
    let base = string.len() as Idx;
    let len = s.len() as Idx;
    string.push_str(s);
    Span { base, len }
}

impl<S: BuildHasher> Interner<S> {
    /// Gets the interned symbol for this string,
    /// but does not insert it if it is missing.
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn get(&self, s: &str) -> Option<Symbol> {
        let Interner {
            hasher,
            string_to_symbol,
            symbol_to_span,
            span_to_string,
        } = self;

        let hash = make_hash(hasher, s);
        let entry = string_to_symbol
            .raw_entry()
            .from_hash(hash, |&Opaque(symbol)| {
                let span = unsafe { index_unchecked!(symbol_to_span, symbol.ix()) };
                s == unsafe { index_unchecked!(span_to_string, span.ix()) }
            });

        entry.map(|(&Opaque(symbol), &())| symbol)
    }

    /// Interns the given value.
    ///
    /// If the string has already been interned, this is copy-free.
    /// If the string has not yet been interned, it is copied in.
    #[cfg_attr(feature = "inline-more", inline)]
    pub fn get_or_insert(&mut self, s: &str) -> Symbol {
        let Interner {
            hasher,
            string_to_symbol,
            symbol_to_span,
            span_to_string,
        } = self;

        let hash = make_hash(hasher, s);
        let entry = string_to_symbol
            .raw_entry_mut()
            .from_hash(hash, |&Opaque(symbol)| {
                let span = unsafe { index_unchecked!(symbol_to_span, symbol.ix()) };
                s == unsafe { index_unchecked!(span_to_string, span.ix()) }
            });

        let (&mut Opaque(symbol), &mut ()) = match entry {
            RawEntryMut::Occupied(entry) => entry.into_key_value(),
            RawEntryMut::Vacant(entry) => {
                let span = insert_substring(span_to_string, s);
                symbol_to_span.push(span);
                let symbol = Symbol {
                    raw: unsafe { NonZeroIdx::new_unchecked(symbol_to_span.len() as Idx) },
                };

                entry.insert_with_hasher(hash, Opaque(symbol), (), |&Opaque(symbol)| {
                    let span = unsafe { index_unchecked!(symbol_to_span, symbol.ix()) };
                    let s = unsafe { index_unchecked!(span_to_string, span.ix()) };
                    make_hash(hasher, s)
                })
            }
        };
        symbol
    }
}

impl<S: BuildHasher, Str: AsRef<str>> Extend<Str> for Interner<S> {
    #[cfg_attr(feature = "inline-more", inline)]
    fn extend<I: IntoIterator<Item = Str>>(&mut self, iter: I) {
        // We imitate hashbrown's extend reservation policy here.
        // Reserve the entire hint lower bound if the map is empty.
        // Otherwise reserve half the hint (rounded up), so the map
        // wil only resize twice in the worst case.
        let iter = iter.into_iter();
        let reserve = if self.is_empty() {
            iter.size_hint().0
        } else {
            (iter.size_hint().0 + 1) / 2
        };
        self.symbol_to_span.reserve(reserve);
        iter.for_each(|s| {
            self.get_or_insert(s.as_ref());
        });
    }
}

impl<S: Default + BuildHasher, Str: AsRef<str>> FromIterator<Str> for Interner<S> {
    #[cfg_attr(feature = "inline-more", inline)]
    fn from_iter<I: IntoIterator<Item = Str>>(iter: I) -> Self {
        let mut this = Self::default();
        this.extend(iter);
        this
    }
}
