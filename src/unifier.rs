//! Equality properties between two types. It does a deep comparison, and close
//! holes between the two types, if possible, using pattern unification.
//!
//! The main part of unification is comparing two types, and returning a result.
