//! It does represents types that only unifies with itself, like `Int`, `Bool`, `String`,
//! they are the type constructors.
//!
//! ## Constructors
//!
//! The constructors can be defined by users like:
//!
//! ```coq
//! inductive Hello
//! ```
//!
//! In the previous example, `Hello` is a type constructor, and it's a type itself.
//!
//! ## Universes
//!
//! Universes aren't polymorphic, so the type of a type is always `Type`. You can see: `Type : Type` as
//! an axiom of the zure language.
//!
//! ## Literal values
//!
//! Literal values are values that can be used in the language, like `1`, `2`, `"hello"`, `true`, `false`. They
//! can be evaluated to values and be constructors too, like, they only unify with themselves, so it's easy
//! to compare just using the [`PartialEq`] trait.

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constr {
  Uni,
}
