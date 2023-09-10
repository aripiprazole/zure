//! Concrete syntax tree, it is the representation of the source code in the memory. It's a high-level
//! representation of the source code, it's not the same as the AST, it's a bit more high-level than
//! the AST, because it represents better the source code in it's integrality.

/// The simple identifier, it's a name that can be used to reference a definition
/// in the source code.
///
/// It can be also the name of definition.
#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct Ident {
  pub name: String,
  pub span: crate::span::SrcPos,
}

/// Represents a definition in the source code, it does map to a [`Ident`], that's the
/// pretty same thing.
#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct Defn {
  pub name: Ident,
}

/// Represents a signature in the source code, it's a type that can be used to type
/// check a definition.
#[derive(Debug, Clone)]
pub struct Signature {
  pub name: Defn,                  // name of the signature
  pub type_repr: Option<Box<Exp>>, // type representation of the signature
  pub value: Option<Box<Exp>>,     // value representation of the signature
  pub span: crate::span::SrcPos,   // span of the signature, it's the span of the name of the signature
}

/// Expression base type, it is a tree structure that represents a concrete term
/// in the source code.
///
/// The [`Exp`] type is the core of the language.
#[derive(Debug, Clone)]
pub enum Exp {
  // SECTION: Core expressions
  /// Type universe:
  ///
  /// ```rust,ignore
  /// "Type"
  /// ```
  Uni,
  /// Shifts the expression to type level to be used as a type repr in implicit applications
  ///
  /// ```rust,ignore
  /// "@" expr
  /// ```
  Shift(Box<Exp>),
  Int(i64),
  Var(Ident),
  BinOp(Box<Exp>, crate::tok::BinOp, Box<Exp>),
  Let(Defn, Box<Exp>, Box<Exp>),
  Eval(Box<Exp>),
  Type(Box<Exp>),
  Lam(Box<Exp>, Box<Exp>),
  Pi(Box<Exp>, Box<Exp>),
  Appl(Box<Exp>, Box<Exp>),
  Signature(Signature),

  /// SECTION: Literal expressions, that represents one-to-one with the source code relationship
  Vec(crate::tok::Sep, Vec<Exp>),
  Delim(crate::tok::Delim, Box<Exp>),
  SrcPos(crate::span::SrcPos, Box<Exp>),
}

