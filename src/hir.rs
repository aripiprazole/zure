use crate::ast::Icit;

/// It does represent a term in the source code, it's a term that can be used to type check
/// a definition.
#[derive(Debug, Clone)]
pub enum Term {
  /// Type level representation, that doesn't need a type
  TypeRepr(crate::val::Val),
  /// A value that would be run in run-time, it's a value that can be used in the source code, and
  /// it does need a type
  Exp { value: Box<Exp>, type_repr: Box<Term> },
}

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
  pub name: Defn, // name of the signature

  /// Here the type isn't optional, if it's not present, then it's a
  /// hole to be filled by the type checker.
  pub type_repr: Term, // type representation of the signature

  /// The value can be still optional, if it's not present, then it's a
  /// [`None`] value, because it can refers to FFI, or something like that.
  pub value: Option<Term>, // value representation of the signature

  /// The span of the signature, it's the span
  pub span: crate::span::SrcPos, // span of the signature
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
  Shift(Term),
  /// Application expression, it's the core of the language, it's the only way to build
  /// a term in the language.
  ///
  /// Here we don't need an [`Icit`] because it's it's implicit, only if a variant is instance of
  /// [`Exp::Shift`], if it's shifted, then it's implicit, but if it's not shifted, then the default
  /// case we assume, it's an explicit binding.
  Appl(Term, Term),
  /// Integer literal values, it's a literal value that can be used in the source code.
  /// Not a secret
  Int(i64),
  /// Variable reference, it's a reference to a definition in the source code.
  Var(Ident),
  /// Binary operation, it's a binary operation that can be used within 2 expressions, and some
  /// kind of binary operations can be used in the source code.
  BinOp(Term, crate::tok::BinOp, Term),
  /// Let-binding, they are used to define a definition in the source code. It's a way to define
  /// a definition in the source code with functional programming.
  Let(Defn, Term, Term),
  // SECTION: Function values, they are used to define a function in the source code.
  Pi(Defn, Icit, Term, Term),
  Lam(Defn, Icit, Term, Term),
  /// SECTION: Literal expressions, that represents one-to-one with the source code relationship
  SrcPos(crate::span::SrcPos, Box<Exp>),
}

/// Top level is statement that can be used to define a definition in the source code. Or run code, like
/// in `#eval`, `#type` and defining global functions.
#[derive(Debug, Clone)]
pub enum TopLevel {
  Signature(Signature),
  Eval(Term),
  Type(Term),
}
