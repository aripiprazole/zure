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

/// Represents the implicitness of an application.
#[derive(Debug, Clone)]
#[repr(u8)]
pub enum Icit {
  Expl,
  Impl,
}

/// Represents a signature in the source code, it's a type that can be used to type
/// check a definition.
#[derive(Debug, Clone)]
pub struct Signature {
  pub name: Defn, // name of the signature

  /// Here the type isn't optional, if it's not present, then it's a
  /// hole to be filled by the type checker.
  pub type_repr: Box<Exp>, // type representation of the signature

  /// The value can be still optional, if it's not present, then it's a
  /// [`None`] value, because it can refers to FFI, or something like that.
  pub value: Option<Box<Exp>>, // value representation of the signature

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
  Shift(Box<Exp>),
  /// Application expression, it's the core of the language, it's the only way to build
  /// a term in the language.
  ///
  /// Here we don't need an [`Icit`] because it's it's implicit, only if a variant is instance of
  /// [`Exp::Shift`], if it's shifted, then it's implicit, but if it's not shifted, then the default
  /// case we assume, it's an explicit binding.
  Appl(Box<Exp>, Box<Exp>),

  /// Integer literal values, it's a literal value that can be used in the source code.
  /// Not a secret
  Int(i64),

  /// Variable reference, it's a reference to a definition in the source code.
  Var(Ident),

  /// Binary operation, it's a binary operation that can be used within 2 expressions, and some
  /// kind of binary operations can be used in the source code.
  BinOp(Box<Exp>, crate::tok::BinOp, Box<Exp>),

  /// Let-binding, they are used to define a definition in the source code. It's a way to define
  /// a definition in the source code with functional programming.
  Let(Defn, Box<Exp>, Box<Exp>),

  // SECTION: Function values, they are used to define a function in the source code.
  Pi(Defn, Icit, Box<Exp>, Box<Exp>),
  Lam(Defn, Icit, Box<Exp>, Box<Exp>),

  /// SECTION: Literal expressions, that represents one-to-one with the source code relationship
  SrcPos(crate::span::SrcPos, Box<Exp>),
}

/// Top level is statement that can be used to define a definition in the source code. Or run code, like
/// in `#eval`, `#type` and defining global functions.
#[derive(Debug, Clone)]
pub enum TopLevel {
  Signature(Signature),
  Eval(Box<Exp>),
  Type(Box<Exp>),
}
