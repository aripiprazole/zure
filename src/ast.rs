use crate::src::Identifier;
use crate::src::Implicitness;
use crate::src::ModuleId;
use crate::src::Span;

/// Module file. It does tracks the imports, and the path of the file, for
/// incremental computing, and caching the module and build system.
///
/// It does contains the source code of the module, and the imports.
#[salsa::tracked]
pub struct Module {
  pub path: File,

  #[return_ref]
  pub imports: Vec<Import>,

  #[return_ref]
  pub top_levels: Vec<TopLevel>,
}

/// File is a source file. It does contains the source code of the file, and
/// the path of the file, for incremental computing, and caching the module and
/// build system.
#[salsa::input]
pub struct File {
  #[id]
  pub id: ModuleId,

  #[return_ref]
  pub contents: String,
}

/// Imports are references to another files. They are used to track the
/// dependencies of the module.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Import {
  pub path: String,
  pub span: Option<Span>,
}

/// Inductive variant. It does contains the type representation of the variant,
/// and the parameters of the variant.
#[salsa::tracked]
pub struct Variant {
  #[id]
  pub id: Identifier,

  /// GADT type representation
  pub type_repr: Option<Term>,
  pub parameters: Vec<Term>,
}

/// A variable declaration in a let expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetDeclaration {
  pub name: Identifier,
  pub parameters: Vec<Parameter>,
  pub value: Term,
}

/// A type declaration in a let expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDeclaration {
  pub id: Identifier,
  pub type_repr: Term,
  pub parameters: Vec<Parameter>,
  pub declarations: Vec<Variant>,
  pub value: Term,
}

/// A variable declaration in a let expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValDeclaration {
  pub id: Identifier,
  pub type_repr: Term,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Declaration {
  Let(LetDeclaration),
  Val(ValDeclaration),
  Type(TypeDeclaration),
}

/// Parameter for types or let declarations.
#[salsa::input]
pub struct Parameter {
  #[return_ref]
  pub text: String,
  pub implicitness: Implicitness,
  pub type_repr: Option<Term>,
  pub span: Span,
}

/// Top level declarations. These are top level statements that declares a
/// thing in the code, like a function, or a type.
#[salsa::tracked]
pub struct TopLevel {
  #[id]
  pub id: Identifier,
  pub declaration: Declaration,
  pub span: Span,
}

/// Expression is the base of the language. It does contains the type
/// representation of the expression, and the parameters of the expression.
#[salsa::tracked]
pub struct Term {
  pub span: Span,
  pub data: Expression,
}

/// A module opening in a let expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetOpen {
  pub module_id: ModuleId,
  pub span: Span,
}

/// A variable binding in a let expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LetBinding {
  Variable(LetDeclaration),
  Open(LetOpen),
}

/// Expression data type that implements polymorphism for expressions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
  Match(Match),
  Tuple(Tuple),
  Raise(Raise),
  Text(String),
  Appl(Appl),
  Anno(Anno),
  Int(isize),
  Idn(Identifier),
  Fun(Fun),
  Let(Let),
  Idx(crate::typer::Idx),

  // SECTION: Types
  /// The universe type, *-type, or the type of types.
  ///
  /// It's the type of all types, and it's the type of the universe itself.
  Universe,

  /// Pi type is a dependent type, it's a function type where the codomain
  /// depends on the domain.
  Pi(Pi),
}

#[salsa::tracked]
pub struct Pattern {
  pub span: Span,
  pub data: PatternData,
}

/// The kind of a pattern, it's used to match a value against a pattern.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatternData {
  /// A variable pattern, it's a pattern that matches any value, and binds
  /// the value to a variable.
  Var(Identifier),

  /// A tuple pattern, it's a pattern that matches a tuple value, and binds
  /// the values to variables.
  Tuple(Vec<Pattern>),

  /// A wildcard pattern, it's a pattern that matches any value, and doesn't
  /// bind the value to a variable.
  Wildcard,

  /// A constructor pattern, it's a pattern that matches a constructor value,
  /// and binds the values to variables.
  Constructor(Identifier, Vec<Pattern>),
}

/// A case in the pattern matching
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Case {
  pub pattern: Pattern,
  pub value: Term,
}

/// Pattern matching expression, it's an expression that matches a value
/// against a list of cases.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Match {
  pub value: Term,
  pub cases: Vec<Case>,
}

/// A tuple that can be used as a type, or a value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tuple {
  pub terms: Vec<Term>,

  /// If it's a type repr, like `a ** b`, or if it's a value, like `(a, b)`.
  pub is_type_level: bool,
}

/// Abstraction expression, it's a function that doesn't have a name that runs
/// anonymously a function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fun {
  pub parameter: Parameter,
  pub value: Term,
}

/// Let binding expression, it's a simple expression that binds a variable
/// to a value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Let {
  pub binding: LetBinding,
  pub next: Term,
}

/// Raises an exception to be catch by the exception handler. It's used for
/// error handling.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Raise {
  pub exception: Term,
}

/// Type annotation expression, it's an expression that has a type annotation
/// in it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Anno {
  pub term: Term,
  pub type_repr: Term,
}

/// Pi type is a dependent type, it's a function type where the codomain
/// depends on the domain.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pi {
  pub domain: Parameter,
  pub implicitness: Implicitness,
  pub codomain: Term,
}

/// Application of a function to a list of arguments, it's called spine
/// where the function is the callee, and the arguments are the spine.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Appl {
  pub callee: Term,
  pub value: Term,
}
