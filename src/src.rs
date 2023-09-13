use std::path::PathBuf;

/// The span of a node in the source code. It's used for error reporting.
///
/// It can be either incremental or atomic
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Span {
  Atomic(usize),
  Relative(usize),
}

/// Module file. It does tracks the imports, and the path of the file, for
/// incremental computing, and caching the module and build system.
///
/// It does contains the source code of the module, and the imports.
#[salsa::tracked]
pub struct Module {
  pub path: File,

  #[return_ref]
  pub imports: Vec<Import>,
}

/// The identifier of a module. It does contains the path of the module, and
/// the name of the module.
#[salsa::input]
pub struct ModuleId {
  #[return_ref]
  pub text: String,

  #[return_ref]
  pub path: PathBuf,
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
  pub id: FunctionId,

  /// GADT type representation
  pub type_repr: Option<Term>,
  pub parameters: Vec<Term>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Declaration {
  Inductive(Vec<Variant>),
  Value(Term),
  Signature,
}

/// The functional id that are applied for every function, inductive, types,
/// or anything that is inside a module.
#[salsa::input]
pub struct FunctionId {
  #[return_ref]
  pub text: String,
  pub module: ModuleId,
  pub span: Span,
}

/// Parameter for types or let declarations.
#[salsa::input]
pub struct Parameter {
  #[return_ref]
  pub text: String,
  pub type_repr: Option<Expression>,
  pub span: Span,
}

/// Top level declarations. These are top level statements that declares a
/// thing in the code, like a function, or a type.
#[salsa::tracked]
pub struct TopLevel {
  #[id]
  pub id: FunctionId,
  pub type_repr: Term,
  pub parameters: Vec<Parameter>,
  pub declaration: Declaration,
  pub span: Span,
}

/// Expression is the base of the language. It does contains the type
/// representation of the expression, and the parameters of the expression.
#[salsa::tracked]
pub struct Term {
  pub data: Expression,
  pub span: Span,
}

/// A variable declaration in a let expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetVariable {
  pub id: FunctionId,
  pub type_repr: Term,
  pub parameters: Vec<Parameter>,
  pub declaration: Expression,
  pub span: Span,
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
  Variable(LetVariable),
  Open(LetOpen),
}

/// The implicitness of a parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Implicitness {
  Explicit,
  Implicit,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
  Int(isize),
  Txt(String),
  Var(FunctionId),
  App(App),
  Lam(Lam),
  Let(Let),
  Pi(Pi),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lam {
  pub parameters: Vec<Parameter>,
  pub value: Term,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Let {
  pub bindings: Vec<LetBinding>,
  pub next: Term,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pi {
  pub domain: Parameter,
  pub icit: Implicitness,
  pub codomain: Term,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct App {
  pub callee: Term,
  pub spine: Vec<Term>,
}
