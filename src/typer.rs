use std::rc::Rc;
use std::cell::{Cell, RefCell};

pub use debruijin::*;
pub use metas::*;
pub use TypeKind::*;

use crate::ast::{Expression, Term};
use crate::src::Identifier;
use crate::src::Implicitness;
use crate::src::Span;
use crate::ZureDb;

/// Type constructors are the constructors of the type system. They are used to
/// represent the types of the language.
pub struct Constr {}

/// A spine is a list of values that are applied to a function.
pub type Spine = Vec<Type>;

/// Environment of a function. It contains the level of the function, and the
/// stack of values that are applied to the function.
///
/// The stack is used with debruijin indexes.
#[derive(Debug, Clone)]
pub struct Env {
  pub lvl: Lvl,
  pub stack: Vec<Type>,
}

/// It's a lazy function, it's a closure that contains the environment and the
/// quoted value.
///
/// In this case, the HIR is the quoted value, and the environment is defined here
#[derive(Debug, Clone)]
pub struct Closure {
  pub env: Env,
  pub value: Term,
}

/// Value kind of the language. It's the base of the types, and it's the
/// representation of the values in the language.
#[derive(Debug, Clone)]
pub enum TypeKind {
  Constr(Rc<Constr>),
  Rigid(Lvl, Spine),
  Flex(Hole, Spine),
  Fun(Identifier, Implicitness, Closure),
  Pi(Option<Identifier>, Implicitness, Type, Closure),
}

/// The elaborated value, it's the value with the span position.
#[derive(Debug, Clone)]
pub struct Type(pub Rc<TypeKind>, Span);

impl Type {
  /// Creates a new value with the given kind and span.
  pub fn new(span: Span, kind: TypeKind) -> Self {
    Self(kind.into(), span)
  }

  /// Borrows the value cloning the [`crate::span::SrcPos`] span position
  /// of the value.
  pub fn span(&self) -> Span {
    self.1.clone()
  }
}

/// Constructors, like `::rigid` and `::flex`.
impl Type {
  /// Creates a new rigid type variable that is applied to the given spine.
  ///
  /// # Parameters
  ///
  /// - `lvl`   - The level of the type variable
  /// - `spine` - The spine of the type variable
  pub fn rigid(lvl: Lvl, spine: Spine) -> Self {
    Self::new(Span::default(), Rigid(lvl, spine))
  }

  /// Creates a new rigid type variable that doesn't have a spine applied to it.
  ///
  /// # Parameters
  ///
  /// - `lvl`   - The level of the type variable
  pub fn stuck(lvl: Lvl) -> Self {
    Self::new(Span::default(), Rigid(lvl, Default::default()))
  }

  /// Creates a new flexible type variable that is applied to the given spine.
  ///
  /// # Parameters
  ///
  /// - `hole`  - The hole of the type variable
  /// - `spine` - The spine of the type variable
  pub fn flex<I: Into<Hole>>(hole: I, spine: Spine) -> Self {
    Self::new(Span::default(), Flex(hole.into(), spine))
  }

  /// Creates a new flexible type variable that doesn't have a spine
  /// applied to it.
  ///
  /// # Parameters
  ///
  /// - `hole` - The hole of the type variable
  pub fn var<I: Into<Hole>>(hole: I) -> Self {
    Self::new(Span::default(), Flex(hole.into(), Default::default()))
  }
}

/// Debruijin indexes and levels. It's used for type checker's efficient indexing
/// and substitution.
mod debruijin {
  /// Defines a debruijin level. It does represent the level of the context/environment
  ///
  /// It can be transformed into a debruijin index by using the [`Lvl::as_ix`] method.
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  pub struct Lvl(usize);

  impl Lvl {
    /// Transforms a level into a debruijin index.
    pub fn as_ix(&self, Lvl(x): Lvl) -> Idx {
      let Lvl(l) = *self;
      assert!(l > x, "l > x, but {l} < {x}");
      assert!(l > 0, "l should be greater than 0");

      Idx(l - x - 1)
    }
  }

  impl std::ops::Add<usize> for Lvl {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
      Self(self.0 + rhs)
    }
  }

  impl std::ops::AddAssign<usize> for Lvl {
    fn add_assign(&mut self, rhs: usize) {
      self.0 += rhs
    }
  }

  /// Defines a debruijin index. That can be converted by two levels.
  ///
  /// It's used to represent a variable in the syntax tree.
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  pub struct Idx(usize);

  impl std::ops::Add<usize> for Idx {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
      Self(self.0 + rhs)
    }
  }

  impl std::ops::AddAssign<usize> for Idx {
    fn add_assign(&mut self, rhs: usize) {
      self.0 += rhs
    }
  }
}

/// Meta variables stuff, like holes and meta
mod metas {
  use super::*;

  #[derive(Debug, Clone)]
  pub enum Meta {
    Empty(Lvl),
    Filled(Type),
  }

  #[derive(Debug, Clone)]
  pub struct Hole {
    name: String,
    value: Rc<RefCell<Meta>>,
  }

  impl Hole {
    /// Creates a new empty hole with the given name and level.
    ///
    /// The level is the level of the context where the hole was created.
    pub fn empty(name: String, lvl: Lvl) -> Self {
      Self {
        name,
        value: Rc::new(RefCell::new(Meta::Empty(lvl))),
      }
    }

    /// Creates a new filled hole with the given name and value.
    pub fn new(name: String, value: Type) -> Self {
      Self {
        name,
        value: Rc::new(RefCell::new(Meta::Filled(value))),
      }
    }

    /// Borrows the name of the hole.
    pub fn name(&self) -> &str {
      &self.name
    }

    /// Takes and clones the value of the hole.
    ///
    /// NOTE: It's useful to avoid runtime borrow errors
    pub fn lookup(&self) -> Meta {
      self.value.borrow().clone()
    }

    /// Returns if the hole is empty.
    pub fn is_empty(&self) -> bool {
      matches!(&*self.value.borrow(), Meta::Empty(_))
    }
  }

  impl PartialEq for Hole {
    fn eq(&self, other: &Self) -> bool {
      Rc::ptr_eq(&self.value, &other.value)
    }
  }
}

// SECTION: Type check
#[derive(Debug, Clone)]
pub struct Ctx<'db> {
  pub db: &'db dyn ZureDb,
  pub env: Env,
  pub types: Vec<(String, Type)>,
  pub position: RefCell<Span>,
  pub unique: Cell<usize>
}

impl<'db> Ctx<'db> {
  /// Creates a new environment with the given level and stack.
  pub fn create_value(&self, value: Type) -> Ctx {
    let mut ctx = self.clone();
    ctx.env.lvl += 1;
    ctx.env.stack.push(value);
    ctx
  }
}

/// Evaluates a value to get the type of the value. It does elaborate the
/// expression to get an elaborated value.
pub fn eval(ctx: &Ctx, value: Term) -> Type {
  use Expression::*;

  match value.data(ctx.db) {
    Match(_) => todo!(),
    Tuple(_) => todo!(),
    Raise(_) => todo!(),
    Text(_) => todo!(),
    Appl(_) => todo!(),
    Anno(_) => todo!(),
    Int(_) => todo!(),
    Var(_) => todo!(),
    Fun(_) => todo!(),
    Let(_) => todo!(),
    Idx(_) => todo!(),
    Universe => todo!(),
    Pi(_) => todo!(),
  }
}

/// Applies a closure with a value to get the result of the application.
///
/// # Parameters
///
/// - `ctx`    - The context of the application
/// - `callee` - The closure to apply
/// - `value`  - The value to apply to the closure
pub fn apply(ctx: &Ctx, callee: Closure, value: Type) -> Type {
  eval(&ctx.env.create_value(value.clone()), callee.value)
}