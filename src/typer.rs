use std::cell::Cell;
use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;

pub use debruijin::*;
pub use metas::*;
pub use TypeKind::*;

pub use self::Constr::*;
use crate::ast::Case;
use crate::ast::Expression;
use crate::ast::Parameter;
use crate::ast::Pattern;
use crate::ast::Term;
use crate::src::Identifier;
use crate::src::Implicitness;
use crate::src::Span;
use crate::ZureDb;

/// Type constructors are the constructors of the type system. They are used to
/// represent the types of the language.
#[derive(Debug, Clone)]
pub enum Constr {
  Any,
  Uni,
  Int,
  Str,
}

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
  /// Synthesize a new value with the given kind.
  pub fn synthesize(kind: TypeKind) -> Self {
    Self::new(Span::default(), kind)
  }

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
  #[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
  pub struct IdxMeta {
    /// Debug assertion name
    #[cfg(debug_assertions)]
    pub(crate) name: &'static str,
  }

  impl IdxMeta {
    /// Creates a new meta with the given name.
    pub fn new(name: String) -> Self {
      Self {
        #[cfg(debug_assertions)]
        name: Box::leak(name.into_boxed_str()),
      }
    }
  }

  /// Defines a debruijin level. It does represent the level of the context/environment
  ///
  /// It can be transformed into a debruijin index by using the [`Lvl::as_ix`] method.
  #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
  pub struct Lvl(pub usize);

  impl Lvl {
    /// Transforms a level into a debruijin index.
    pub fn as_ix(&self, Lvl(x): Lvl) -> Idx {
      let Lvl(l) = *self;
      assert!(l > x, "l > x, but {l} < {x}");
      assert!(l > 0, "l should be greater than 0");

      Idx(l - x - 1, Default::default())
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
  #[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
  pub struct Idx(usize, IdxMeta);

  impl Idx {
    /// Creates a new index with meta
    pub fn new(meta: IdxMeta) -> Self {
      Self(0, meta)
    }
  }

  impl From<usize> for Idx {
    fn from(value: usize) -> Self {
      Self(value, Default::default())
    }
  }

  impl std::ops::Add<usize> for Idx {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
      Self(self.0 + rhs, self.1)
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
pub struct Ctx {
  pub env: Env,
  pub types: VecDeque<(String, Type)>,
  pub position: RefCell<Span>,
  pub unique: Cell<usize>,
}

impl Env {
  /// Creates a new environment with the given level and stack.
  pub fn create_value(&self, value: Type) -> Env {
    let mut ctx = self.clone();
    ctx.stack.push(value);
    ctx
  }
}

impl Ctx {
  /// Increases the level of the context.
  pub fn lift_lvl(&self) -> Ctx {
    let mut ctx = self.clone();
    ctx.env.lvl += 1;
    ctx
  }

  /// Creates a new environment with the given level and stack.
  pub fn create_value(&self, value: Type) -> Ctx {
    let mut ctx = self.clone();
    ctx.env.lvl += 1;
    ctx.env.stack.push(value.clone());
    ctx.types.push_front((self.fresh_name(), value));
    ctx
  }

  /// Creates a new environment with the given level and stack.
  pub fn fresh_name(&self) -> String {
    let name = format!("t{}", self.unique.get());
    self.unique.set(self.unique.get() + 1);
    name
  }
}

/// Evaluates a value to get the type of the value. It does elaborate the
/// expression to get an elaborated value.
pub fn eval(db: &dyn ZureDb, env: &Env, value: Term) -> Type {
  use Expression::*;

  let _ = env;
  match value.data(db) {
    Match(_) => todo!(),
    Tuple(_) => todo!(),
    Raise(_) => todo!(),
    Text(_) => todo!(),
    Appl(_) => todo!(),
    Anno(_) => todo!(),
    Int(_) => todo!(),
    Idn(_) => todo!(),
    Fun(_) => todo!(),
    Let(_) => todo!(),
    Idx(_) => todo!(),
    Universe => todo!(),
    Pi(_) => todo!(),
  }
}

fn resolve_pattern(db: &dyn ZureDb, ctx: &Ctx, case: crate::src::Term) -> Pattern {
  todo!()
}

fn resolve_binding(db: &dyn ZureDb, ctx: &Ctx, binding: crate::src::LetBinding) -> crate::ast::LetBinding {
  todo!()
}

fn resolve_case(db: &dyn ZureDb, ctx: &Ctx, case: crate::src::Case) -> Case {
  Case {
    pattern: resolve_pattern(db, ctx, case.pattern),
    value: resolve(db, ctx, case.value),
  }
}

/// Performs resolution and elaboration of terms to get the type of the term.
///
/// # Parameters
///
/// - `db`    - The incremental database of the application
/// - `ctx`   - The context of the application
/// - `value` - The value to resolve
pub fn resolve(db: &dyn ZureDb, ctx: &Ctx, value: crate::src::Term) -> Term {
  use crate::src::Expression::*;

  Term::new(db, value.span(db), match value.data(db) {
    Universe => Expression::Universe,
    Var(var) => {
      let text = var.text(db).clone();
      let mut types = ctx.types.clone();
      let mut idx = Idx::new(IdxMeta::new(text.clone()));
      while let Some((name, _)) = types.pop_front() {
        if &name == var.text(db) {
          return Term::new(db, value.span(db), Expression::Idx(idx));
        }

        idx += 1;
      }

      panic!("variable not found")
    }
    Text(text) => Expression::Text(text.clone()),
    Int(int) => Expression::Int(int),
    Tuple(tuple) => Expression::Tuple(crate::ast::Tuple {
      terms: tuple.terms.into_iter().map(|term| resolve(db, ctx, term)).collect(),
      is_type_level: tuple.is_type_level,
    }),
    Raise(raise) => Expression::Raise(crate::ast::Raise {
      exception: resolve(db, ctx, raise.exception),
    }),
    Let(expr) => Expression::Let(crate::ast::Let {
      binding: resolve_binding(db, ctx, expr.binding),
      next: resolve(db, &ctx.create_value(Type::stuck(ctx.env.lvl + 1)), expr.next),
    }),
    Anno(anno) => Expression::Anno(crate::ast::Anno {
      term: resolve(db, ctx, anno.term),
      type_repr: resolve(db, ctx, anno.type_repr),
    }),
    Match(expr) => Expression::Match(crate::ast::Match {
      value: resolve(db, ctx, expr.value),
      cases: expr.cases.into_iter().map(|case| resolve_case(db, ctx, case)).collect(),
    }),
    Pi(pi) => Expression::Pi(crate::ast::Pi {
      domain: Parameter::new(
        db,
        /* text         = */ pi.domain.text(db).clone(),
        /* implicitness = */ pi.domain.implicitness(db),
        /* type_repr    = */ pi.domain.type_repr(db).map(|term| resolve(db, ctx, term)),
        /* span         = */ pi.domain.span(db),
      ),
      implicitness: pi.implicitness,
      codomain: resolve(db, &ctx.create_value(Type::stuck(ctx.env.lvl + 1)), pi.codomain),
    }),
    // Resolves the applications of a function. It does resolve the callee,
    // and transform the spine of application into a bunch of curried calls.
    Appl(appl) => {
      // Applies the spine of applications into one each application
      // to be easier to deal with currying in the elaboration and
      // infer/checking phases.
      return appl
        .spine
        .into_iter()
        .fold(resolve(db, ctx, appl.callee), |callee, value| {
          Term::new(
            db,
            value.span(db),
            Expression::Appl(crate::ast::Appl {
              callee,
              value: resolve(db, ctx, value),
            }),
          )
        });
    }
    // Maps an uncurried function call into a curried function call resolving
    // the parameters and the value.
    Fun(fun) => {
      // Creates a local context to accumulate the level of the parameters, which will
      // be increased by the occurrence of a parameter.
      let mut local_ctx = ctx.clone();
      let span = value.span(db);
      return fun
        .parameters
        .into_iter()
        .map(|parameter| {
          local_ctx = local_ctx.clone().create_value(Type::stuck(local_ctx.env.lvl + 1));

          Parameter::new(
            db,
            /* text         = */ parameter.text(db).clone(),
            /* implicitness = */ parameter.implicitness(db),
            /* type_repr    = */ parameter.type_repr(db).map(|term| resolve(db, &local_ctx, term)),
            /* span         = */ parameter.span(db),
          )
        })
        .collect::<Vec<_>>()
        .into_iter()
        // Folds to generate one curried function with all the parameters
        // applied to the value.
        .fold(resolve(db, &local_ctx, fun.value), |value, parameter| {
          Term::new(db, span.clone(), Expression::Fun(crate::ast::Fun { parameter, value }))
        });
    }
    // Generates a lambda applying a pattern matching against it
    // it's like the \case extension in haskell, or even the function
    // keyword in OCaml.
    Function(function) => {
      // `function | Some a -> a | None -> panic`, turns into `fun a -> match a ...`
      let span = value.span(db);
      let name = ctx.fresh_name();
      let parameter = Identifier::new(db, name.clone(), None, span.clone());

      Expression::Fun(crate::ast::Fun {
        parameter: Parameter::new(db, name, Implicitness::Explicit, None, span.clone()),
        value: Term::new(
          db,
          span.clone(),
          Expression::Match(crate::ast::Match {
            value: Term::new(db, span.clone(), Expression::Idn(parameter)),
            cases: function
              .cases
              .into_iter()
              .map(|case| resolve_case(db, ctx, case))
              .collect(),
          }),
        ),
      })
    }
    Error(_) => todo!(),
    Group(_) => todo!(),
  })
}

/// Infers the type for a term. It does resolve the term, and then elaborates
/// the term to get the type of the term.
///
/// # Parameters
///
/// - `db`    - The incremental database of the application
/// - `ctx`   - The context of the application
/// - `value` - The value to infer
pub fn infer(db: &dyn ZureDb, ctx: &Ctx, value: &Term) -> Type {
  let _ = db;
  let _ = ctx;
  let _ = value;
  Type::synthesize(Constr(Rc::new(Any)))
}

/// Checks the type of a term. It does resolve the term, and then elaborates
/// checking a term against a type.
///
/// # Parameters
///
/// - `db`        - The incremental database of the application
/// - `ctx`       - The context of the application
/// - `value`     - The value to check
/// - `type_repr` - The type to check against
pub fn check(db: &dyn ZureDb, ctx: &Ctx, value: Term, type_repr: &Type) -> (Term, Type) {
  let _ = db;
  let _ = ctx;
  let _ = type_repr;
  (value, Type::synthesize(Constr(Rc::new(Any))))
}

/// Applies a closure with a value to get the result of the application.
///
/// # Parameters
///
/// - `db`     - The incremental database of the application
/// - `env`    - The environment of the application
/// - `callee` - The closure to apply
/// - `value`  - The value to apply to the closure
pub fn apply(db: &dyn ZureDb, env: &Env, callee: Closure, value: Type) -> Type {
  eval(db, &env.create_value(value.clone()), callee.value)
}
