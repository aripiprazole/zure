//! Equality properties between two types. It does a deep comparison, and close
//! holes between the two types, if possible, using pattern unification.
//!
//! The main part of unification is comparing two types, and returning a result.

use crate::ast::Icit;
use crate::nfe::Nfe;
use crate::show_nfe_of;
use crate::val::Lvl;
use crate::val::Spine;
use crate::val::Val;
use crate::val::ValKind;

#[derive(miette::Diagnostic, thiserror::Error, Debug, Clone, PartialEq)]
#[diagnostic()]
pub enum UnifyError {
  /// Unification error between two values
  #[error("expected value: `{0}`, got the value: `{1}`")]
  #[diagnostic(url(docsrs), code(unify::cant_unify))]
  MismatchBetweenValues(Nfe, Nfe),

  /// Icit mismatch between two values,
  #[error("expected a value with icit: {0}, and got: {1}")]
  #[diagnostic(url(docsrs), code(unify::icit_mismatch))]
  IcitMismatch(Icit, Icit),

  /// Unification error between two types
  #[error("expected type: `{0}`, got the type: `{1}`")]
  #[diagnostic(url(docsrs), code(unify::cant_unify))]
  CantUnify(Nfe, Nfe),
}

impl Val {
  /// Forcing is an important part of the type checker, it does
  /// removes the holes created by the elaborator.
  ///
  /// It does returns a value without holes.
  pub fn force(self) -> ValKind {
    use crate::val::Meta::*;

    match &*self.0 {
      kind @ ValKind::Flex(ref m, ref sp) => match m.lookup() {
        Empty(_) => kind.clone(),
        Filled(value) => value.force(),
      },
      otherwise => otherwise.clone(),
    }
  }

  /// Performs unification between two values, its a
  /// equality relation between two values.
  ///
  /// It does closes some holes.
  ///
  /// # Parameters
  ///
  /// - `self` - The left hand side of the unification
  /// - `rhs`  - The right hand side of the unification
  /// - `ctx`  - The context where the unification is happening
  ///            right now
  ///
  /// # Returns
  ///
  /// It does returns an error if the unification fails. And a
  /// unit if the unification succeeds.
  ///
  /// # Another stuff
  ///
  /// NOTE: I disabled the formatter so I can align values
  /// and it looks cutier.
  #[rustfmt::skip]
  pub fn unify(self, rhs: Val, lvl: Lvl) -> Result<(), UnifyError> {
    /// Imports every stuff so we can't have a lot of
    /// `::` in the code blowing or mind.
    use crate::val::ValKind::*;
    use crate::val::Val as Type;
    use UnifyError::*;

    /// Unifies a spine of applications, it does unifies two list of applications
    /// that are spines.
    /// 
    /// It requires that the spines have the same length, and it does unifies
    /// the spines.
    fn unify_sp(sp_a: Spine, sp_b: Spine, lvl: Lvl) -> Result<(), UnifyError> {
      assert!(sp_a.len() == sp_b.len(), "spines must have the same length");

      for (u_a, u_b) in sp_a.into_iter().zip(sp_b) {
        u_a.unify(u_b, lvl)?;
      }

      Ok(())
    }

    // Forcing here is important because it does removes the holes created
    // by the elaborator, and it does returns a value without holes.
    //
    // It's important to do this because we don't want to unify holes
    // with values, because it will cause a lot of problems, and it will
    // increase the pattern matching complexity.
    match (self.force(), rhs.force()) {
      // Unification of literal values, it does checks if the values are equal
      // directly. If they are not, it does returns an error.
      (Constr(v_a)             , Constr(v_b)) if v_a == v_b => Ok(()), // 1 = 1, 2 = 2, etc...
      (Constr(v_a)             , Constr(v_b))               => Err(MismatchBetweenValues(show_nfe_of!(v_a), show_nfe_of!(v_b)))?,

      // Unification of application spines or meta variables, it does unifies
      // flexibles, rigids and meta variable's spines.
      //
      // It does unifies the spines of the applications.
      (Flex(m_a, sp_a)         ,     Flex(m_b, sp_b)) if m_a == m_b => unify_sp(sp_a, sp_b, lvl),
      (   Rigid(m_a, sp_a)     ,    Rigid(m_b, sp_b)) if m_a == m_b => unify_sp(sp_a, sp_b, lvl),

      // Lambda unification, that applies closures and pi types
      // using the spine of applications.
      //
      // It does unifies the closures and the pi types.
      (Lam(_, i_a, v_a)        ,            Lam(_, i_b, v_b)) if i_a == i_b => {
        v_a.apply(Type::stuck(lvl))
           .unify(v_b.apply(Type::stuck(lvl)), lvl + 1)
      }
      (Lam(_, _, v_a)           ,                              tt) => {
        v_a.apply(Type::stuck(lvl))
           .unify(tt.apply(Type::stuck(lvl)), lvl + 1)
      }
      (tt                               ,            Lam(_, _, v_b)) => {
        tt.apply(Type::stuck(lvl))
          .unify(v_b.apply(Type::stuck(lvl)), lvl + 1)
      }

      // Pi type unification, it does unifies the domain and the codomain
      // of the pi types.
      //
      // NOTE: cod stands for codomain, and dom stands for domain.
      (ref lhs @ Pi(_, i_a, ..), ref rhs @ Pi(_, i_b, ..)) if i_a != i_b => Err(IcitMismatch(i_a, i_b)),
      (Pi(_, _, d_a, c_a)      ,       Pi(_, _, d_b, c_b))               => {
        d_a.unify(d_b, lvl)?;
        c_a.apply(Type::stuck(lvl))
           .unify(c_b.apply(Type::stuck(lvl)), lvl + 1)
      }

      // Unification of meta variables, it does unifies meta variables that
      // are present in the context.
      //
      // It does require a solver function.
      //
      // TODO: Solve
      (Flex(m, sp), t) | (t, Flex(m, sp)) => t.solve(sp, m, ctx.lvl),

      // Fallback case which will cause an error if we can't unify
      // the values.
      //
      // It's the fallback of the fallbacks cases, the last error message
      // and the least meaningful.
      //
      // Debug locations for types, its useful to display better error messages
      // to the final user of the language.
      //
      // Like if the type is hand-written, it will display the location of the
      // type in the source code.
      (lhs, rhs) => Err(CantUnify(show_nfe_of!(lhs), show_nfe_of!(rhs)))?,
    }
  }
}
