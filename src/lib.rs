//! The main file of the project. It contains the library functions, and it's
//! used by the binary crate, which is the main entry point of the program.
//!
//! It contains the code that can be used as library, like lex, parser, typing, codegen,
//! evaluation.
//!
//! The configurations that are applied to CLI are done by the `main.rs` binary crate.

use vfs::ModuleLoader;

/// The local were the incremental components are found. It's used by the
/// `salsa` crate to incremental computing.
#[salsa::jar(db = ZureDb)]
pub struct Jar(
  parser::parse,
  src::Module,
  src::ModuleId,
  src::File,
  src::TopLevel,
  src::Term,
  src::FunctionId,
  src::Variant,
  src::Parameter,
  ast::Module,
  ast::ModuleId,
  ast::File,
  ast::TopLevel,
  ast::Term,
  ast::FunctionId,
  ast::Variant,
  ast::Parameter,
);

/// The database trait were we find the incremental components, and
/// functions to interact with the database.
///
/// It's implemented by the [`crate::db::LocalDb`] struct.
pub trait ZureDb: salsa::DbWithJar<Jar> + ModuleLoader {}

impl<Db: salsa::DbWithJar<Jar>> ZureDb for Db where Db: ModuleLoader {}

/// Re-export `salsa` from `salsa-2022`
extern crate salsa_2022 as salsa;

// SECTION: Module re-exports
pub mod db;
pub mod parser;
pub mod solver;
pub mod src;
pub mod vfs;
pub mod ast;
